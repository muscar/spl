use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, Write},
    iter::Peekable,
    str::Chars,
};

// AST

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Ident(String);

#[derive(Debug)]
struct TypeName(String);

#[derive(Debug)]
struct Block {
    declarations: Vec<(TypeName, Vec<Ident>)>,
    body: Vec<Stmt>,
}

#[derive(Debug)]
enum Stmt {
    Expr(Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Expr {
    IntLit(i64),
    Ident(Ident),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum BinOp {
    Plus,
    Minus,
}

// Scanner

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenKind {
    IntLit,
    Ident,
    Plus,
    Minus,
    Becomes,
    Comma,
    Period,
    Semicolon,
    Colon,
    KwInteger,
    KwString,
    KwBegin,
    KwEnd,
    Unknown,
    Eof,
}

struct Token {
    kind: TokenKind,
    text: String,
}

struct SourceReader<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> SourceReader<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn read(&mut self) -> Option<char> {
        self.chars.next()
    }
}

struct Tokens<'a> {
    reader: &'a mut SourceReader<'a>,
    keywords: HashMap<&'static str, TokenKind>,
}

impl<'a> Tokens<'a> {
    fn new(reader: &'a mut SourceReader<'a>, keywords: HashMap<&'static str, TokenKind>) -> Self {
        Self { reader, keywords }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while matches!(self.reader.peek(), Some(c) if c.is_whitespace()) {
            self.reader.read();
        }
        let mut token = Token {
            kind: TokenKind::Eof,
            text: String::new(),
        };
        token.kind = match self.reader.peek() {
            Some('+') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Plus
            }
            Some('-') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Minus
            }
            Some(',') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Comma
            }
            Some('.') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Period
            }
            Some(';') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Semicolon
            }
            Some(':') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Becomes
                } else {
                    TokenKind::Colon
                }
            }
            Some(c) if c.is_digit(10) => {
                while matches!(self.reader.peek(), Some(c) if c.is_digit(10)) {
                    token.text.push(self.reader.read().unwrap());
                }
                TokenKind::IntLit
            }
            Some(c) if c.is_alphanumeric() => {
                while matches!(self.reader.peek(), Some(c) if c.is_alphanumeric()) {
                    token.text.push(self.reader.read().unwrap());
                }
                self.keywords
                    .get(&token.text[..])
                    .cloned()
                    .unwrap_or(TokenKind::Ident)
            }
            Some(_) => {
                while matches!(self.reader.peek(), Some(c) if !c.is_whitespace()) {
                    token.text.push(self.reader.read().unwrap());
                }
                TokenKind::Unknown
            }
            None => {
                token.text = "end of file".to_string();
                TokenKind::Eof
            }
        };
        Some(token)
    }
}

struct Scanner<'a> {
    reader: SourceReader<'a>,
}

impl<'a> Scanner<'a> {
    fn new(reader: SourceReader<'a>) -> Self {
        Self { reader }
    }

    fn scan(&'a mut self) -> Tokens<'a> {
        let mut keywords = HashMap::new();
        keywords.insert("integer", TokenKind::KwInteger);
        keywords.insert("string", TokenKind::KwString);
        keywords.insert("begin", TokenKind::KwBegin);
        keywords.insert("end", TokenKind::KwEnd);
        Tokens::new(&mut self.reader, keywords)
    }
}

// Parser

struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Tokens<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> String {
        match self.tokens.next() {
            Some(tok) if tok.kind == expected => tok.text,
            Some(tok) => panic!(
                "syntactic error: expecting {:?}, got {:?} (`{}')",
                expected, tok.kind, tok.text
            ),
            _ => panic!(),
        }
    }

    fn ident(&mut self) -> Ident {
        Ident(self.expect(TokenKind::Ident))
    }

    fn ident_list(&mut self) -> Vec<Ident> {
        let mut idents = vec![self.ident()];
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Comma) {
            self.tokens.next();
            idents.push(self.ident());
        }
        idents
    }

    fn parse(&mut self) -> Block {
        self.program()
    }

    fn program(&mut self) -> Block {
        let expr = self.block();
        self.expect(TokenKind::Period);
        expr
    }

    fn block(&mut self) -> Block {
        let mut declarations = vec![];
        self.expect(TokenKind::KwBegin);
        while matches!(
            self.tokens.peek(),
            Some(tok) if tok.kind == TokenKind::KwInteger || tok.kind == TokenKind::KwString
        ) {
            let ty_name = TypeName(self.tokens.next().unwrap().text);
            declarations.push((ty_name, self.ident_list()));
            self.expect(TokenKind::Semicolon);
        }
        let mut body = vec![self.statement()];
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Semicolon) {
            self.tokens.next();
            body.push(self.statement());
        }
        self.expect(TokenKind::KwEnd);
        Block { declarations, body }
    }

    fn statement(&mut self) -> Stmt {
        let lhs = self.simple_expr();
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Becomes) {
            self.tokens.next();
            let rhs = self.simple_expr();
            return Stmt::Assign(Box::new(lhs), Box::new(rhs));
        }
        Stmt::Expr(Box::new(lhs))
    }

    fn simple_expr(&mut self) -> Expr {
        let mut lhs = self.factor();
        while matches!(
            self.tokens.peek(),
            Some(tok) if tok.kind == TokenKind::Plus || tok.kind == TokenKind::Minus
        ) {
            let op = match self.tokens.next() {
                Some(tok) if tok.kind == TokenKind::Plus => BinOp::Plus,
                Some(tok) if tok.kind == TokenKind::Minus => BinOp::Minus,
                _ => panic!(),
            };
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(self.factor()));
        }
        lhs
    }

    fn term(&mut self) -> Expr {
        self.factor()
    }

    fn factor(&mut self) -> Expr {
        let tok = self.tokens.next().expect("expecting factor");
        match tok.kind {
            TokenKind::IntLit => Expr::IntLit(tok.text.parse().expect("malformed integer literal")),
            TokenKind::Ident => Expr::Ident(Ident(tok.text)),
            _ => panic!("sytactic error: expecting factor"),
        }
    }
}

// Semantic

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    Integer,
    String,
}

fn resolve_type(name: &str) -> Type {
    match name {
        "integer" => Type::Integer,
        "string" => Type::String,
        _ => panic!("type error: unknown type {}", name),
    }
}

fn type_check_block(ty_env: &mut HashMap<Ident, Type>, block: &Block) {
    for (ty_name, ids) in &block.declarations {
        let ty = resolve_type(&ty_name.0);
        for id in ids {
            ty_env.insert(id.clone(), ty);
        }
    }
    for stmt in &block.body {
        type_check_stmt(ty_env, stmt);
    }
}

fn type_check_stmt(ty_env: &HashMap<Ident, Type>, stmt: &Stmt) {
    match stmt {
        Stmt::Expr(e) => {
            type_of_expr(ty_env, e);
        }
        Stmt::Assign(lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            if rty != lty {
                panic!("type error: trying to assign {:?} to {:?}", rty, lty);
            }
        }
    };
}

fn type_of_expr(ty_env: &HashMap<Ident, Type>, expr: &Expr) -> Type {
    match expr {
        Expr::IntLit(_) => Type::Integer,
        Expr::Ident(id) => ty_env
            .get(id)
            .copied()
            .unwrap_or_else(|| panic!("unbound identifier `{}'", id.0)),
        Expr::BinOp(op, lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            match (lty, rty) {
                (Type::Integer, Type::Integer) => Type::Integer,
                _ => panic!("type error: operator {:?} must have integer operands", op),
            }
        }
    }
}

// Compiler + VM

#[derive(Debug, Clone, Copy)]
struct LocalIdx(usize);

enum OpCode {
    LdcI8(i64),
    LdLoc(LocalIdx),
    StLoc(LocalIdx),
    Add,
    Sub,
    Ret,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::LdcI8(n) => f.write_fmt(format_args!("ldc.i8 {}", n)),
            OpCode::LdLoc(idx) => f.write_fmt(format_args!("ldloc {}", idx.0)),
            OpCode::StLoc(idx) => f.write_fmt(format_args!("stloc {}", idx.0)),
            OpCode::Add => f.write_str("add"),
            OpCode::Sub => f.write_str("sub"),
            OpCode::Ret => f.write_str("ret"),
        }
    }
}

struct CodeBlock {
    code: Vec<OpCode>,
    next_local_idx: usize,
}

impl CodeBlock {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            next_local_idx: 0,
        }
    }

    fn add_local(&mut self) -> LocalIdx {
        let idx = LocalIdx(self.next_local_idx);
        self.next_local_idx += 1;
        idx
    }

    fn emit(&mut self, op: OpCode) {
        self.code.push(op);
    }
}

struct ActivationRecord {
    locals: Vec<i64>,
}

impl ActivationRecord {
    fn new(locals_count: usize) -> Self {
        Self {
            locals: vec![0; locals_count],
        }
    }
}

struct Vm {
    stack: Vec<i64>,
    call_stack: Vec<ActivationRecord>,
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    fn run(&mut self, gen: &CodeBlock) -> i64 {
        self.call_stack
            .push(ActivationRecord::new(gen.next_local_idx));
        for op in &gen.code {
            match op {
                OpCode::LdcI8(n) => self.stack.push(*n),
                OpCode::LdLoc(idx) => self
                    .stack
                    .push(self.call_stack.last().unwrap().locals[idx.0]),
                OpCode::StLoc(idx) => {
                    let arec = self.call_stack.last_mut().unwrap();
                    arec.locals[idx.0] = self.stack.pop().expect("stack underflow");
                }
                OpCode::Add => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m + n);
                }
                OpCode::Sub => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m - n);
                }
                OpCode::Ret => break,
            }
        }
        self.call_stack.pop();
        self.stack.pop().expect("stack underflow")
    }
}

#[derive(Debug, Clone, Copy)]
enum Item {
    Imm(i64),
    Stack,
    Local(LocalIdx),
}

fn compile_block(block: &Block, gen: &mut CodeBlock) {
    let mut names = HashMap::new();
    for (_, ids) in &block.declarations {
        for id in ids {
            let idx = gen.add_local();
            names.insert(id.clone(), idx);
            // gen.emit(OpCode::LdcI8(0));
            // gen.emit(OpCode::StLoc(idx));
        }
    }
    for s in &block.body {
        compile_stmt(s, gen, &mut names);
    }
}

fn compile_stmt(stmt: &Stmt, gen: &mut CodeBlock, names: &mut HashMap<Ident, LocalIdx>) {
    match stmt {
        Stmt::Expr(e) => {
            let item = compile_expr(e, gen, names);
            load(gen, item);
        }
        Stmt::Assign(lhs, rhs) => match compile_expr(lhs, gen, names) {
            Item::Imm(_) | Item::Stack => panic!("trying to assign to value on stack! bug?"),
            Item::Local(idx) => {
                let item = compile_expr(rhs, gen, names);
                load(gen, item);
                gen.emit(OpCode::StLoc(idx));
            }
        },
    }
}

fn compile_expr(expr: &Expr, gen: &mut CodeBlock, names: &HashMap<Ident, LocalIdx>) -> Item {
    match expr {
        Expr::IntLit(n) => {
            // gen.emit(OpCode::LdcI8(*n));
            Item::Imm(*n)
        }
        Expr::Ident(id) => {
            let idx = names
                .get(id)
                .unwrap_or_else(|| panic!("unbound identifier: {}", id.0));
            // gen.emit(OpCode::LdLoc(*idx));
            Item::Local(*idx)
        }
        Expr::BinOp(op, lhs, rhs) => {
            let litem = compile_expr(lhs, gen, names);
            let ritem = compile_expr(rhs, gen, names);
            match (litem, ritem) {
                (Item::Imm(m), Item::Imm(n)) => match op {
                    BinOp::Plus => Item::Imm(m + n),
                    BinOp::Minus => Item::Imm(m - n),
                },
                _ => {
                    load(gen, litem);
                    load(gen, ritem);
                    match op {
                        BinOp::Plus => gen.emit(OpCode::Add),
                        BinOp::Minus => gen.emit(OpCode::Sub),
                    }
                    Item::Stack
                }
            }
        }
    }
}

fn load(gen: &mut CodeBlock, item: Item) {
    match item {
        Item::Imm(n) => gen.emit(OpCode::LdcI8(n)),
        Item::Local(idx) => gen.emit(OpCode::LdLoc(idx)),
        Item::Stack => (),
    }
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    loop {
        stdout.write_all(b"> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buf).unwrap();
        let sr = SourceReader::new(&buf);
        let mut s = Scanner::new(sr);
        let mut p = Parser::new(s.scan());
        let node = p.parse();
        println!("; {:?}", node);

        let mut ty_env = HashMap::new();
        type_check_block(&mut ty_env, &node);

        let mut gen = CodeBlock::new();
        compile_block(&node, &mut gen);
        println!("; code:");
        for op in &gen.code {
            println!(";   {}", op);
        }

        let mut vm = Vm::new();
        println!("; {:?}", vm.run(&gen));

        buf.clear();
    }
}

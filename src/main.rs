use std::{
    collections::HashMap,
    io::{self, Write},
    iter::Peekable,
    str::Chars,
};

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

#[derive(Debug, PartialEq, Eq)]
enum Token {
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
    lexeme: String,
}

impl<'a> Tokens<'a> {
    fn new(reader: &'a mut SourceReader<'a>) -> Self {
        Self {
            reader,
            lexeme: String::new(),
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = (Token, String);

    fn next(&mut self) -> Option<Self::Item> {
        while matches!(self.reader.peek(), Some(c) if c.is_whitespace()) {
            self.reader.read();
        }
        self.lexeme.clear();
        match self.reader.peek() {
            Some('+') => {
                self.lexeme.push(self.reader.read().unwrap());
                Some((Token::Plus, self.lexeme.clone()))
            }
            Some('-') => {
                self.lexeme.push(self.reader.read().unwrap());
                Some((Token::Minus, self.lexeme.clone()))
            }
            Some(',') => {
                self.lexeme.push(self.reader.read().unwrap());
                Some((Token::Comma, self.lexeme.clone()))
            }
            Some('.') => {
                self.lexeme.push(self.reader.read().unwrap());
                Some((Token::Period, self.lexeme.clone()))
            }
            Some(';') => {
                self.lexeme.push(self.reader.read().unwrap());
                Some((Token::Semicolon, self.lexeme.clone()))
            }
            Some(':') => {
                self.lexeme.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    self.lexeme.push(self.reader.read().unwrap());
                    return Some((Token::Becomes, self.lexeme.clone()));
                }
                Some((Token::Colon, self.lexeme.clone()))
            }
            Some(c) if c.is_digit(10) => {
                while matches!(self.reader.peek(), Some(c) if c.is_digit(10)) {
                    self.lexeme.push(self.reader.read().unwrap());
                }
                Some((Token::IntLit, self.lexeme.clone()))
            }
            Some(c) if c.is_alphanumeric() => {
                while matches!(self.reader.peek(), Some(c) if c.is_alphanumeric()) {
                    self.lexeme.push(self.reader.read().unwrap());
                }
                match &self.lexeme[..] {
                    "integer" => Some((Token::KwInteger, self.lexeme.clone())),
                    "string" => Some((Token::KwString, self.lexeme.clone())),
                    "begin" => Some((Token::KwBegin, self.lexeme.clone())),
                    "end" => Some((Token::KwEnd, self.lexeme.clone())),
                    _ => Some((Token::Ident, self.lexeme.clone())),
                }
            }
            Some(_) => {
                while matches!(self.reader.peek(), Some(c) if !c.is_whitespace()) {
                    self.lexeme.push(self.reader.read().unwrap());
                }
                Some((Token::Unknown, self.lexeme.clone()))
            }
            None => Some((Token::Eof, "end of file".to_string())),
        }
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
        Tokens::new(&mut self.reader)
    }
}

struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Tokens<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expect(&mut self, expected: Token) -> String {
        match self.tokens.next() {
            Some((tok, lex)) if tok == expected => lex,
            Some((tok, lex)) => panic!(
                "syntactic error: expecting {:?}, got {:?} (`{}')",
                expected, tok, lex
            ),
            _ => panic!(),
        }
    }

    fn ident(&mut self) -> Ident {
        Ident(self.expect(Token::Ident))
    }

    fn ident_list(&mut self) -> Vec<Ident> {
        let mut idents = vec![self.ident()];
        while matches!(self.tokens.peek(), Some((Token::Comma, _))) {
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
        self.expect(Token::Period);
        expr
    }

    fn block(&mut self) -> Block {
        let mut declarations = vec![];
        self.expect(Token::KwBegin);
        while matches!(
            self.tokens.peek(),
            Some((Token::KwInteger, _)) | Some((Token::KwString, _))
        ) {
            let ty_name = TypeName(self.tokens.next().unwrap().1);
            declarations.push((ty_name, self.ident_list()));
            self.expect(Token::Semicolon);
        }
        let mut body = vec![self.statement()];
        while matches!(self.tokens.peek(), Some((Token::Semicolon, _))) {
            self.tokens.next();
            body.push(self.statement());
        }
        self.expect(Token::KwEnd);
        Block { declarations, body }
    }

    fn statement(&mut self) -> Stmt {
        let lhs = self.simple_expr();
        if matches!(self.tokens.peek(), Some((Token::Becomes, _))) {
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
            Some((Token::Plus, _)) | Some((Token::Minus, _))
        ) {
            let op = match self.tokens.next() {
                Some((Token::Plus, _)) => BinOp::Plus,
                Some((Token::Minus, _)) => BinOp::Minus,
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
        let (tok, lex) = self.tokens.next().expect("expecting factor");
        match tok {
            Token::IntLit => Expr::IntLit(lex.parse().expect("malformed integer literal")),
            Token::Ident => Expr::Ident(Ident(lex.clone())),
            _ => panic!("sytactic error: expecting factor"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LocalIdx(usize);

#[derive(Debug)]
enum OpCode {
    LdcI8(i64),
    LdLoc(LocalIdx),
    StLoc(LocalIdx),
    Add,
    Sub,
    Ret,
}

struct CodeGen {
    code: Vec<OpCode>,
    next_local_idx: usize,
}

impl CodeGen {
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

    fn run(&mut self, gen: &CodeGen) -> i64 {
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

fn compile_block(block: &Block, gen: &mut CodeGen) {
    let mut names = HashMap::new();
    for (_, idx) in &block.declarations {
        for id in idx {
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

fn compile_stmt(stmt: &Stmt, gen: &mut CodeGen, names: &mut HashMap<Ident, LocalIdx>) {
    match stmt {
        Stmt::Expr(e) => compile_expr(e, gen, names),
        Stmt::Assign(lhs, rhs) => match &**lhs {
            Expr::Ident(id) => {
                let idx = names
                    .get(&id)
                    .expect(&format!("unbound identifier: {}", id.0));
                compile_expr(rhs, gen, names);
                gen.emit(OpCode::StLoc(*idx))
            }
            _ => panic!("general assignment is not implemented"),
        },
    }
}

fn compile_expr(expr: &Expr, gen: &mut CodeGen, names: &HashMap<Ident, LocalIdx>) {
    match expr {
        Expr::IntLit(n) => gen.emit(OpCode::LdcI8(*n)),
        Expr::Ident(id) => {
            let idx = names
                .get(id)
                .expect(&format!("unbound identifier: {}", id.0));
            gen.emit(OpCode::LdLoc(*idx));
        }
        Expr::BinOp(BinOp::Plus, lhs, rhs) => {
            compile_expr(lhs, gen, names);
            compile_expr(rhs, gen, names);
            gen.emit(OpCode::Add);
        }
        Expr::BinOp(BinOp::Minus, lhs, rhs) => {
            compile_expr(lhs, gen, names);
            compile_expr(rhs, gen, names);
            gen.emit(OpCode::Sub);
        }
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

        let mut gen = CodeGen::new();
        compile_block(&node, &mut gen);
        let mut vm = Vm::new();
        println!("; {:?} ; compile", vm.run(&gen));

        buf.clear();
    }
}

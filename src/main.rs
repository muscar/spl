use std::{
    collections::HashMap,
    env,
    fmt::Display,
    fs,
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
struct Program {
    body: Stmt,
}

#[derive(Debug)]
enum Stmt {
    Expr(Box<Expr>),
    Block(Vec<(TypeName, Vec<Ident>)>, Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    BeginUntil(
        Vec<(TypeName, Vec<Ident>)>,
        Vec<Ident>,
        Vec<Stmt>,
        Vec<(Ident, Vec<Stmt>)>,
    ),
    LoopWhile(Vec<Stmt>, Box<Expr>, Vec<Stmt>),
    LoopUntil(Vec<Ident>, Vec<Stmt>, Vec<(Ident, Vec<Stmt>)>),
}

#[derive(Debug)]
enum Expr {
    BoolLit(bool),
    IntLit(i64),
    Ident(Ident),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum UnOp {
    Not,
}

#[derive(Debug)]
enum BinOp {
    Plus,
    Minus,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

// Scanner

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenKind {
    IntLit,
    Ident,
    Plus,
    Minus,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Tilde,
    Becomes,
    FatArrow,
    Comma,
    Period,
    Semicolon,
    Colon,
    LPar,
    RPar,
    KwBoolean,
    KwInteger,
    KwString,
    KwBegin,
    KwEnd,
    KwTrue,
    KwIf,
    KwThen,
    KwElse,
    KwFi,
    KwLoop,
    KwWhile,
    KwRepeat,
    KwUntil,
    KwOr,
    KwFalse,
    Unknown,
    Eof,
}

#[derive(Debug)]
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
            Some('=') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('>') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }
            Some('≠') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Neq
            }
            Some('¬') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Neq
                } else {
                    TokenKind::Tilde
                }
            }
            Some('<') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Lte
                } else {
                    TokenKind::Lt
                }
            }
            Some('≤') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Lte
            }
            Some('>') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Gte
                } else {
                    TokenKind::Gt
                }
            }
            Some('≥') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Gte
            }
            Some('~') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Tilde
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
            Some('(') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::LPar
            }
            Some(')') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::RPar
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
        keywords.insert("boolean", TokenKind::KwBoolean);
        keywords.insert("integer", TokenKind::KwInteger);
        keywords.insert("string", TokenKind::KwString);
        keywords.insert("begin", TokenKind::KwBegin);
        keywords.insert("end", TokenKind::KwEnd);
        keywords.insert("if", TokenKind::KwIf);
        keywords.insert("then", TokenKind::KwThen);
        keywords.insert("else", TokenKind::KwElse);
        keywords.insert("fi", TokenKind::KwFi);
        keywords.insert("loop", TokenKind::KwLoop);
        keywords.insert("while", TokenKind::KwWhile);
        keywords.insert("repeat", TokenKind::KwRepeat);
        keywords.insert("until", TokenKind::KwUntil);
        keywords.insert("or", TokenKind::KwOr);
        keywords.insert("true", TokenKind::KwTrue);
        keywords.insert("false", TokenKind::KwFalse);
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

    fn is_add_op(tok: &Token) -> bool {
        tok.kind == TokenKind::KwBoolean
            || tok.kind == TokenKind::Plus
            || tok.kind == TokenKind::Minus
    }

    fn is_rel_op(tok: &Token) -> bool {
        tok.kind == TokenKind::KwBoolean
            || tok.kind == TokenKind::Eq
            || tok.kind == TokenKind::Neq
            || tok.kind == TokenKind::Lt
            || tok.kind == TokenKind::Lte
            || tok.kind == TokenKind::Gt
            || tok.kind == TokenKind::Gte
    }

    fn is_builtin_type_name(tok: &Token) -> bool {
        tok.kind == TokenKind::KwBoolean
            || tok.kind == TokenKind::KwInteger
            || tok.kind == TokenKind::KwString
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

    // ident_list ::= Ident {',' Ident}
    fn ident_list(&mut self, sep: TokenKind) -> Vec<Ident> {
        let mut idents = vec![self.ident()];
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == sep) {
            self.tokens.next();
            idents.push(self.ident());
        }
        idents
    }

    fn parse(&mut self) -> Program {
        self.program()
    }

    fn program(&mut self) -> Program {
        let body = self.statement();
        self.expect(TokenKind::Period);
        Program { body }
    }

    fn statement_list(&mut self) -> Vec<Stmt> {
        let mut body = vec![self.statement()];
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Semicolon) {
            self.tokens.next();
            body.push(self.statement());
        }
        body
    }

    fn statement(&mut self) -> Stmt {
        match self.tokens.peek() {
            Some(tok) if tok.kind == TokenKind::KwBegin => self.block_dispatch(),
            Some(tok) if tok.kind == TokenKind::KwIf => self.if_then_else(),
            Some(tok) if tok.kind == TokenKind::KwLoop => self.loop_dispatch(),
            _ => {
                let lhs = self.expr();
                if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Becomes) {
                    return self.assignment(lhs);
                }
                Stmt::Expr(Box::new(lhs))
            }
        }
    }

    fn block_dispatch(&mut self) -> Stmt {
        self.expect(TokenKind::KwBegin);
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwUntil) {
            return self.begin_until();
        }
        self.begin()
    }

    fn begin(&mut self) -> Stmt {
        let mut declarations = vec![];
        while matches!(
            self.tokens.peek(),
            Some(tok) if Self::is_builtin_type_name(tok)
        ) {
            let ty_name = TypeName(self.tokens.next().unwrap().text);
            declarations.push((ty_name, self.ident_list(TokenKind::Comma)));
            self.expect(TokenKind::Semicolon);
        }
        let body = self.statement_list();
        self.expect(TokenKind::KwEnd);
        Stmt::Block(declarations, body)
    }

    fn begin_until(&mut self) -> Stmt {
        self.expect(TokenKind::KwUntil);
        let mut declarations = vec![];
        while matches!(
            self.tokens.peek(),
            Some(tok) if Self::is_builtin_type_name(tok)
        ) {
            let ty_name = TypeName(self.tokens.next().unwrap().text);
            declarations.push((ty_name, self.ident_list(TokenKind::Comma)));
            self.expect(TokenKind::Semicolon);
        }
        let situations = self.ident_list(TokenKind::KwOr);
        let s = self.statement_list();
        self.expect(TokenKind::KwEnd);
        self.expect(TokenKind::KwThen);
        let mut handlers = vec![self.situation_handler()];
        while !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwFi) {
            handlers.push(self.situation_handler());
        }
        self.expect(TokenKind::KwFi);
        Stmt::BeginUntil(declarations, situations, s, handlers)
    }

    fn loop_dispatch(&mut self) -> Stmt {
        self.expect(TokenKind::KwLoop);
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwUntil) {
            return self.loop_until_repeat();
        }
        self.loop_while_repeat()
    }

    fn loop_until_repeat(&mut self) -> Stmt {
        self.expect(TokenKind::KwUntil);
        let situations = self.ident_list(TokenKind::KwOr);
        let s = self.statement_list();
        self.expect(TokenKind::KwRepeat);
        self.expect(TokenKind::KwThen);
        let mut handlers = vec![self.situation_handler()];
        while !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwFi) {
            handlers.push(self.situation_handler());
        }
        self.expect(TokenKind::KwFi);
        Stmt::LoopUntil(situations, s, handlers)
    }

    fn situation_handler(&mut self) -> (Ident, Vec<Stmt>) {
        let situation = self.ident();
        self.expect(TokenKind::FatArrow);
        let s = self.statement_list();
        (situation, s)
    }

    fn loop_while_repeat(&mut self) -> Stmt {
        let mut s = Vec::new();
        if !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwWhile) {
            s = self.statement_list();
        }
        self.expect(TokenKind::KwWhile);
        let test = self.expr();
        let mut t = Vec::new();
        if !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwRepeat) {
            t = self.statement_list();
        }
        self.expect(TokenKind::KwRepeat);
        Stmt::LoopWhile(s, Box::new(test), t)
    }

    fn if_then_else(&mut self) -> Stmt {
        self.expect(TokenKind::KwIf);
        let test = self.expr();
        self.expect(TokenKind::KwThen);
        let cons = self.statement_list();
        let mut alt = Vec::new();
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwElse) {
            self.tokens.next();
            alt = self.statement_list();
        }
        self.expect(TokenKind::KwFi);
        Stmt::IfThenElse(Box::new(test), cons, alt)
    }

    fn assignment(&mut self, lhs: Expr) -> Stmt {
        self.expect(TokenKind::Becomes);
        let rhs = self.expr();
        Stmt::Assign(Box::new(lhs), Box::new(rhs))
    }

    fn expr(&mut self) -> Expr {
        let mut lhs = self.simple_expr();
        while matches!(
            self.tokens.peek(),
            Some(tok) if Self::is_rel_op(tok)
        ) {
            let op = match self.tokens.next() {
                Some(Token {
                    kind: TokenKind::Eq,
                    ..
                }) => BinOp::Eq,
                Some(Token {
                    kind: TokenKind::Neq,
                    ..
                }) => BinOp::Neq,
                Some(Token {
                    kind: TokenKind::Lt,
                    ..
                }) => BinOp::Lt,
                Some(Token {
                    kind: TokenKind::Lte,
                    ..
                }) => BinOp::Lte,
                Some(Token {
                    kind: TokenKind::Gt,
                    ..
                }) => BinOp::Gt,
                Some(Token {
                    kind: TokenKind::Gte,
                    ..
                }) => BinOp::Gte,
                _ => panic!(),
            };
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(self.simple_expr()));
        }
        lhs
    }

    fn simple_expr(&mut self) -> Expr {
        let mut lhs = self.term();
        while matches!(
            self.tokens.peek(),
            Some(tok) if Self::is_add_op(tok)
        ) {
            let op = match self.tokens.next() {
                Some(Token {
                    kind: TokenKind::Plus,
                    ..
                }) => BinOp::Plus,
                Some(Token {
                    kind: TokenKind::Minus,
                    ..
                }) => BinOp::Minus,
                _ => panic!(),
            };
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(self.term()));
        }
        lhs
    }

    fn term(&mut self) -> Expr {
        self.factor()
    }

    fn factor(&mut self) -> Expr {
        let tok = self.tokens.next().expect("expecting factor");
        match tok.kind {
            TokenKind::KwTrue => Expr::BoolLit(true),
            TokenKind::KwFalse => Expr::BoolLit(false),
            TokenKind::IntLit => Expr::IntLit(tok.text.parse().expect("malformed integer literal")),
            TokenKind::Ident => Expr::Ident(Ident(tok.text)),
            TokenKind::Tilde => Expr::UnOp(UnOp::Not, Box::new(self.expr())),
            TokenKind::LPar => {
                let expr = self.expr();
                self.expect(TokenKind::RPar);
                expr
            }
            tok => panic!("syntactic error: expecting factor (context={:?})", tok),
        }
    }
}

// Semantic

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    Boolean,
    Integer,
    String,
}

fn resolve_type(name: &str) -> Type {
    match name {
        "boolean" => Type::Boolean,
        "integer" => Type::Integer,
        "string" => Type::String,
        _ => panic!("type error: unknown type {}", name),
    }
}

fn type_check(ty_env: &mut HashMap<Ident, Type>, prog: &Program) {
    type_check_stmt(ty_env, &prog.body);
}

fn type_check_stmt(ty_env: &mut HashMap<Ident, Type>, stmt: &Stmt) {
    match stmt {
        Stmt::Expr(e) => {
            type_of_expr(ty_env, e);
        }
        Stmt::Block(declarations, body) => {
            for (ty_name, ids) in declarations {
                let ty = resolve_type(&ty_name.0);
                for id in ids {
                    ty_env.insert(id.clone(), ty);
                }
            }
            for stmt in body {
                type_check_stmt(ty_env, stmt);
            }
        }
        Stmt::BeginUntil(declarations, situations, s, handlers) => {
            for (ty_name, ids) in declarations {
                let ty = resolve_type(&ty_name.0);
                for id in ids {
                    ty_env.insert(id.clone(), ty);
                }
            }
            for id in situations {
                ty_env.insert(id.clone(), Type::Integer);
            }
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
            for (id, s) in handlers {
                // TODO: this is inefficient
                if !situations.contains(id) {
                    panic!("type error: situation in handler is not present in loop until declaration: {}", id.0)
                }
                for stmt in s {
                    type_check_stmt(ty_env, stmt);
                }
            }
        }
        Stmt::Assign(lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            if rty != lty {
                panic!("type error: trying to assign {:?} to {:?}", rty, lty);
            }
        }
        Stmt::IfThenElse(test, s, t) | Stmt::LoopWhile(s, test, t) => {
            let tty = type_of_expr(ty_env, test);
            if tty != Type::Boolean {
                panic!("type error: condition must be a boolean value");
            }
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
            for stmt in t {
                type_check_stmt(ty_env, stmt);
            }
        }
        Stmt::LoopUntil(situations, s, handlers) => {
            for id in situations {
                ty_env.insert(id.clone(), Type::Integer);
            }
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
            for (id, s) in handlers {
                // TODO: this is inefficient
                if !situations.contains(id) {
                    panic!("type error: situation in handler is not present in loop until declaration: {}", id.0)
                }
                for stmt in s {
                    type_check_stmt(ty_env, stmt);
                }
            }
        }
    };
}

fn type_of_expr(ty_env: &HashMap<Ident, Type>, expr: &Expr) -> Type {
    match expr {
        &Expr::BoolLit(_) => Type::Boolean,
        Expr::IntLit(_) => Type::Integer,
        Expr::Ident(id) => ty_env
            .get(id)
            .copied()
            .unwrap_or_else(|| panic!("unbound identifier `{}'", id.0)),
        Expr::UnOp(op, arg) => {
            let aty = type_of_expr(ty_env, arg);
            match op {
                UnOp::Not => {
                    if aty != Type::Boolean {
                        panic!("type error: operator {:?} must have a boolean operand", op)
                    }
                    Type::Boolean
                }
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            match op {
                BinOp::Plus | BinOp::Minus => {
                    if lty != rty || lty != Type::Integer {
                        panic!("type error: operator {:?} must have integer operands", op);
                    }
                    Type::Integer
                }
                BinOp::Eq | BinOp::Neq => {
                    if lty != rty {
                        panic!("type error: operator {:?} must have same type operands", op);
                    }
                    Type::Boolean
                }
                BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => {
                    if lty != rty || lty != Type::Integer {
                        panic!("type error: operator {:?} must have integer operands", op);
                    }
                    Type::Boolean
                }
            }
        }
    }
}

// Compiler + VM

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LocalIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LabelIdx(usize);

#[derive(Clone, Copy)]
enum OpCode {
    LdcI8(i64),
    LdLoc(LocalIdx),
    StLoc(LocalIdx),
    Not,
    Add,
    Sub,
    Eq,
    Lt,
    Gt,
    Ret,
    Br(LabelIdx),
    BrFalse(LabelIdx),
    Nop,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::LdcI8(n) => f.write_fmt(format_args!("ldc.i8 {}", n)),
            OpCode::LdLoc(idx) => f.write_fmt(format_args!("ldloc {}", idx.0)),
            OpCode::StLoc(idx) => f.write_fmt(format_args!("stloc {}", idx.0)),
            OpCode::Not => f.write_str("not"),
            OpCode::Add => f.write_str("add"),
            OpCode::Sub => f.write_str("sub"),
            OpCode::Eq => f.write_str("eq"),
            OpCode::Lt => f.write_str("lt"),
            OpCode::Gt => f.write_str("gt"),
            OpCode::Br(lab) => f.write_fmt(format_args!("br {}", lab.0)),
            OpCode::BrFalse(lab) => f.write_fmt(format_args!("br.false {}", lab.0)),
            OpCode::Ret => f.write_str("ret"),
            OpCode::Nop => f.write_str("nop"),
        }
    }
}

struct CodeBlock {
    code: Vec<OpCode>,
    labels: HashMap<LabelIdx, usize>,
    next_local_idx: usize,
    next_label_idx: usize,
}

impl CodeBlock {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            labels: HashMap::new(),
            next_local_idx: 0,
            next_label_idx: 0,
        }
    }

    fn add_local(&mut self) -> LocalIdx {
        let idx = LocalIdx(self.next_local_idx);
        self.next_local_idx += 1;
        idx
    }

    fn new_label(&mut self) -> LabelIdx {
        let idx = LabelIdx(self.next_label_idx);
        self.next_label_idx += 1;
        idx
    }

    fn mark_label(&mut self, label: LabelIdx) {
        self.labels.insert(label, self.code.len());
    }

    fn emit(&mut self, op: OpCode) {
        self.code.push(op);
    }

    fn backpatch(&mut self) {
        self.code = self
            .code
            .iter()
            .map(|op| match op {
                OpCode::BrFalse(lab) => {
                    OpCode::BrFalse(LabelIdx(*self.labels.get(lab).expect("unknown label")))
                }
                OpCode::Br(lab) => {
                    OpCode::Br(LabelIdx(*self.labels.get(lab).expect("unknown label")))
                }
                op => *op,
            })
            .collect();
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
        let mut ip = 0;
        while ip < gen.code.len() {
            match gen.code[ip] {
                OpCode::LdcI8(n) => {
                    self.stack.push(n);
                    ip += 1;
                }
                OpCode::LdLoc(idx) => {
                    self.stack
                        .push(self.call_stack.last().unwrap().locals[idx.0]);
                    ip += 1;
                }
                OpCode::StLoc(idx) => {
                    let arec = self.call_stack.last_mut().unwrap();
                    arec.locals[idx.0] = self.stack.pop().expect("stack underflow");
                    ip += 1;
                }
                OpCode::Not => {
                    let x = self.stack.pop().expect("stack underflow");
                    self.stack.push(if x == 0 { 1 } else { 0 });
                    ip += 1;
                }
                OpCode::Add => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m + n);
                    ip += 1;
                }
                OpCode::Sub => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m - n);
                    ip += 1;
                }
                OpCode::Eq => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(if m == n { 1 } else { 0 });
                    ip += 1;
                }
                OpCode::Lt => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(if m < n { 1 } else { 0 });
                    ip += 1;
                }
                OpCode::Gt => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(if m > n { 1 } else { 0 });
                    ip += 1;
                }
                OpCode::Br(loc) => ip = loc.0,
                OpCode::BrFalse(loc) => {
                    let x = self.stack.pop().expect("stack underflow");
                    if x == 0 {
                        ip = loc.0;
                    } else {
                        ip += 1;
                    }
                }
                OpCode::Nop => ip += 1,
                OpCode::Ret => break,
            }
        }
        self.call_stack.pop();
        self.stack.pop().expect("stack underflow")
    }
}

#[derive(PartialEq, Eq, Hash)]
enum Symbol {
    Local(LocalIdx),
    Situation(LabelIdx),
}

#[derive(Clone, Copy)]
enum Item {
    Imm(i64),
    Stack,
    Local(LocalIdx),
}

fn compile(prog: &Program, gen: &mut CodeBlock) {
    let mut names = HashMap::new();
    compile_stmt(&prog.body, gen, &mut names);
}

fn compile_stmt(stmt: &Stmt, gen: &mut CodeBlock, names: &mut HashMap<Ident, Symbol>) {
    match stmt {
        Stmt::Expr(e) => {
            let item = compile_expr(e, gen, names);
            load(gen, item);
        }
        Stmt::Block(declarations, body) => {
            for (_, ids) in declarations {
                for id in ids {
                    let idx = gen.add_local();
                    names.insert(id.clone(), Symbol::Local(idx));
                }
            }
            for s in body {
                compile_stmt(s, gen, names);
            }
        }
        Stmt::BeginUntil(declarations, situations, s, handlers) => {
            for (_, ids) in declarations {
                for id in ids {
                    let idx = gen.add_local();
                    names.insert(id.clone(), Symbol::Local(idx));
                }
            }
            let end_lab = gen.new_label();
            let mut situation_labs = HashMap::new();
            for id in situations {
                let lab = gen.new_label();
                names.insert(id.clone(), Symbol::Situation(lab));
                situation_labs.insert(id.clone(), lab);
            }
            for stmt in s {
                compile_stmt(stmt, gen, names);
            }
            for (id, s) in handlers {
                gen.mark_label(
                    *situation_labs
                        .get(id)
                        .expect("unknown situation label. bug?"),
                );
                for stmt in s {
                    compile_stmt(stmt, gen, names);
                }
                gen.emit(OpCode::Br(end_lab));
            }
            gen.mark_label(end_lab);
        }
        Stmt::Assign(lhs, rhs) => match compile_expr(lhs, gen, names) {
            Item::Imm(_) | Item::Stack => panic!("trying to assign to value on stack! bug?"),
            Item::Local(idx) => {
                let item = compile_expr(rhs, gen, names);
                load(gen, item);
                gen.emit(OpCode::StLoc(idx));
            }
        },
        Stmt::IfThenElse(test, cons, alt) => {
            let else_lab = gen.new_label();
            let end_lab = gen.new_label();
            let titem = compile_expr(test, gen, names);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(else_lab));
            for stmt in cons {
                compile_stmt(stmt, gen, names);
            }
            gen.emit(OpCode::Br(end_lab));
            gen.mark_label(else_lab);
            for stmt in alt {
                compile_stmt(stmt, gen, names);
            }
            gen.mark_label(end_lab);
        }
        Stmt::LoopWhile(s, test, t) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            gen.mark_label(loop_lab);
            for stmt in s {
                compile_stmt(stmt, gen, names);
            }
            let titem = compile_expr(test, gen, names);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(end_lab));
            for stmt in t {
                compile_stmt(stmt, gen, names);
            }
            gen.emit(OpCode::Br(loop_lab));
            gen.mark_label(end_lab);
        }
        Stmt::LoopUntil(situations, s, handlers) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            let mut situation_labs = HashMap::new();
            for id in situations {
                let lab = gen.new_label();
                names.insert(id.clone(), Symbol::Situation(lab));
                situation_labs.insert(id.clone(), lab);
            }
            gen.mark_label(loop_lab);
            for stmt in s {
                compile_stmt(stmt, gen, names);
            }
            gen.emit(OpCode::Br(loop_lab));
            for (id, s) in handlers {
                gen.mark_label(
                    *situation_labs
                        .get(id)
                        .expect("unknown situation label. bug?"),
                );
                for stmt in s {
                    compile_stmt(stmt, gen, names);
                }
                gen.emit(OpCode::Br(end_lab));
            }
            gen.mark_label(end_lab);
        }
    }
}

fn compile_expr(expr: &Expr, gen: &mut CodeBlock, names: &HashMap<Ident, Symbol>) -> Item {
    match expr {
        Expr::BoolLit(b) => Item::Imm(if *b { 1 } else { 0 }),
        Expr::IntLit(n) => Item::Imm(*n),
        Expr::Ident(id) => {
            let sym = names
                .get(id)
                .unwrap_or_else(|| panic!("unbound identifier: {}", id.0));
            match sym {
                Symbol::Local(idx) => Item::Local(*idx),
                Symbol::Situation(lab) => {
                    gen.emit(OpCode::Br(*lab));
                    // TODO: Is this right?
                    Item::Stack
                }
            }
        }
        Expr::UnOp(op, arg) => {
            let aitem = compile_expr(arg, gen, names);
            match aitem {
                Item::Imm(n) => match op {
                    UnOp::Not => Item::Imm(if n == 0 { 1 } else { 0 }),
                },
                _ => {
                    load(gen, aitem);
                    match op {
                        UnOp::Not => gen.emit(OpCode::Not),
                    }
                    Item::Stack
                }
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let litem = compile_expr(lhs, gen, names);
            let ritem = compile_expr(rhs, gen, names);
            match (litem, ritem) {
                (Item::Imm(m), Item::Imm(n)) => match op {
                    BinOp::Plus => Item::Imm(m + n),
                    BinOp::Minus => Item::Imm(m - n),
                    BinOp::Eq => Item::Imm(if m == n { 1 } else { 0 }),
                    BinOp::Neq => Item::Imm(if m != n { 1 } else { 0 }),
                    BinOp::Lt => Item::Imm(if m < n { 1 } else { 0 }),
                    BinOp::Lte => Item::Imm(if m <= n { 1 } else { 0 }),
                    BinOp::Gt => Item::Imm(if m > n { 1 } else { 0 }),
                    BinOp::Gte => Item::Imm(if m >= n { 1 } else { 0 }),
                },
                _ => {
                    load(gen, litem);
                    load(gen, ritem);
                    match op {
                        BinOp::Plus => gen.emit(OpCode::Add),
                        BinOp::Minus => gen.emit(OpCode::Sub),
                        BinOp::Eq => gen.emit(OpCode::Eq),
                        BinOp::Neq => {
                            gen.emit(OpCode::Eq);
                            gen.emit(OpCode::Not);
                        }
                        BinOp::Lt => gen.emit(OpCode::Lt),
                        BinOp::Lte => {
                            gen.emit(OpCode::Gt);
                            gen.emit(OpCode::Not);
                        }
                        BinOp::Gt => gen.emit(OpCode::Gt),
                        BinOp::Gte => {
                            gen.emit(OpCode::Lt);
                            gen.emit(OpCode::Not);
                        }
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

fn compile_and_run(s: &str) {
    let sr = SourceReader::new(s);
    let mut s = Scanner::new(sr);
    let mut p = Parser::new(s.scan());
    let node = p.parse();
    println!("; {:?}", node);

    let mut ty_env = HashMap::new();
    type_check(&mut ty_env, &node);

    let mut gen = CodeBlock::new();
    compile(&node, &mut gen);
    gen.backpatch();

    println!("; code:");
    for (off, op) in gen.code.iter().enumerate() {
        println!(";   {:02}: {}", off, op);
    }

    let mut vm = Vm::new();
    println!("; {:?}", vm.run(&gen));
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    loop {
        stdout.write_all(b"> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buf).unwrap();
        compile_and_run(&buf);
        buf.clear();
    }
}

fn main() {
    let args = env::args().skip(1).collect::<Vec<String>>();
    if args.is_empty() {
        repl();
    } else {
        let s = fs::read_to_string(&args[0]).expect("could not read source file");
        compile_and_run(&s);
    }
}

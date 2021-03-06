use std::{
    collections::HashMap,
    env,
    fmt::Display,
    fs,
    hash::Hash,
    io::{self, Write},
    iter::Peekable,
    panic,
    str::Chars,
};

// AST

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Ident(String);

#[derive(Debug, Clone)]
enum TypeAnnot {
    TypeName(String),
    Array(Box<TypeAnnot>, Option<usize>),
}

#[derive(Debug)]
struct Program {
    body: Stmt,
}

#[derive(Debug)]
enum Decl {
    Var(Ident, Option<TypeAnnot>, Option<Expr>),
    Procedure(Ident, Option<TypeAnnot>, Vec<(Ident, TypeAnnot)>),
}

#[derive(Debug)]
struct Block {
    body: Vec<Stmt>,
    discard_value: bool,
}

#[derive(Debug)]
enum Stmt {
    Skip,
    Expr(Expr),
    Block(Block),
    Decls(Vec<Decl>),
    IfThenElse(Expr, Box<Stmt>, Box<Stmt>),
    BeginUntil(Vec<Ident>, Box<Stmt>, Vec<(Ident, Stmt)>),
    LoopWhile(Box<Stmt>, Expr, Box<Stmt>),
    LoopUntil(Vec<Ident>, Box<Stmt>, Vec<(Ident, Stmt)>),
    WhileElseIf(Expr, Box<Stmt>, Vec<(Expr, Stmt)>),
}

#[derive(Debug)]
enum Expr {
    BoolLit(bool),
    IntLit(i64),
    StrLit(String),
    ArrayLit(Vec<Expr>),
    Ident(Ident),
    Subscript(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
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
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    LogicAnd,
    LogicOr,
}

// Scanner

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenKind {
    IntLit,
    StrLit,
    Ident,
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Ampersand,
    Tilde,
    Becomes,
    FatArrow,
    Comma,
    Period,
    Semicolon,
    Colon,
    LPar,
    RPar,
    LBrack,
    RBrack,
    KwTrue,
    KwFalse,
    KwBoolean,
    KwInteger,
    KwString,
    KwOr,
    KwAnd,
    KwArray,
    KwLet,
    KwProcedure,
    KwBegin,
    KwEnd,
    KwIf,
    KwThen,
    KwElse,
    KwFi,
    KwLoop,
    KwWhile,
    KwRepeat,
    KwUntil,
    KwSkip,
    KwWriteStr,
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
            Some('*') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Star
            }
            Some('/') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Neq
                } else {
                    TokenKind::Slash
                }
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
            Some('???') | Some('#') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Neq
            }
            Some('??') => {
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
            Some('???') => {
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
            Some('???') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Gte
            }
            Some('&') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::Ampersand
            }
            Some('~') => {
                token.text.push(self.reader.read().unwrap());
                if self.reader.peek() == Some('=') {
                    token.text.push(self.reader.read().unwrap());
                    TokenKind::Neq
                } else {
                    TokenKind::Tilde
                }
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
            Some('(') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::LPar
            }
            Some(')') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::RPar
            }
            Some('[') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::LBrack
            }
            Some(']') => {
                token.text.push(self.reader.read().unwrap());
                TokenKind::RBrack
            }
            Some('"') => {
                self.reader.read().unwrap();
                while matches!(self.reader.peek(), Some(c) if c != '"') {
                    token.text.push(self.reader.read().unwrap());
                }
                if !matches!(self.reader.peek(), Some(c) if c == '"') {
                    panic!("lexical error: missing closing double quote for string literal");
                }
                TokenKind::StrLit
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
        keywords.insert("let", TokenKind::KwLet);
        keywords.insert("array", TokenKind::KwArray);
        keywords.insert("procedure", TokenKind::KwProcedure);
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
        keywords.insert("skip", TokenKind::KwSkip);
        keywords.insert("and", TokenKind::KwAnd);
        keywords.insert("or", TokenKind::KwOr);
        keywords.insert("true", TokenKind::KwTrue);
        keywords.insert("false", TokenKind::KwFalse);
        keywords.insert("writestr", TokenKind::KwWriteStr);
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
            || tok.kind == TokenKind::KwOr
    }

    fn is_mul_op(tok: &Token) -> bool {
        tok.kind == TokenKind::KwBoolean
            || tok.kind == TokenKind::Star
            || tok.kind == TokenKind::Slash
            || tok.kind == TokenKind::KwAnd
            || tok.kind == TokenKind::Ampersand
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

    fn type_annot(&mut self) -> TypeAnnot {
        let ty_name = TypeAnnot::TypeName(self.tokens.next().unwrap().text);
        match self.tokens.peek() {
            Some(Token {
                kind: TokenKind::KwArray,
                ..
            }) => {
                self.tokens.next();
                TypeAnnot::Array(Box::new(ty_name), None)
            }
            _ => ty_name,
        }
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

    fn statement_list(&mut self, end_toks: Vec<TokenKind>) -> Stmt {
        let mut body = vec![];
        let mut discard_value = true;
        // TODO: contains() is slow
        while matches!(self.tokens.peek(), Some(tok) if !end_toks.contains(&tok.kind)) {
            body.push(self.statement());
            if !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Semicolon) {
                discard_value = false;
                break;
            }
            self.tokens.next();
        }
        Stmt::Block(Block {
            body,
            discard_value,
        })
    }

    fn statement(&mut self) -> Stmt {
        match self.tokens.peek() {
            Some(tok) if tok.kind == TokenKind::KwSkip => {
                self.tokens.next();
                Stmt::Skip
            }
            Some(tok) if Self::is_builtin_type_name(tok) || tok.kind == TokenKind::KwLet => {
                self.var_decls()
            }
            Some(tok) if tok.kind == TokenKind::KwBegin => self.block_dispatch(),
            Some(tok) if tok.kind == TokenKind::KwIf => self.if_then_else(),
            Some(tok) if tok.kind == TokenKind::KwLoop => self.loop_dispatch(),
            Some(tok) if tok.kind == TokenKind::KwWhile => self.while_else_if(),
            _ => Stmt::Expr(self.expr()),
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
        let body = self.statement_list(vec![TokenKind::KwEnd]);
        self.expect(TokenKind::KwEnd);
        body
    }

    fn begin_until(&mut self) -> Stmt {
        self.expect(TokenKind::KwUntil);
        let situations = self.ident_list(TokenKind::KwOr);
        let s = self.statement_list(vec![TokenKind::KwEnd]);
        self.expect(TokenKind::KwEnd);
        self.expect(TokenKind::KwThen);
        let mut handlers = vec![self.situation_handler()];
        while !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwFi) {
            handlers.push(self.situation_handler());
        }
        self.expect(TokenKind::KwFi);
        Stmt::BeginUntil(situations, Box::new(s), handlers)
    }

    fn var_decls(&mut self) -> Stmt {
        let mut decls = Vec::new();
        let mut ty_annot = if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwLet)
        {
            self.tokens.next();
            None
        } else {
            Some(self.type_annot())
        };
        let name = self.ident();
        if let Some(TypeAnnot::Array(base_type, _)) = ty_annot {
            self.expect(TokenKind::LBrack);
            let dim = match self.tokens.next() {
                Some(Token {
                    kind: TokenKind::IntLit,
                    text,
                }) => Some(
                    text.parse()
                        .expect("array dimensions must be integer literals"),
                ),
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => None,
                _ => panic!("array dimension must be integral constant or `*'"),
            };
            ty_annot = Some(TypeAnnot::Array(base_type, dim));
            self.expect(TokenKind::RBrack);
        }
        let init_expr = if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Becomes)
        {
            self.tokens.next();
            Some(self.expr())
        } else {
            None
        };
        decls.push(Decl::Var(name, ty_annot.clone(), init_expr));
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Comma) {
            self.tokens.next();
            let name = self.ident();
            let init_expr = if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Becomes)
            {
                self.tokens.next();
                Some(self.expr())
            } else {
                None
            };
            decls.push(Decl::Var(name, ty_annot.clone(), init_expr));
        }
        Stmt::Decls(decls)
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
        let s = self.statement_list(vec![TokenKind::KwRepeat]);
        self.expect(TokenKind::KwRepeat);
        self.expect(TokenKind::KwThen);
        let mut handlers = vec![self.situation_handler()];
        while !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwFi) {
            handlers.push(self.situation_handler());
        }
        self.expect(TokenKind::KwFi);
        Stmt::LoopUntil(situations, Box::new(s), handlers)
    }

    fn situation_handler(&mut self) -> (Ident, Stmt) {
        let situation = self.ident();
        self.expect(TokenKind::FatArrow);
        let s = self.statement();
        (situation, s)
    }

    fn loop_while_repeat(&mut self) -> Stmt {
        let s = self.statement_list(vec![TokenKind::KwWhile]);
        self.expect(TokenKind::KwWhile);
        let test = self.expr();
        let t = self.statement_list(vec![TokenKind::KwRepeat]);
        self.expect(TokenKind::KwRepeat);
        Stmt::LoopWhile(Box::new(s), test, Box::new(t))
    }

    fn while_else_if(&mut self) -> Stmt {
        self.expect(TokenKind::KwWhile);
        let test = self.expr();
        let s = self.statement_list(vec![TokenKind::KwElse, TokenKind::KwFi]);
        let mut alts = Vec::new();
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwElse) {
            self.expect(TokenKind::KwElse);
            self.expect(TokenKind::KwIf);
            let test = self.expr();
            self.expect(TokenKind::KwThen);
            let t = self.statement_list(vec![TokenKind::KwElse, TokenKind::KwFi]);
            alts.push((test, t));
        }
        self.expect(TokenKind::KwFi);
        Stmt::WhileElseIf(test, Box::new(s), alts)
    }

    fn if_then_else(&mut self) -> Stmt {
        self.expect(TokenKind::KwIf);
        let test = self.expr();
        self.expect(TokenKind::KwThen);
        let cons = self.statement_list(vec![TokenKind::KwElse, TokenKind::KwFi]);
        let mut alt = Stmt::Block(Block {
            body: vec![],
            discard_value: false,
        });
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwElse) {
            self.tokens.next();
            alt = self.statement_list(vec![TokenKind::KwFi]);
        }
        self.expect(TokenKind::KwFi);
        Stmt::IfThenElse(test, Box::new(cons), Box::new(alt))
    }

    fn expr(&mut self) -> Expr {
        let lhs = self.rel_expr();
        if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Becomes) {
            self.tokens.next();
            let rhs = self.expr();
            return Expr::Assign(Box::new(lhs), Box::new(rhs));
        }
        lhs
    }

    fn rel_expr(&mut self) -> Expr {
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
                Some(Token {
                    kind: TokenKind::KwOr,
                    ..
                }) => BinOp::LogicOr,
                _ => panic!(),
            };
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(self.term()));
        }
        lhs
    }

    fn term(&mut self) -> Expr {
        let mut lhs = self.factor();
        while matches!(
            self.tokens.peek(),
            Some(tok) if Self::is_mul_op(tok)
        ) {
            let op = match self.tokens.next() {
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => BinOp::Mul,
                Some(Token {
                    kind: TokenKind::Slash,
                    ..
                }) => BinOp::Div,
                Some(Token {
                    kind: TokenKind::KwAnd,
                    ..
                }) => BinOp::LogicAnd,
                Some(Token {
                    kind: TokenKind::Ampersand,
                    ..
                }) => BinOp::LogicAnd,
                _ => panic!(),
            };
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(self.factor()));
        }
        lhs
    }

    fn factor(&mut self) -> Expr {
        let tok = self.tokens.next().expect("expecting factor");
        match tok.kind {
            TokenKind::KwTrue => Expr::BoolLit(true),
            TokenKind::KwFalse => Expr::BoolLit(false),
            TokenKind::IntLit => Expr::IntLit(tok.text.parse().expect("malformed integer literal")),
            TokenKind::StrLit => Expr::StrLit(tok.text),
            TokenKind::Ident => {
                let id = Expr::Ident(Ident(tok.text));
                if matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::LBrack) {
                    self.tokens.next();
                    let sub = self.expr();
                    self.expect(TokenKind::RBrack);
                    return Expr::Subscript(Box::new(id), Box::new(sub));
                }
                id
            }
            TokenKind::Tilde => Expr::UnOp(UnOp::Not, Box::new(self.expr())),
            TokenKind::LPar => {
                let expr = self.expr();
                self.expect(TokenKind::RPar);
                expr
            }
            TokenKind::LBrack => {
                let mut es = vec![self.expr()];
                while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::Comma) {
                    self.tokens.next();
                    es.push(self.expr());
                }
                self.expect(TokenKind::RBrack);
                Expr::ArrayLit(es)
            }
            tok => panic!("syntactic error: expecting factor (context={:?})", tok),
        }
    }
}

// Semantic

struct Env<T: Clone> {
    scopes: Vec<HashMap<Ident, T>>,
}

impl<T: Clone> Env<T> {
    fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scopes.pop().expect("empty scope chain");
    }

    fn insert(&mut self, id: Ident, value: T) {
        self.scopes
            .last_mut()
            .expect("empty scope chain")
            .insert(id, value);
    }

    fn lookup(&self, id: &Ident) -> Option<&T> {
        for s in self.scopes.iter().rev() {
            if let x @ Some(_) = s.get(id) {
                return x;
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Boolean,
    Integer,
    String,
    Array(Box<Type>, Option<usize>),
}

fn resolve_type(name: &str) -> Type {
    match name {
        "boolean" => Type::Boolean,
        "integer" => Type::Integer,
        "string" => Type::String,
        _ => panic!("type error: unknown type {}", name),
    }
}

fn resolve_type_annot(name: &TypeAnnot) -> Type {
    match name {
        TypeAnnot::TypeName(ty_name) => resolve_type(&ty_name[..]),
        TypeAnnot::Array(base_ty, size) => {
            Type::Array(Box::new(resolve_type_annot(base_ty)), *size)
        }
    }
}

type TypeEnv = Env<Type>;

fn type_check(ty_env: &mut TypeEnv, prog: &Program) {
    // TODO: gross hack
    ty_env.enter();
    ty_env.insert(
        Ident("ARGS".to_string()),
        Type::Array(Box::new(Type::Integer), None),
    );
    type_check_stmt(ty_env, &prog.body);
    ty_env.exit();
}

fn type_check_decl(ty_env: &mut TypeEnv, decl: &Decl) {
    match decl {
        Decl::Var(name, ty_annot, init_expr) => {
            let var_ty = ty_annot.as_ref().map(resolve_type_annot);
            let init_expr_ty = init_expr.as_ref().map(|e| type_of_expr(ty_env, e));
            let ty = match (&var_ty, &init_expr_ty) {
                (Some(ty @ Type::Array(sty1, Some(len1))), Some(Type::Array(sty2, Some(len2)))) => {
                    if *len1 <= 0 {
                        panic!("arrays must positive sizes");
                    }
                    if sty1 != sty2 || len2 > len1 {
                        panic!("type error: the type initialiser for variable `{}' doesn't match its type annotation", name.0);
                    }
                    ty.clone()
                }
                (Some(ty1), Some(ty2)) => {
                    if ty1 != ty2 {
                        panic!("type error: the type initialiser for variable `{}' doesn't match its type annotation", name.0);
                    }
                    ty1.clone()
                }
                (Some(ty @ Type::Array(_, Some(len))), None)
                | (None, Some(ty @ Type::Array(_, Some(len)))) => {
                    if *len <= 0 {
                        panic!("arrays must positive sizes");
                    }
                    ty.clone()
                }
                (Some(ty), None) | (None, Some(ty)) => ty.clone(),
                (None, None) => {
                    panic!("type error: a variable introduced by `let' must have an initialiser")
                }
            };
            ty_env.insert(name.clone(), ty);
        }
        Decl::Procedure(_, _, _) => todo!(),
    }
}

fn type_check_stmt(ty_env: &mut TypeEnv, stmt: &Stmt) {
    match stmt {
        Stmt::Skip => (),
        Stmt::Expr(e) => {
            type_of_expr(ty_env, e);
        }
        Stmt::Decls(decls) => {
            for d in decls {
                type_check_decl(ty_env, d);
            }
        }
        Stmt::Block(body) => {
            ty_env.enter();
            for stmt in &body.body {
                type_check_stmt(ty_env, stmt);
            }
            ty_env.exit();
        }
        Stmt::BeginUntil(situations, s, handlers) => {
            ty_env.enter();
            for id in situations {
                ty_env.insert(id.clone(), Type::Integer);
            }
            type_check_stmt(ty_env, s);
            for (id, s) in handlers {
                // TODO: this is inefficient
                if !situations.contains(id) {
                    panic!("type error: situation in handler is not present in loop until declaration: {}", id.0)
                }
                type_check_stmt(ty_env, s);
            }
            ty_env.exit();
        }
        Stmt::IfThenElse(test, s, t) | Stmt::LoopWhile(s, test, t) => {
            let tty = type_of_expr(ty_env, test);
            if tty != Type::Boolean {
                panic!("type error: condition must be a boolean value");
            }
            ty_env.enter();
            type_check_stmt(ty_env, s);
            ty_env.exit();
            ty_env.enter();
            type_check_stmt(ty_env, t);
            ty_env.exit();
        }
        Stmt::LoopUntil(situations, s, handlers) => {
            ty_env.enter();
            for id in situations {
                ty_env.insert(id.clone(), Type::Integer);
            }
            ty_env.enter();
            type_check_stmt(ty_env, s);
            for (id, s) in handlers {
                // TODO: this is inefficient
                if !situations.contains(id) {
                    panic!("type error: situation in handler is not present in loop until declaration: {}", id.0)
                }
                type_check_stmt(ty_env, s);
            }
            ty_env.exit();
            ty_env.exit();
        }
        Stmt::WhileElseIf(test, s, alts) => {
            let tty = type_of_expr(ty_env, test);
            if tty != Type::Boolean {
                panic!("type error: condition must be a boolean value");
            }
            ty_env.enter();
            type_check_stmt(ty_env, s);
            for (atest, ablock) in alts {
                let atty = type_of_expr(ty_env, atest);
                if atty != Type::Boolean {
                    panic!("type error: condition must be a boolean value");
                }
                type_check_stmt(ty_env, ablock);
            }
            ty_env.exit();
        }
    };
}

fn type_of_expr(ty_env: &TypeEnv, expr: &Expr) -> Type {
    match expr {
        Expr::BoolLit(_) => Type::Boolean,
        Expr::IntLit(_) => Type::Integer,
        Expr::StrLit(_) => Type::String,
        Expr::ArrayLit(es) => {
            if es.is_empty() {
                panic!("type error: can't determine the base type of an empty array literal");
            }
            let mut etys = es.iter().map(|e| type_of_expr(ty_env, e));
            let ty = etys.next().unwrap();
            for ety in etys {
                if ety != ty {
                    panic!("type error: all the items in an array literal must have the same type");
                }
            }
            Type::Array(Box::new(ty), Some(es.len()))
        }
        Expr::Ident(id) => ty_env
            .lookup(id)
            .cloned()
            .unwrap_or_else(|| panic!("unbound identifier `{}'", id.0)),
        Expr::Subscript(target, sub) => {
            let tty = type_of_expr(ty_env, target);
            let sty = type_of_expr(ty_env, sub);
            if sty != Type::Integer {
                panic!("type error: array subscripts must have integer type");
            }
            match tty {
                Type::Array(ty, _) => *ty,
                _ => panic!("type error: only arrays can be subscripted"),
            }
        }
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
        Expr::Assign(lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            match (&lty, &rty) {
                (Type::Array(sty1, Some(len1)), Type::Array(sty2, Some(len2))) => {
                    if sty1 != sty2 || len2 > len1 {
                        panic!("type error: trying to assign {:?} to {:?}", rty, lty);
                    }
                }
                (ty1, ty2) => {
                    if ty1 != ty2 {
                        panic!("type error: trying to assign {:?} to {:?}", rty, lty);
                    }
                }
            };
            lty
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lty = type_of_expr(ty_env, lhs);
            let rty = type_of_expr(ty_env, rhs);
            match op {
                BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Div => {
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
                BinOp::LogicAnd | BinOp::LogicOr => {
                    if lty != rty || lty != Type::Boolean {
                        panic!("type error: operator {:?} must have boolean operands", op);
                    }
                    Type::Boolean
                }
            }
        }
    }
}

// Compiler + VM

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct StrIdx(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LabelIdx(usize);

#[derive(Clone, Copy)]
enum OpCode {
    Dup,
    Pop,
    LdcI8(i64),
    LdcStr(StrIdx),
    LdLoc(LocalIdx),
    StLoc(LocalIdx),
    LdElem,
    StElem,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    Br(LabelIdx),
    BrFalse(LabelIdx),
    WriteStr,
    Nop,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Dup => f.write_str("dup"),
            OpCode::Pop => f.write_str("pop"),
            OpCode::LdcI8(n) => f.write_fmt(format_args!("ldc.i8 {}", n)),
            OpCode::LdcStr(idx) => f.write_fmt(format_args!("ldc.str {}", idx.0)),
            OpCode::LdLoc(idx) => f.write_fmt(format_args!("ld.loc {}", idx.0)),
            OpCode::StLoc(idx) => f.write_fmt(format_args!("st.loc {}", idx.0)),
            OpCode::LdElem => f.write_fmt(format_args!("ld.elem")),
            OpCode::StElem => f.write_fmt(format_args!("st.elem")),
            OpCode::Not => f.write_str("not"),
            OpCode::Add => f.write_str("add"),
            OpCode::Sub => f.write_str("sub"),
            OpCode::Mul => f.write_str("mul"),
            OpCode::Div => f.write_str("div"),
            OpCode::Eq => f.write_str("eq"),
            OpCode::Lt => f.write_str("lt"),
            OpCode::Gt => f.write_str("gt"),
            OpCode::Br(lab) => f.write_fmt(format_args!("br {}", lab.0)),
            OpCode::BrFalse(lab) => f.write_fmt(format_args!("br.false {}", lab.0)),
            OpCode::WriteStr => f.write_str("write.str"),
            OpCode::Nop => f.write_str("nop"),
        }
    }
}

struct CodeBlock {
    code: Vec<OpCode>,
    str_pool: Vec<String>,
    str_map: HashMap<String, StrIdx>,
    labels: HashMap<LabelIdx, usize>,
    next_local_idx: usize,
    next_label_idx: usize,
}

impl CodeBlock {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            str_pool: Vec::new(),
            str_map: HashMap::new(),
            labels: HashMap::new(),
            next_local_idx: 0,
            next_label_idx: 0,
        }
    }

    fn intern_string(&mut self, s: &str) -> StrIdx {
        match self.str_map.get(s) {
            Some(idx) => *idx,
            None => {
                let idx = StrIdx(self.str_pool.len());
                self.str_pool.push(s.to_owned());
                idx
            }
        }
    }

    fn add_local(&mut self, size: usize) -> LocalIdx {
        let idx = LocalIdx(self.next_local_idx);
        self.next_local_idx += size;
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

    fn run(&mut self, gen: &CodeBlock, args: &[i64]) -> i64 {
        self.call_stack
            .push(ActivationRecord::new(gen.next_local_idx));
        // TODO: gross hack
        for (i, arg) in args.iter().enumerate() {
            self.call_stack[0].locals[i] = *arg;
        }
        let mut ip = 0;
        while ip < gen.code.len() {
            match gen.code[ip] {
                OpCode::Dup => {
                    let x = self.stack.last().expect("stack underflow").clone();
                    self.stack.push(x);
                    ip += 1;
                }
                OpCode::Pop => {
                    self.stack.pop().expect("stack underflow");
                    ip += 1;
                }
                OpCode::LdcI8(n) => {
                    self.stack.push(n);
                    ip += 1;
                }
                OpCode::LdcStr(idx) => {
                    // TODO hacky pointers
                    self.stack.push(idx.0 as i64);
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
                OpCode::LdElem => {
                    let base_off = self.stack.pop().expect("stack underflow");
                    let idx = self.stack.pop().expect("stack underflow");
                    self.stack
                        .push(self.call_stack.last().unwrap().locals[(base_off + idx) as usize]);
                    ip += 1;
                }
                OpCode::StElem => {
                    let arec = self.call_stack.last_mut().unwrap();
                    let x = self.stack.pop().expect("stack underflow");
                    let base_off = self.stack.pop().expect("stack underflow");
                    let idx = self.stack.pop().expect("stack underflow");
                    arec.locals[(base_off + idx) as usize] = x;
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
                OpCode::Mul => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m * n);
                    ip += 1;
                }
                OpCode::Div => {
                    let n = self.stack.pop().expect("stack underflow");
                    let m = self.stack.pop().expect("stack underflow");
                    self.stack.push(m / n);
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
                OpCode::WriteStr => {
                    let idx = self.stack.pop().expect("stack underflow");
                    println!("{}", gen.str_pool[idx as usize]);
                    ip += 1;
                }
                OpCode::Nop => ip += 1,
            }
        }
        self.call_stack.pop();
        self.stack.pop().expect("stack underflow")
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
enum Symbol {
    Local(LocalIdx),
    LocalArray(LocalIdx, Option<usize>),
    Situation(LabelIdx),
}

type SymTab = Env<Symbol>;

#[derive(Debug, Clone, Copy)]
enum Item {
    Stack,
    Imm(i64),
    Str(StrIdx),
    Local(LocalIdx),
    ArrayElem(LocalIdx, Option<usize>),
}

fn compile(prog: &Program, gen: &mut CodeBlock) {
    let mut symtab = SymTab::new();
    symtab.enter();
    // TODO: gross hack
    symtab.insert(
        Ident("ARGS".to_string()),
        Symbol::LocalArray(LocalIdx(0), None),
    );
    compile_stmt(&prog.body, gen, &mut symtab);
    symtab.exit();
}

fn compile_decl(decl: &Decl, gen: &mut CodeBlock, symtab: &mut SymTab) {
    match decl {
        Decl::Var(name, ty_annot, init_expr) => {
            let idx = match ty_annot {
                Some(TypeAnnot::Array(_, Some(size))) => {
                    let idx = gen.add_local(*size);
                    symtab.insert(name.clone(), Symbol::LocalArray(idx, Some(*size)));
                    idx
                }
                Some(TypeAnnot::Array(_, None)) => todo!("open arrays"),
                _ => {
                    let idx = gen.add_local(1);
                    symtab.insert(name.clone(), Symbol::Local(idx));
                    idx
                } // TODO: not correct
            };
            match &init_expr {
                Some(Expr::ArrayLit(es)) => {
                    for (i, e) in es.iter().enumerate() {
                        gen.emit(OpCode::LdcI8(idx.0 as i64));
                        gen.emit(OpCode::LdcI8(i as i64));
                        let item = compile_expr(e, gen, symtab);
                        load(gen, item);
                        gen.emit(OpCode::StElem)
                    }
                }
                Some(e) => {
                    let item = compile_expr(e, gen, symtab);
                    load(gen, item);
                    gen.emit(OpCode::StLoc(idx));
                }
                _ => (),
            }
        }
        Decl::Procedure(_, _, _) => todo!(),
    }
}

fn compile_block(block: &Block, gen: &mut CodeBlock, symtab: &mut SymTab) {
    symtab.enter();
    for s in &block.body {
        compile_stmt(s, gen, symtab);
    }
    if block.discard_value {
        gen.emit(OpCode::Pop);
    }
    symtab.exit();
}

fn compile_stmt(stmt: &Stmt, gen: &mut CodeBlock, symtab: &mut SymTab) {
    match stmt {
        Stmt::Skip => gen.emit(OpCode::Nop),
        Stmt::Expr(e) => {
            let item = compile_expr(e, gen, symtab);
            load(gen, item);
        }
        Stmt::Decls(decls) => {
            for d in decls {
                compile_decl(d, gen, symtab);
            }
        }
        Stmt::Block(body) => compile_block(body, gen, symtab),
        Stmt::BeginUntil(situations, s, handlers) => {
            symtab.enter();
            let end_lab = gen.new_label();
            let mut situation_labs = HashMap::new();
            for id in situations {
                let lab = gen.new_label();
                symtab.insert(id.clone(), Symbol::Situation(lab));
                situation_labs.insert(id.clone(), lab);
            }
            compile_stmt(s, gen, symtab);
            for (id, s) in handlers {
                gen.mark_label(
                    *situation_labs
                        .get(id)
                        .expect("unknown situation label. bug?"),
                );
                compile_stmt(s, gen, symtab);
                gen.emit(OpCode::Br(end_lab));
            }
            gen.mark_label(end_lab);
            symtab.exit();
        }
        Stmt::IfThenElse(test, cons, alt) => {
            let else_lab = gen.new_label();
            let end_lab = gen.new_label();
            let titem = compile_expr(test, gen, symtab);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(else_lab));
            symtab.enter();
            compile_stmt(cons, gen, symtab);
            gen.emit(OpCode::Br(end_lab));
            symtab.exit();
            symtab.enter();
            gen.mark_label(else_lab);
            compile_stmt(alt, gen, symtab);
            gen.mark_label(end_lab);
            symtab.exit();
        }
        Stmt::LoopWhile(s, test, t) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            gen.mark_label(loop_lab);
            compile_stmt(s, gen, symtab);
            let titem = compile_expr(test, gen, symtab);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(end_lab));
            compile_stmt(t, gen, symtab);
            gen.emit(OpCode::Br(loop_lab));
            gen.mark_label(end_lab);
        }
        Stmt::LoopUntil(situations, s, handlers) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            let mut situation_labs = HashMap::new();
            symtab.enter();
            for id in situations {
                let lab = gen.new_label();
                symtab.insert(id.clone(), Symbol::Situation(lab));
                situation_labs.insert(id.clone(), lab);
            }
            symtab.enter();
            gen.mark_label(loop_lab);
            compile_stmt(s, gen, symtab);
            gen.emit(OpCode::Br(loop_lab));
            for (id, s) in handlers {
                gen.mark_label(
                    *situation_labs
                        .get(id)
                        .expect("unknown situation label. bug?"),
                );
                compile_stmt(s, gen, symtab);
                gen.emit(OpCode::Br(end_lab));
            }
            gen.mark_label(end_lab);
            symtab.exit();
            symtab.exit();
        }
        Stmt::WhileElseIf(test, s, alts) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            let mut alt_labs = Vec::new();
            for _ in 0..alts.len() {
                alt_labs.push(gen.new_label());
            }
            alt_labs.push(end_lab);
            let mut next_lab_idx = 0;

            gen.mark_label(loop_lab);
            symtab.enter();
            let titem = compile_expr(test, gen, symtab);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(alt_labs[next_lab_idx]));
            compile_stmt(s, gen, symtab);
            gen.emit(OpCode::Br(loop_lab));
            symtab.exit();

            for (atest, ablock) in alts {
                gen.mark_label(alt_labs[next_lab_idx]);
                next_lab_idx += 1;
                symtab.enter();
                let titem = compile_expr(atest, gen, symtab);
                load(gen, titem);
                gen.emit(OpCode::BrFalse(end_lab));
                compile_stmt(ablock, gen, symtab);
                gen.emit(OpCode::Br(loop_lab));
                symtab.exit();
            }

            gen.mark_label(end_lab);
        }
    }
}

fn compile_expr(expr: &Expr, gen: &mut CodeBlock, symtab: &SymTab) -> Item {
    match expr {
        Expr::BoolLit(b) => Item::Imm(if *b { 1 } else { 0 }),
        Expr::IntLit(n) => Item::Imm(*n),
        Expr::StrLit(s) => Item::Str(gen.intern_string(&s)),
        Expr::ArrayLit(_) => Item::Stack,
        Expr::Ident(id) => {
            let sym = symtab
                .lookup(id)
                .unwrap_or_else(|| panic!("unbound identifier: {}", id.0));
            match sym {
                Symbol::Local(idx) => Item::Local(*idx),
                Symbol::LocalArray(idx, size) => Item::ArrayElem(*idx, *size),
                Symbol::Situation(lab) => {
                    gen.emit(OpCode::Br(*lab));
                    // TODO: Is this right?
                    Item::Stack
                }
            }
        }
        Expr::Subscript(target, sub) => {
            let titem = compile_expr(target, gen, symtab);
            let sitem = compile_expr(sub, gen, symtab);
            match (titem, sitem) {
                (Item::ArrayElem(base_off, Some(size)), Item::Imm(idx)) => {
                    if idx < 0 || idx as usize >= size {
                        panic!("subscript out of bounds");
                    }
                    Item::Local(LocalIdx(base_off.0 + idx as usize))
                }
                (Item::ArrayElem(base_off, None), Item::Imm(idx)) => {
                    // TODO: bounds check
                    Item::Local(LocalIdx(base_off.0 + idx as usize))
                }
                (Item::ArrayElem(base_off, _size), _) => {
                    // TODO: bounds check
                    load(gen, sitem);
                    gen.emit(OpCode::LdcI8(base_off.0 as i64));
                    titem
                }
                _ => panic!("{:?} {:?}", titem, sitem),
            }
        }
        Expr::UnOp(op, arg) => {
            let aitem = compile_expr(arg, gen, symtab);
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
        Expr::BinOp(BinOp::LogicAnd, lhs, rhs) => {
            let false_lab = gen.new_label();
            let end_lab = gen.new_label();
            let litem = compile_expr(lhs, gen, symtab);
            load(gen, litem);
            gen.emit(OpCode::BrFalse(false_lab));
            let ritem = compile_expr(rhs, gen, symtab);
            load(gen, ritem);
            gen.emit(OpCode::BrFalse(false_lab));
            gen.emit(OpCode::LdcI8(1));
            gen.emit(OpCode::Br(end_lab));
            gen.mark_label(false_lab);
            gen.emit(OpCode::LdcI8(0));
            gen.mark_label(end_lab);
            Item::Stack
        }
        // TODO: array assignment
        Expr::Assign(lhs, rhs) => match compile_expr(lhs, gen, symtab) {
            Item::Imm(_) | Item::Stack => panic!("trying to assign to value on stack! bug?"),
            Item::Str(_) => todo!("string assignment"),
            Item::Local(idx) => {
                let item = compile_expr(rhs, gen, symtab);
                load(gen, item);
                gen.emit(OpCode::Dup);
                gen.emit(OpCode::StLoc(idx));
                Item::Stack
            }
            Item::ArrayElem(_, _) => {
                let item = compile_expr(rhs, gen, symtab);
                load(gen, item);
                gen.emit(OpCode::Dup);
                gen.emit(OpCode::StElem);
                Item::Stack
            }
        },
        Expr::BinOp(BinOp::LogicOr, lhs, rhs) => {
            let else_lab = gen.new_label();
            let false_lab = gen.new_label();
            let end_lab = gen.new_label();
            let litem = compile_expr(lhs, gen, symtab);
            load(gen, litem);
            gen.emit(OpCode::BrFalse(else_lab));
            gen.emit(OpCode::LdcI8(1));
            gen.emit(OpCode::Br(end_lab));
            gen.mark_label(else_lab);
            let ritem = compile_expr(rhs, gen, symtab);
            load(gen, ritem);
            gen.emit(OpCode::BrFalse(false_lab));
            gen.emit(OpCode::LdcI8(1));
            gen.emit(OpCode::Br(end_lab));
            gen.mark_label(false_lab);
            gen.emit(OpCode::LdcI8(0));
            gen.mark_label(end_lab);
            Item::Stack
        }
        Expr::BinOp(op, lhs, rhs) => {
            let litem = compile_expr(lhs, gen, symtab);
            let ritem = compile_expr(rhs, gen, symtab);
            match (litem, ritem) {
                // TODO: overflow
                (Item::Imm(m), Item::Imm(n)) => match op {
                    BinOp::Plus => Item::Imm(m + n),
                    BinOp::Minus => Item::Imm(m - n),
                    BinOp::Mul => Item::Imm(m * n),
                    BinOp::Div => Item::Imm(m / n),
                    BinOp::Eq => Item::Imm(if m == n { 1 } else { 0 }),
                    BinOp::Neq => Item::Imm(if m != n { 1 } else { 0 }),
                    BinOp::Lt => Item::Imm(if m < n { 1 } else { 0 }),
                    BinOp::Lte => Item::Imm(if m <= n { 1 } else { 0 }),
                    BinOp::Gt => Item::Imm(if m > n { 1 } else { 0 }),
                    BinOp::Gte => Item::Imm(if m >= n { 1 } else { 0 }),
                    _ => panic!(),
                },
                _ => {
                    load(gen, litem);
                    load(gen, ritem);
                    match op {
                        BinOp::Plus => gen.emit(OpCode::Add),
                        BinOp::Minus => gen.emit(OpCode::Sub),
                        BinOp::Mul => gen.emit(OpCode::Mul),
                        BinOp::Div => gen.emit(OpCode::Div),
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
                        _ => panic!(),
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
        Item::Str(idx) => gen.emit(OpCode::LdcStr(idx)),
        Item::Local(idx) => gen.emit(OpCode::LdLoc(idx)),
        Item::ArrayElem(_, _) => {
            // TODO: bounds check?
            gen.emit(OpCode::LdElem);
        }
        Item::Stack => (),
    }
}

// Driver

fn compile_and_run(s: &str, args: &[i64]) {
    let sr = SourceReader::new(s);
    let mut s = Scanner::new(sr);
    let mut p = Parser::new(s.scan());
    let node = p.parse();
    println!("; {:?}", node);

    let mut ty_env = TypeEnv::new();
    type_check(&mut ty_env, &node);

    let mut gen = CodeBlock::new();
    // TODO: gross hack
    gen.next_local_idx = args.len();
    compile(&node, &mut gen);
    gen.backpatch();

    println!("; code:");
    for (off, op) in gen.code.iter().enumerate() {
        println!(";   {:02}: {}", off, op);
    }

    let mut vm = Vm::new();
    println!("; {:?}", vm.run(&gen, args));
}

fn repl(args: &[i64]) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    loop {
        stdout.write_all(b"> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buf).unwrap();
        compile_and_run(&buf, args);
        buf.clear();
    }
}

#[derive(Debug)]
struct Options {
    source_file: Option<String>,
    args: Vec<i64>,
}

fn parse_args() -> Options {
    let mut args = env::args().skip(1);
    let mut source_file = None;
    while let Some(arg) = args.next() {
        if arg == "-f" {
            source_file = args.next();
        }
        if arg == "--" {
            break;
        }
    }
    Options {
        source_file,
        args: args
            .map(|s| s.parse().expect("script args must be integers"))
            .collect::<Vec<i64>>(),
    }
}

fn main() {
    let opts = parse_args();
    if let Some(s) = opts.source_file {
        let s = fs::read_to_string(&s).expect("could not read source file");
        compile_and_run(&s, opts.args.as_slice());
    } else {
        repl(opts.args.as_slice());
    }
}

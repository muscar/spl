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

#[derive(Debug, Clone)]
enum TypeAnnot {
    TypeName(String),
    Array(Box<TypeAnnot>, usize),
}

#[derive(Debug)]
struct Program {
    body: Stmt,
}

#[derive(Debug)]
enum Stmt {
    Skip,
    Expr(Box<Expr>),
    Block(Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    Decls(Vec<Decl>),
    IfThenElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    BeginUntil(Vec<Ident>, Vec<Stmt>, Vec<(Ident, Stmt)>),
    LoopWhile(Vec<Stmt>, Box<Expr>, Vec<Stmt>),
    LoopUntil(Vec<Ident>, Vec<Stmt>, Vec<(Ident, Stmt)>),
    WhileElseIf(Box<Expr>, Vec<Stmt>, Vec<(Expr, Vec<Stmt>)>),
}

#[derive(Debug)]
enum Decl {
    Var(Ident, Option<TypeAnnot>, Option<Expr>),
    Procedure(Ident, Option<TypeAnnot>, Vec<(Ident, TypeAnnot)>),
}

#[derive(Debug)]
enum Expr {
    BoolLit(bool),
    IntLit(i64),
    ArrayLit(Vec<Expr>),
    Ident(Ident),
    Subscript(Box<Expr>, Box<Expr>),
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
}

// Scanner

#[derive(Debug, PartialEq, Eq, Clone)]
enum TokenKind {
    IntLit,
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
                TokenKind::Slash
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

    fn is_mul_op(tok: &Token) -> bool {
        tok.kind == TokenKind::KwBoolean
            || tok.kind == TokenKind::Star
            || tok.kind == TokenKind::Slash
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
                TypeAnnot::Array(Box::new(ty_name), 0)
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
        let body = self.statement_list();
        self.expect(TokenKind::KwEnd);
        Stmt::Block(body)
    }

    fn begin_until(&mut self) -> Stmt {
        self.expect(TokenKind::KwUntil);
        let situations = self.ident_list(TokenKind::KwOr);
        let s = self.statement_list();
        self.expect(TokenKind::KwEnd);
        self.expect(TokenKind::KwThen);
        let mut handlers = vec![self.situation_handler()];
        while !matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwFi) {
            handlers.push(self.situation_handler());
        }
        self.expect(TokenKind::KwFi);
        Stmt::BeginUntil(situations, s, handlers)
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
            let dim = self
                .expect(TokenKind::IntLit)
                .parse()
                .expect("array dimensions must be integer literals");
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

    fn situation_handler(&mut self) -> (Ident, Stmt) {
        let situation = self.ident();
        self.expect(TokenKind::FatArrow);
        let s = self.statement();
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

    fn while_else_if(&mut self) -> Stmt {
        self.expect(TokenKind::KwWhile);
        let test = self.expr();
        let s = self.statement_list();
        let mut alts = Vec::new();
        while matches!(self.tokens.peek(), Some(tok) if tok.kind == TokenKind::KwElse) {
            self.expect(TokenKind::KwElse);
            self.expect(TokenKind::KwIf);
            let test = self.expr();
            self.expect(TokenKind::KwThen);
            let t = self.statement_list();
            alts.push((test, t));
        }
        self.expect(TokenKind::KwFi);
        Stmt::WhileElseIf(Box::new(test), s, alts)
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
            Type::Array(Box::new(resolve_type_annot(base_ty)), Some(*size))
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
            let ty = match (var_ty, init_expr_ty) {
                (Some(Type::Array(sty1, Some(len1))), Some(Type::Array(sty2, Some(len2)))) => {
                    if sty1 != sty2 || len2 > len1 {
                        panic!("type error: the type initialiser for variable `{}' doesn't match its type annotation", name.0);
                    }
                    Type::Array(sty1, Some(len1))
                }
                (Some(ty1), Some(ty2)) => {
                    if ty1 != ty2 {
                        panic!("type error: the type initialiser for variable `{}' doesn't match its type annotation", name.0);
                    }
                    ty1
                }
                (Some(ty), None) | (None, Some(ty)) => ty,
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
            for stmt in body {
                type_check_stmt(ty_env, stmt);
            }
            ty_env.exit();
        }
        Stmt::BeginUntil(situations, s, handlers) => {
            ty_env.enter();
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
                type_check_stmt(ty_env, s);
            }
            ty_env.exit();
        }
        Stmt::Assign(lhs, rhs) => {
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
        }
        Stmt::IfThenElse(test, s, t) | Stmt::LoopWhile(s, test, t) => {
            let tty = type_of_expr(ty_env, test);
            if tty != Type::Boolean {
                panic!("type error: condition must be a boolean value");
            }
            ty_env.enter();
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
            ty_env.exit();
            ty_env.enter();
            for stmt in t {
                type_check_stmt(ty_env, stmt);
            }
            ty_env.exit();
        }
        Stmt::LoopUntil(situations, s, handlers) => {
            ty_env.enter();
            for id in situations {
                ty_env.insert(id.clone(), Type::Integer);
            }
            ty_env.enter();
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
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
            for stmt in s {
                type_check_stmt(ty_env, stmt);
            }
            for (atest, ablock) in alts {
                let atty = type_of_expr(ty_env, atest);
                if atty != Type::Boolean {
                    panic!("type error: condition must be a boolean value");
                }
                for stmt in ablock {
                    type_check_stmt(ty_env, stmt);
                }
            }
            ty_env.exit();
        }
    };
}

fn type_of_expr(ty_env: &TypeEnv, expr: &Expr) -> Type {
    match expr {
        Expr::BoolLit(_) => Type::Boolean,
        Expr::IntLit(_) => Type::Integer,
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
            }
        }
    }
}

// Compiler + VM

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LabelIdx(usize);

#[derive(Clone, Copy)]
enum OpCode {
    LdcI8(i64),
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
    Nop,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::LdcI8(n) => f.write_fmt(format_args!("ldc.i8 {}", n)),
            OpCode::LdLoc(idx) => f.write_fmt(format_args!("ldloc {}", idx.0)),
            OpCode::StLoc(idx) => f.write_fmt(format_args!("stloc {}", idx.0)),
            OpCode::LdElem => f.write_fmt(format_args!("ldelem")),
            OpCode::StElem => f.write_fmt(format_args!("stelem")),
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
    LocalArray(LocalIdx, usize),
    Situation(LabelIdx),
}

type SymTab = Env<Symbol>;

#[derive(Debug, Clone, Copy)]
enum Item {
    Stack,
    Imm(i64),
    Local(LocalIdx),
    ArrayElem(LocalIdx),
}

fn compile(prog: &Program, gen: &mut CodeBlock) {
    let mut symtab = SymTab::new();
    symtab.enter();
    // TODO: gross hack
    symtab.insert(
        Ident("ARGS".to_string()),
        Symbol::LocalArray(LocalIdx(0), 0),
    );
    compile_stmt(&prog.body, gen, &mut symtab);
    symtab.exit();
}

fn compile_decl(decl: &Decl, gen: &mut CodeBlock, symtab: &mut SymTab) {
    match decl {
        Decl::Var(name, ty_annot, init_expr) => {
            let idx = match ty_annot {
                Some(TypeAnnot::Array(_, dim)) => {
                    let idx = gen.add_local(*dim);
                    symtab.insert(name.clone(), Symbol::LocalArray(idx, *dim));
                    idx
                }
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

fn compile_stmt(stmt: &Stmt, gen: &mut CodeBlock, symtab: &mut SymTab) {
    match stmt {
        Stmt::Skip => gen.emit(OpCode::Nop),
        Stmt::Expr(e) => {
            let item = compile_expr(e, gen, symtab);
            load(gen, item);
        }
        // TODO: array assignment
        Stmt::Assign(lhs, rhs) => match compile_expr(lhs, gen, symtab) {
            Item::Imm(_) | Item::Stack => panic!("trying to assign to value on stack! bug?"),
            Item::Local(idx) => {
                let item = compile_expr(rhs, gen, symtab);
                load(gen, item);
                gen.emit(OpCode::StLoc(idx));
            }
            Item::ArrayElem(_) => {
                let item = compile_expr(rhs, gen, symtab);
                load(gen, item);
                gen.emit(OpCode::StElem);
            }
        },
        Stmt::Decls(decls) => {
            for d in decls {
                compile_decl(d, gen, symtab);
            }
        }
        Stmt::Block(body) => {
            symtab.enter();
            for s in body {
                compile_stmt(s, gen, symtab);
            }
            symtab.exit();
        }
        Stmt::BeginUntil(situations, s, handlers) => {
            symtab.enter();
            let end_lab = gen.new_label();
            let mut situation_labs = HashMap::new();
            for id in situations {
                let lab = gen.new_label();
                symtab.insert(id.clone(), Symbol::Situation(lab));
                situation_labs.insert(id.clone(), lab);
            }
            for stmt in s {
                compile_stmt(stmt, gen, symtab);
            }
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
            for stmt in cons {
                compile_stmt(stmt, gen, symtab);
            }
            gen.emit(OpCode::Br(end_lab));
            symtab.exit();
            symtab.enter();
            gen.mark_label(else_lab);
            for stmt in alt {
                compile_stmt(stmt, gen, symtab);
            }
            gen.mark_label(end_lab);
            symtab.exit();
        }
        Stmt::LoopWhile(s, test, t) => {
            let loop_lab = gen.new_label();
            let end_lab = gen.new_label();
            gen.mark_label(loop_lab);
            for stmt in s {
                compile_stmt(stmt, gen, symtab);
            }
            let titem = compile_expr(test, gen, symtab);
            load(gen, titem);
            gen.emit(OpCode::BrFalse(end_lab));
            for stmt in t {
                compile_stmt(stmt, gen, symtab);
            }
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
            for stmt in s {
                compile_stmt(stmt, gen, symtab);
            }
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
            for stmt in s {
                compile_stmt(stmt, gen, symtab);
            }
            gen.emit(OpCode::Br(loop_lab));
            symtab.exit();

            for (atest, ablock) in alts {
                gen.mark_label(alt_labs[next_lab_idx]);
                next_lab_idx += 1;
                symtab.enter();
                let titem = compile_expr(atest, gen, symtab);
                load(gen, titem);
                gen.emit(OpCode::BrFalse(end_lab));
                for stmt in ablock {
                    compile_stmt(stmt, gen, symtab);
                }
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
        Expr::ArrayLit(_) => Item::Stack,
        Expr::Ident(id) => {
            let sym = symtab
                .lookup(id)
                .unwrap_or_else(|| panic!("unbound identifier: {}", id.0));
            match sym {
                Symbol::Local(idx) => Item::Local(*idx),
                Symbol::LocalArray(idx, _) => Item::ArrayElem(*idx),
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
                (Item::ArrayElem(base_off), Item::Imm(idx)) => {
                    Item::Local(LocalIdx(base_off.0 + idx as usize))
                }
                (Item::ArrayElem(base_off), _) => {
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
        Item::ArrayElem(_) => {
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

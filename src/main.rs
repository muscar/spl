use std::{
    io::{self, Write},
    iter::Peekable,
    str::Chars,
};

#[derive(Debug)]
enum Expr {
    IntLit(i64),
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
    Plus,
    Minus,
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

    fn next(&mut self) -> Option<char> {
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
            self.reader.next();
        }
        self.lexeme.clear();
        match self.reader.peek() {
            Some('+') => {
                self.lexeme.push(self.reader.next().unwrap());
                Some((Token::Plus, self.lexeme.clone()))
            }
            Some('-') => {
                self.lexeme.push(self.reader.next().unwrap());
                Some((Token::Minus, self.lexeme.clone()))
            }
            Some(c) if c.is_digit(10) => {
                while matches!(self.reader.peek(), Some(c) if c.is_digit(10)) {
                    self.lexeme.push(self.reader.next().unwrap());
                }
                Some((Token::IntLit, self.lexeme.clone()))
            }
            Some(_) => {
                while matches!(self.reader.peek(), Some(c) if !c.is_whitespace()) {
                    self.lexeme.push(self.reader.next().unwrap());
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

    fn parse(&mut self) -> Expr {
        self.simple_expr()
    }

    fn simple_expr(&mut self) -> Expr {
        let mut lhs = self.factor();
        while matches!(self.tokens.peek(), Some((tok, _)) if *tok == Token::Plus || *tok == Token::Minus)
        {
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
        let lex = self.expect(Token::IntLit);
        Expr::IntLit(lex.parse().expect("malformed integer literal"))
    }
}

// #[derive(Debug)]
// enum Value {
//     Int(i64),
// }

// fn eval(expr: &Expr) -> Value {
//     match expr {
//         Expr::IntLit(n) => Value::Int(*n),
//         Expr::BinOp(BinOp::Plus, lhs, rhs) => {
//             let lv = eval(lhs);
//             let rv = eval(rhs);
//             match (lv, rv) {
//                 (Value::Int(m), Value::Int(n)) => Value::Int(m + n),
//             }
//         }
//         Expr::BinOp(BinOp::Minus, lhs, rhs) => {
//             let lv = eval(lhs);
//             let rv = eval(rhs);
//             match (lv, rv) {
//                 (Value::Int(m), Value::Int(n)) => Value::Int(m - n),
//             }
//         }
//     }
// }

#[derive(Debug)]
enum OpCode {
    LdcI8(i64),
    Add,
    Sub,
    Ret,
}

struct CodeGen {
    code: Vec<OpCode>,
}

impl CodeGen {
    fn new() -> Self {
        Self { code: Vec::new() }
    }

    fn emit(&mut self, op: OpCode) {
        self.code.push(op);
    }
}

struct Vm {
    stack: Vec<i64>,
}

impl Vm {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn run(&mut self, code: &[OpCode]) -> i64 {
        for op in code {
            match op {
                OpCode::LdcI8(n) => self.stack.push(*n),
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
        self.stack.pop().expect("stack underflow")
    }
}

fn compile(expr: &Expr, gen: &mut CodeGen) {
    match expr {
        Expr::IntLit(n) => gen.emit(OpCode::LdcI8(*n)),
        Expr::BinOp(BinOp::Plus, lhs, rhs) => {
            compile(lhs, gen);
            compile(rhs, gen);
            gen.emit(OpCode::Add);
        }
        Expr::BinOp(BinOp::Minus, lhs, rhs) => {
            compile(lhs, gen);
            compile(rhs, gen);
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
        // println!("; {:?}", node);
        // println!("{:?} ; eval", eval(&node));

        let mut gen = CodeGen::new();
        compile(&node, &mut gen);
        let mut vm = Vm::new();
        println!("; {:?} ; compile", vm.run(&gen.code));

        buf.clear();
    }
}

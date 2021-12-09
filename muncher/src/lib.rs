#![allow(unused)]

mod lexer;
mod interpreter;
mod muncher;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use crate::interpreter::{Env, Interpreter};
use crate::lexer::Token;
use crate::muncher::Muncher;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    String(Rc<str>),
    Ident(Rc<str>),
    Block(Rc<Block>),
    Object(Rc<Object>),
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl Value {
    fn get_muncher(&self) -> Rc<dyn Muncher> {
        match self {
            Value::Int(i) => Rc::new(muncher::NumMuncher { value: *i }),
            Value::Bool(b) => Rc::new(muncher::BoolMuncher { value: *b }),
            Value::Block(b) => Rc::new(muncher::BlockCallMuncher { value: b.clone() }),
            Value::Object(o) => o.muncher.clone(),
            _ => Rc::new(muncher::NoMuncher),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Ident(i) => write!(f, "<Ident {:?}>", i),
            Value::Block(b) => write!(f, "<Block>"),
            Value::Object(o) => write!(f, "<Object {}>", o.name),
        }
    }
}

#[derive(Debug, Clone)]
enum Block {
    Source(SourceBlock),
    Intrinsic(Intrinsic),
}

#[derive(Clone)]
struct SourceBlock {
    closure: Env,
    tokens: Rc<[Token]>,
}

impl fmt::Debug for SourceBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceBlock")
            .field("tokens", &self.tokens)
            .finish()
    }
}

#[derive(Debug, Clone)]
enum Intrinsic {
    Print(String),
    Value(Value),
}

struct Object {
    name: Rc<str>,
    properties: RefCell<HashMap<Rc<str>, Value>>,
    muncher: Rc<dyn Muncher>,
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Object")
            .field("name", &self.name)
            .field("properties", &self.properties)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub(crate) fn union(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

pub trait Intrinsics {
    fn print(&self, value: &str);
}

pub fn eval(source: &str, intrinsics: Rc<dyn Intrinsics>) -> Result<()> {
    let tokens = lexer::lex_program(source)?;
    let env = Env::new();
    env.define_raw("print", Value::Object(Rc::new(Object {
        name: "Print".into(),
        properties: Default::default(),
        muncher: Rc::new(muncher::PrintMuncher),
    })));
    let mut interp = Interpreter {
        env: env.clone(),
        intrinsics,
    };
    interp.block_with_env(
        &Block::Source(SourceBlock {
            closure: env.clone(),
            tokens: tokens.into(),
        }),
        env,
    )?;
    Ok(())
}

#[test]
fn basic_test() {
    struct Intr;
    impl Intrinsics for Intr {
        fn print(&self, value: &str) {}
    }
    eval(
        r#" print("Hello, world!"); "#,
        Rc::new(Intr),
    ).unwrap();
}

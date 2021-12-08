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

impl Value {
    fn get_muncher(&self) -> Rc<dyn Muncher> {
        match self {
            Value::Object(o) => o.muncher.clone(),
            _ => Rc::new(muncher::NoMuncher),
        }
    }
}

#[derive(Debug, Clone)]
enum Block {
    Source(SourceBlock),
    Intrinsic(Intrinsic),
}

#[derive(Debug, Clone)]
struct SourceBlock {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
enum Intrinsic {
    Print(String),
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

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

pub trait Intrinsics {
    fn print(&self, value: &str);
}

pub fn eval(source: &str, intrinsics: Rc<dyn Intrinsics>) -> Result<()> {
    let block = lexer::lex_block(source)?;
    let env = Env::new();
    env.define_raw("print", Value::Object(Rc::new(Object {
        name: "Print".into(),
        properties: Default::default(),
        muncher: Rc::new(muncher::PrintMuncher),
    })));
    let mut interp = Interpreter {
        env,
        intrinsics,
    };
    interp.block(Rc::new(Block::Source(block)))?;
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

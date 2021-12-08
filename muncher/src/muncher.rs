use std::collections::HashMap;
use std::rc::Rc;
use crate::{Block, Error, Intrinsic, Value};
use crate::interpreter::{Env, Interpreter};
use crate::lexer::Token;

pub(crate) enum MunchOutput<'src> {
    Done {
        block: Rc<Block>,
        tokens: &'src [Token],
    },
    Continue {
        muncher: Rc<dyn Muncher>,
        tokens: &'src [Token],
    },
    Failed,
    FailedEval { error: Error },
}

pub(crate) trait Muncher {
    fn munch<'src>(&self, tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src>;
}

pub(crate) struct NoMuncher;

impl Muncher for NoMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        MunchOutput::Failed
    }
}

pub(crate) struct CompleteMuncher {
    pub(crate) block: Rc<Block>,
}

impl Muncher for CompleteMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        MunchOutput::Done { block: self.block.clone(), tokens }
    }
}

pub(crate) struct PlainMuncher {
    pub(crate) cases: HashMap<Rc<str>, Rc<dyn Muncher>>,
}

impl Muncher for PlainMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        match tokens {
            [tok, rest @ ..] => {
                if let Some(next) = self.cases.get(&tok.source).cloned() {
                    MunchOutput::Continue {
                        muncher: next,
                        tokens: rest,
                    }
                } else {
                    MunchOutput::Failed
                }
            }
            [] => {
                MunchOutput::Failed
            }
        }
    }
}

pub(crate) struct PrintMuncher;

impl Muncher for PrintMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        if let Err(e) = eat_token(&mut tokens, Token::is_left_paren) {
            return e;
        }
        let (value, rest) = match caller.expr(&tokens) {
            Ok(x) => x,
            Err(error) => return MunchOutput::FailedEval { error },
        };
        tokens = rest;
        if let Err(e) = eat_token(&mut tokens, Token::is_right_paren) {
            return e;
        }

        let printed = match value {
            Value::Nil => "nil".to_owned(),
            Value::Int(x) => x.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::String(s) => s.to_string(),
            Value::Ident(i) => format!("<Ident {:?}>", i),
            Value::Block(b) => "<Block>".to_owned(),
            Value::Object(o) => format!("<Object {}>", o.name),
        };

        MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Print(printed))),
            tokens,
        }
    }
}

fn eat_token<'src>(tokens: &mut &'src [Token], check: impl FnOnce(&Token) -> bool) -> Result<(), MunchOutput<'src>> {
    if tokens.len() == 0 || !check(&tokens[0]) {
        return Err(MunchOutput::Failed);
    }
    *tokens = &tokens[1..];
    Ok(())
}

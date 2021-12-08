use std::collections::HashMap;
use std::rc::Rc;
use crate::{Block, Error, Intrinsic, Value, Span};
use crate::interpreter::{Env, Interpreter, Tokens};
use crate::lexer::{Token, TokenKind};

pub(crate) enum MunchOutput {
    Done {
        block: Rc<Block>,
    },
    Continue {
        muncher: Rc<dyn Muncher>,
    },
    Failed,
    FailedEval { error: Error },
}

pub(crate) trait Muncher {
    fn munch(&self, tokens: &mut Tokens, env: &Env, caller: &mut Interpreter) -> MunchOutput {
        self.munch_inner(tokens, env, caller).unwrap_or_else(std::convert::identity)
    }

    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput>;
}

pub(crate) struct NoMuncher;

impl Muncher for NoMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        Ok(MunchOutput::Failed)
    }
}

pub(crate) struct CompleteMuncher {
    pub(crate) block: Rc<Block>,
}

impl Muncher for CompleteMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        Ok(MunchOutput::Done { block: self.block.clone() })
    }
}

pub(crate) struct PlainMuncher {
    pub(crate) cases: HashMap<Rc<str>, Rc<dyn Muncher>>,
}

impl Muncher for PlainMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        Ok(match tokens.peek() {
            Some(tok) => {
                if let Some(next) = self.cases.get(&tok.source).cloned() {
                    tokens.advance();
                    MunchOutput::Continue {
                        muncher: next,
                    }
                } else {
                    MunchOutput::Failed
                }
            }
            None => {
                MunchOutput::Failed
            }
        })
    }
}

pub(crate) struct ExprMuncher {
    pub(crate) bind: Token,
    pub(crate) cont: Rc<dyn Muncher>,
}

impl Muncher for ExprMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        let value = match caller.expr(tokens) {
            Ok(x) => x.value,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        env.define(self.bind.clone(), value).unwrap();
        Ok(MunchOutput::Continue {
            muncher: self.cont.clone(),
        })
    }
}

pub(crate) struct PrintMuncher;

impl Muncher for PrintMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_left_paren, "`(`").map_err(|_| MunchOutput::Failed)?;
        let value = match caller.expr(tokens) {
            Ok(x) => x.value,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        tokens.expect(Token::is_right_paren, "`)`").map_err(|_| MunchOutput::Failed)?;
        Ok(MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Print(value.to_string()))),
        })
    }
}

pub(crate) struct NumMuncher {
    pub(crate) value: i64,
}

impl Muncher for NumMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_dot, "`.`").map_err(|_| MunchOutput::Failed)?;

        fn cast_int(value: Value, span: Span) -> Result<i64, Error> {
            if let Value::Int(i) = value {
                Ok(i)
            } else {
                Err(Error {
                    msg: format!("expected int, got {}", value),
                    span,
                })
            }
        }

        let handler: fn(_, _, i64) -> _ = match tokens
            .peek()
            .filter(|t| t.kind == TokenKind::Ident)
            .map(|t| &*t.source)
        {
            Some("add") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_add(i).into()),
            Some("sub") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_sub(i).into()),
            Some("mul") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_mul(i).into()),
            Some("div") => |v, s, n| cast_int(v, s).and_then(|i| if i == 0 {
                Err(Error {
                    msg: "divisor is zero".to_owned(),
                    span: s,
                })
            } else {
                Ok(n.wrapping_div(i).into())
            }),
            Some("mod") => |v, s, n| cast_int(v, s).and_then(|i| if i == 0 {
                Err(Error {
                    msg: "divisor is zero".to_owned(),
                    span: s,
                })
            } else {
                Ok(n.wrapping_rem(i).into())
            }),
            Some("lt") => |v, s, n| cast_int(v, s).map(|i| (n < i).into()),
            Some("le") => |v, s, n| cast_int(v, s).map(|i| (n <= i).into()),
            Some("eq") => |v, s, n| cast_int(v, s).map(|i| (n == i).into()),
            Some("ne") => |v, s, n| cast_int(v, s).map(|i| (n != i).into()),
            Some("gt") => |v, s, n| cast_int(v, s).map(|i| (n > i).into()),
            Some("ge") => |v, s, n| cast_int(v, s).map(|i| (n >= i).into()),
            _ => return Err(MunchOutput::Failed),
        };
        tokens.advance();
        tokens.expect(Token::is_left_paren, "`(`").map_err(|_| MunchOutput::Failed)?;
        let value = match caller.expr(tokens) {
            Ok(x) => x,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        tokens.expect(Token::is_right_paren, "`)`").map_err(|_| MunchOutput::Failed)?;
        // fixme: bogus span
        let result = match handler(value.value, value.span, self.value) {
            Ok(x) => x,
            Err(e) => return Err(MunchOutput::FailedEval { error: e }),
        };

        Ok(MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Value(result))),
        })
    }
}

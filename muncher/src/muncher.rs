use std::collections::HashMap;
use std::rc::Rc;
use crate::{Block, Error, Intrinsic, Value, Span};
use crate::interpreter::{Env, Interpreter};
use crate::lexer::{Token, TokenKind};

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

pub(crate) struct ExprMuncher {
    pub(crate) bind: Token,
    pub(crate) cont: Rc<dyn Muncher>,
}

impl Muncher for ExprMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        let (value, rest) = match caller.expr(tokens) {
            Ok(x) => x,
            Err(error) => return MunchOutput::FailedEval { error },
        };
        env.define(self.bind.clone(), value).unwrap();
        MunchOutput::Continue {
            muncher: self.cont.clone(),
            tokens: rest,
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

        MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Print(value.to_string()))),
            tokens,
        }
    }
}

pub(crate) struct NumMuncher {
    pub(crate) value: i64,
}

impl Muncher for NumMuncher {
    fn munch<'src>(&self, mut tokens: &'src [Token], env: &Env, caller: &mut Interpreter) -> MunchOutput<'src> {
        if let Err(e) = eat_token(&mut tokens, Token::is_dot) {
            return e;
        }

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
            .get(0)
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
            _ => return MunchOutput::Failed,
        };
        tokens = &tokens[1..];
        if let Err(e) = eat_token(&mut tokens, Token::is_left_paren) {
            return e;
        }
        let (value, rest) = match caller.expr(tokens) {
            Ok(x) => x,
            Err(error) => return MunchOutput::FailedEval { error },
        };
        tokens = rest;
        if let Err(e) = eat_token(&mut tokens, Token::is_right_paren) {
            return e;
        }
        // fixme: bogus span
        let result = match handler(value, tokens[0].span, self.value) {
            Ok(x) => x,
            Err(e) => return MunchOutput::FailedEval { error: e },
        };

        MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Value(result))),
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

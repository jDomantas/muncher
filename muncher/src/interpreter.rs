use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::{Value, Block, Intrinsic, SourceBlock, Result, Error, Intrinsics};
use crate::lexer::{Token, TokenKind};
use crate::muncher::MunchOutput;

fn double_def(token: Token) -> Error {
    todo!("double def error")
}

fn undefined_var(token: Token) -> Error {
    todo!("undefined var error")
}

struct EnvInner {
    values: RefCell<HashMap<Rc<str>, Value>>,
    next: Option<Rc<EnvInner>>,
}

impl EnvInner {
    fn define(&self, ident: Token, value: Value) -> Result<()> {
        let prev = self.values.borrow_mut().insert(ident.source.clone(), value);
        if prev.is_none() {
            Ok(())
        } else {
            Err(double_def(ident))
        }
    }

    fn get(&self, ident: Token) -> Result<Value> {
        if let Some(value) = self.values.borrow().get(&ident.source).cloned() {
            Ok(value)
        } else if let Some(env) = &self.next {
            env.get(ident)
        } else {
            Err(undefined_var(ident))
        }
    }

    fn set(&self, ident: Token, value: Value) -> Result<()> {
        let mut values = self.values.borrow_mut();
        if values.contains_key(&ident.source) {
            values.insert(ident.source, value);
            return Ok(());
        } else if let Some(env) = &self.next {
            env.set(ident, value)
        } else {
            Err(undefined_var(ident))
        }
    }
}

pub(crate) struct Env {
    inner: Rc<EnvInner>,
}

impl Env {
    pub(crate) fn new() -> Env {
        Env {
            inner: Rc::new(EnvInner {
                values: Default::default(),
                next: None,
            }),
        }
    }

    pub(crate) fn define(&self, ident: Token, value: Value) -> Result<()> {
        self.inner.define(ident, value)
    }

    pub(crate) fn get(&self, ident: Token) -> Result<Value> {
        self.inner.get(ident)
    }

    pub(crate) fn set(&self, ident: Token, value: Value) -> Result<()> {
        self.inner.set(ident, value)
    }

    pub(crate) fn scope(&self) -> Env {
        Env {
            inner: Rc::new(EnvInner {
                values: Default::default(),
                next: Some(self.inner.clone()),
            }),
        }
    }

    pub(crate) fn define_raw(&self, name: &'static str, value: Value) {
        self.inner.values.borrow_mut().insert(name.into(), value);
    }

    pub(crate) fn get_raw(&self, name: &str) -> Value {
        let mut env = &*self.inner;
        loop {
            if let Some(value) = env.values.borrow().get(name).cloned() {
                return value;
            }
            env = env.next.as_ref().expect("get_raw failed");
        }
    }
}

pub(crate) struct Interpreter {
    pub(crate) env: Env,
    pub(crate) intrinsics: Rc<dyn Intrinsics>,
}

impl Interpreter {
    pub(crate) fn block(&mut self, block: Rc<Block>) -> Result<Value> {
        match &*block {
            Block::Source(s) => self.source_block(s),
            Block::Intrinsic(Intrinsic::Print(str)) => {
                self.intrinsics.print(&str);
                Ok(Value::Nil)
            }
        }
    }

    fn source_block(&mut self, block: &SourceBlock) -> Result<Value> {
        let mut tokens = block.tokens.as_slice();
        while tokens.len() > 0 {
            tokens = self.stmt(tokens)?;
        }
        Ok(Value::Nil)
    }

    pub(crate) fn stmt<'src>(&mut self, tokens: &'src [Token]) -> Result<&'src [Token]> {
        if tokens.len() == 0 {
            Ok(tokens)
        } else if let Some((_, tail)) = eat_token(tokens, |t| t.kind == TokenKind::Let) {
            let (ident, tail) = eat_token(tail, Token::is_ident)
                .ok_or_else(|| todo!("expected identifier"))?;
            let (_eq, tail) = eat_token(tail, Token::is_eq)
                .ok_or_else(|| todo!("expected `=`"))?;
            let (value, tail) = self.expr(tail)?;
            self.env.define(ident, value)?;
            Ok(tail)
        } else if let Some((_, tail)) = eat_token(tokens, Token::is_semi) {
            Ok(tail)
        } else {
            let (_, tail) = self.expr(tokens)?;
            Ok(tail)
        }
    }

    pub(crate) fn expr<'src>(&mut self, tokens: &'src [Token]) -> Result<(Value, &'src [Token])> {
        if let Some((ident, tail)) = eat_token(tokens, Token::is_ident) {
            if let Some((_eq, tail)) = eat_token(tail, Token::is_eq) {
                let (value, tail) = self.expr(tail)?;
                self.env.set(ident, value.clone())?;
                Ok((value, tail))
            } else {
                let value = self.env.get(ident)?;
                self.munch_calls(value, tail)
            }
        } else if let Some((this, tail)) = eat_token(tokens, |t| t.kind == TokenKind::This) {
            if let Some((_dot, ident, _eq, tail)) = eat_token3(
                tail,
                Token::is_dot,
                Token::is_ident,
                Token::is_eq,
            ) {
                let (value, tail) = self.expr(tail)?;
                todo!("assign object property")
                // Ok((value, tail))
            } else {
                todo!("resolve self and continue munching calls")
            }
        } else if let Some((str, tail)) = eat_token(tokens, |t| t.kind == TokenKind::String) {
            let value = Value::String(crate::lexer::unescape_string(&str.source).into());
            self.munch_calls(value, tail)
        } else if let Some((_obj, tail)) = eat_token(tokens, |t| t.kind == TokenKind::Object) {
            todo!("parse object literal")
        } else {
            println!("{:?}", tokens);
            todo!("what else?")
        }
    }

    fn munch_calls<'src>(&mut self, value: Value, tokens: &'src [Token]) -> Result<(Value, &'src [Token])> {
        if tokens.len() == 0 || !can_start_call(&tokens[0]) {
            return Ok((value, tokens))
        }
        let mut muncher = value.get_muncher();
        let mut tokens = tokens;
        let env = Env::new();
        let (block, tokens) = loop {
            match muncher.munch(tokens, &env, self) {
                MunchOutput::Done { block, tokens } => {
                    break (block, tokens);
                }
                MunchOutput::Continue { muncher: next, tokens: next_tokens } => {
                    muncher = next;
                    tokens = next_tokens;
                }
                MunchOutput::Failed => {
                    todo!("emit failed call error");
                }
                MunchOutput::FailedEval { error } => return Err(error),
            }
        };
        let mut block_interp = Interpreter {
            env,
            intrinsics: self.intrinsics.clone(),
        };
        let return_value = block_interp.block(block)?;
        self.munch_calls(return_value, tokens)
    }
}

fn can_start_call(token: &Token) -> bool {
    token.is_dot() || token.is_left_paren()
}

fn eat_token(tokens: &[Token], check: impl FnOnce(&Token) -> bool) -> Option<(Token, &[Token])> {
    match tokens.get(0) {
        Some(tok) if check(tok) => {
            Some((tokens[0].clone(), &tokens[1..]))
        }
        _ => None,
    }
}

fn eat_token3(
    tokens: &[Token],
    check1: impl FnOnce(&Token) -> bool,
    check2: impl FnOnce(&Token) -> bool,
    check3: impl FnOnce(&Token) -> bool,
) -> Option<(Token, Token, Token, &[Token])> {
    let (t1, tokens) = eat_token(tokens, check1)?;
    let (t2, tokens) = eat_token(tokens, check2)?;
    let (t3, tokens) = eat_token(tokens, check3)?;
    Some((t1, t2, t3, tokens))
}

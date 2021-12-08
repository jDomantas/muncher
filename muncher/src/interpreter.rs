use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::{Value, Block, Intrinsic, SourceBlock, Result, Error, Intrinsics, Object, Span};
use crate::lexer::{Token, TokenKind};
use crate::muncher::{MunchOutput, Muncher};

enum EvalBreak {
    Error(Error),
    Return(Value),
}

impl From<Error> for EvalBreak {
    fn from(err: Error) -> Self {
        EvalBreak::Error(err)
    }
}

#[track_caller]
fn todo_error(message: &str) -> Error {
    todo!("{}", message)
}

type EvalResult<T> = std::result::Result<T, EvalBreak>;

fn double_def(token: Token) -> Error {
    todo_error("double def")
}

fn undefined_var(token: Token) -> Error {
    todo!("error: undefined var {}", token.source)
}

#[derive(Debug)]
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

    fn get_raw(&self, var: &str) -> Option<Value> {
        if let Some(value) = self.values.borrow().get(var).cloned() {
            Some(value)
        } else {
            self.next.as_ref().and_then(|e| e.get_raw(var))
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

#[derive(Debug, Clone)]
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
        if let Some(value) = self.inner.get_raw(&ident.source) {
            Ok(value)
        } else if let Ok(num) = ident.source.parse::<i64>() {
            Ok(Value::Int(num))
        } else {
            Err(undefined_var(ident))
        }
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
}

pub(crate) struct Interpreter {
    pub(crate) env: Env,
    pub(crate) intrinsics: Rc<dyn Intrinsics>,
}

impl Interpreter {
    pub(crate) fn block(&mut self, block: Rc<Block>) -> Result<Value> {
        match &*block {
            Block::Source(s) => match self.source_block(s) {
                Ok(()) => Ok(Value::Nil),
                Err(EvalBreak::Error(err)) => Err(err),
                Err(EvalBreak::Return(ret)) => Ok(ret),
            },
            Block::Intrinsic(Intrinsic::Print(str)) => {
                self.intrinsics.print(&str);
                Ok(Value::Nil)
            }
            Block::Intrinsic(Intrinsic::Value(value)) => {
                Ok(value.clone())
            }
        }
    }

    fn source_block(&mut self, block: &SourceBlock) -> EvalResult<()> {
        let mut tokens = Tokens {
            tokens: block.tokens.clone(),
            idx: 0,
        };
        while tokens.peek().is_some() {
            self.stmt(&mut tokens)?;
        }
        Ok(())
    }

    fn stmt(&mut self, tokens: &mut Tokens) -> EvalResult<()> {
        if tokens.peek().is_none() {
            Ok(())
        } else if let Some(_let) = tokens.check(|t| t.kind == TokenKind::Let) {
            let ident = tokens.expect(Token::is_ident, "identifier")?;
            tokens.expect(Token::is_eq, "`=`")?;
            let value = self.expr(tokens)?.value;
            self.env.define(ident, value)?;
            Ok(())
        } else if let Some(_return) = tokens.check(|t| t.kind == TokenKind::Return) {
            let value = self.expr(tokens)?.value;
            Err(EvalBreak::Return(value))
        } else if let Some(_semi) = tokens.check(Token::is_semi) {
            Ok(())
        } else {
            self.expr(tokens)?;
            Ok(())
        }
    }

    pub(crate) fn expr(&mut self, tokens: &mut Tokens) -> Result<SpannedValue> {
        if let Some(ident) = tokens.check(Token::is_ident) {
            if let Some(_eq) = tokens.check(Token::is_eq) {
                let value = self.expr(tokens)?;
                self.env.set(ident, value.value.clone())?;
                Ok(value)
            } else {
                let span = ident.span;
                let value = self.env.get(ident)?;
                let spanned = SpannedValue {
                    value,
                    span,
                };
                self.munch_calls(spanned, tokens)
            }
        } else if let Some(this) = tokens.check(|t| t.kind == TokenKind::This) {
            if let Some((_dot, ident, _eq)) = eat_token3(
                tokens,
                Token::is_dot,
                Token::is_ident,
                Token::is_eq,
            ) {
                let value = self.expr(tokens)?;
                todo!("assign object property")
                // Ok((value, tail))
            } else {
                todo!("resolve self and continue munching calls")
            }
        } else if let Some(str) = tokens.check(|t| t.kind == TokenKind::String) {
            let value = Value::String(crate::lexer::unescape_string(&str.source).into());
            let spanned = SpannedValue {
                value,
                span: str.span,
            };
            self.munch_calls(spanned, tokens)
        } else if let Some(obj) = tokens.check(|t| t.kind == TokenKind::Object) {
            let name = tokens.expect(Token::is_ident, "identifier")?;
            tokens.expect(Token::is_left_brace, "`{`")?;
            let matchers = self.munch_object_contents(tokens)?;
            let muncher = compile_object_muncher(matchers, 0)?;
            let object = Value::Object(Rc::new(Object {
                name: name.source,
                properties: Default::default(),
                muncher,
            }));
            let spanned = SpannedValue {
                value: object,
                span: obj.span.union(tokens.prev_span()),
            };
            self.munch_calls(spanned, tokens)
        } else {
            println!("{:?}", tokens.peek());
            todo!("error: expected expression")
        }
    }

    fn munch_calls(&mut self, value: SpannedValue, tokens: &mut Tokens) -> Result<SpannedValue> {
        match tokens.peek() {
            Some(t) if !can_start_call(t) => return Ok(value),
            None => return Ok(value),
            _ => {}
        }
        let mut muncher = value.value.get_muncher();
        let env = Env::new();
        let block = loop {
            match muncher.munch(tokens, &env, self) {
                MunchOutput::Done { block } => {
                    break block;
                }
                MunchOutput::Continue { muncher: next } => {
                    muncher = next;
                }
                MunchOutput::Failed => {
                    todo_error("failed call");
                }
                MunchOutput::FailedEval { error } => return Err(error),
            }
        };
        let span = value.span.union(tokens.prev_span());
        let env = Env {
            inner: Rc::new(EnvInner {
                values: env.inner.values.clone(),
                next: match &*block {
                    Block::Source(s) => Some(s.closure.inner.clone()),
                    Block::Intrinsic(_) => None,
                },
            }),
        };
        let mut block_interp = Interpreter {
            env,
            intrinsics: self.intrinsics.clone(),
        };
        let returned_value = block_interp.block(block)?;
        let spanned = SpannedValue {
            value: returned_value,
            span,
        };
        self.munch_calls(spanned, tokens)
    }

    fn munch_object_contents(&mut self, tokens: &mut Tokens) -> Result<Vec<Matcher>> {
        let mut matchers = Vec::new();
        while !tokens.at(Token::is_right_brace) {
            let mut pattern = Vec::new();
            if !can_start_call(&tokens.peek().unwrap()) {
                todo!("error: can't start call with token {:?}", tokens.peek().unwrap().source);
            }
            while !tokens.at(Token::is_left_brace) && !tokens.at(Token::is_right_brace) {
                if let Some(_dollar) = tokens.check(Token::is_dollar) {
                    let bind = tokens.expect(Token::is_ident, "identifier")?;
                    tokens.expect(Token::is_colon, "`:`")?;
                    let kind = tokens.expect(Token::is_ident, "identifier")?;
                    pattern.push(Pattern::Var { kind, bind });
                } else {
                    pattern.push(Pattern::Token(tokens.check(|_| true).unwrap()));
                }
            }
            if tokens.at(Token::is_right_brace) {
                todo_error("method is missing its body");
            }
            tokens.advance();
            let body = self.munch_source_block(tokens);
            let body = Rc::new(Block::Source(body));
            matchers.push(Matcher { pattern, body });
        }
        tokens.advance();
        Ok(matchers)
    }

    fn munch_source_block(&mut self, tokens: &mut Tokens) -> SourceBlock {
        let mut contents = Vec::new();
        let mut depth = 0;
        while depth != 0 || !tokens.peek().unwrap().is_right_brace() {
            if tokens.at(Token::is_left_brace) {
                depth += 1;
            } else if tokens.at(Token::is_right_brace) {
                depth -= 1;
            }
            contents.push(tokens.advance());
        }
        tokens.advance();
        SourceBlock {
            closure: self.env.clone(),
            tokens: contents,
        }
    }
}

pub(crate) struct SpannedValue {
    pub(crate) value: Value,
    pub(crate) span: Span,
}

pub(crate) struct Tokens {
    tokens: Vec<Token>,
    idx: usize,
}

impl Tokens {
    pub(crate) fn check(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Token> {
        if self.at(pred) {
            Some(self.advance())
        } else {
            None
        }
    }

    pub(crate) fn advance(&mut self) -> Token {
        let token = self.tokens[self.idx].clone();
        self.idx += 1;
        token
    }

    pub(crate) fn at(&mut self, pred: impl FnOnce(&Token) -> bool) -> bool {
        match self.peek() {
            Some(tok) => pred(tok),
            _ => false,
        }
    }

    pub(crate) fn expect(&mut self, pred: impl FnOnce(&Token) -> bool, msg: &str) -> Result<Token> {
        if self.at(pred) {
            Ok(self.advance())
        } else {
            todo!("expected {}", msg)
        }
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.idx)
    }

    pub(crate) fn nth(&self, idx: usize) -> Option<&Token> {
        self.tokens.get(self.idx + idx)
    }

    fn prev_span(&self) -> Span {
        assert!(self.idx > 0);
        self.tokens[self.idx - 1].span
    }

    pub(crate) fn debug(&self, msg: &str) {
        println!("{}: {:?}", msg, self.tokens[self.idx..].iter().map(|t| &*t.source).collect::<Vec<_>>());
    }
}

fn compile_object_muncher(matchers: Vec<Matcher>, idx: usize) -> Result<Rc<dyn Muncher>> {
    if matchers.len() == 0 {
        return Ok(Rc::new(crate::muncher::NoMuncher));
    }
    let mut simple = Vec::new();
    let mut meta = Vec::new();
    let mut done = Vec::new();
    for matcher in matchers {
        match matcher.pattern.get(idx).cloned() {
            Some(Pattern::Var { kind, bind }) => {
                meta.push((kind, bind, matcher));
            }
            Some(Pattern::Token(tok)) => {
                simple.push((tok, matcher));
            }
            None => {
                done.push(matcher);
            }
        }
    }
    if done.len() > 1 {
        todo_error("multiple matchers are identical");
    }
    if done.len() > 0 && (simple.len() > 0 || meta.len() > 0) {
        todo_error("matcher is prefix of other matcher");
    }
    if done.len() > 0 {
        return Ok(Rc::new(crate::muncher::CompleteMuncher {
            block: done[0].body.clone(),
        }));
    }
    if simple.len() > 0 && meta.len() > 0 {
        todo_error("overlapping matchers");
    }
    if simple.len() > 0 {
        simple.sort_by(|a, b| a.0.source.cmp(&b.0.source));
        let mut group = Vec::<(Token, Matcher)>::new();
        let mut cases = HashMap::new();
        while let Some((tok, matcher)) = simple.pop() {
            if group.len() > 0 && tok.source != group[0].0.source {
                let source = group[0].0.source.clone();
                let group = compile_object_muncher(
                    group.drain(..).map(|(_, m)| m).collect(),
                    idx + 1,
                )?;
                cases.insert(source, group);
            }
            group.push((tok, matcher));
        }
        if group.len() > 0 {
            let source = group[0].0.source.clone();
            let group = compile_object_muncher(
                group.drain(..).map(|(_, m)| m).collect(),
                idx + 1,
            )?;
            cases.insert(source, group);
        }
        return Ok(Rc::new(crate::muncher::PlainMuncher { cases }));
    }
    if meta.iter().any(|(kind, _, _)| kind.source != meta[0].0.source) {
        todo_error("meta matchers branch out");
    }
    let bind = meta[0].1.clone();
    let cont = compile_object_muncher(meta.into_iter().map(|t| t.2).collect(), idx + 1)?;
    Ok(Rc::new(crate::muncher::ExprMuncher { bind, cont }))
}

#[derive(Clone)]
enum Pattern {
    Var { kind: Token, bind: Token },
    Token(Token),
}

struct Matcher {
    pattern: Vec<Pattern>,
    body: Rc<Block>,
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
    tokens: &mut Tokens,
    check1: impl FnOnce(&Token) -> bool,
    check2: impl FnOnce(&Token) -> bool,
    check3: impl FnOnce(&Token) -> bool,
) -> Option<(Token, Token, Token)> {
    match tokens.nth(0) {
        Some(t) if !check1(t) => return None,
        None => return None,
        _ => {}
    }
    match tokens.nth(1) {
        Some(t) if !check2(t) => return None,
        None => return None,
        _ => {}
    }
    match tokens.nth(2) {
        Some(t) if !check3(t) => return None,
        None => return None,
        _ => {}
    }
    let t1 = tokens.advance();
    let t2 = tokens.advance();
    let t3 = tokens.advance();
    Some((t1, t2, t3))
}

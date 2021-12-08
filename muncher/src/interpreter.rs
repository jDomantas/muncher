use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::{Value, Block, Intrinsic, SourceBlock, Result, Error, Intrinsics, Object};
use crate::lexer::{Token, TokenKind};
use crate::muncher::{MunchOutput, Muncher};

fn double_def(token: Token) -> Error {
    todo!("double def error")
}

fn undefined_var(token: Token) -> Error {
    todo!("undefined var error: {:?}", token)
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
            let (name, tail) = eat_token(tail, |t| t.kind == TokenKind::Ident)
                .ok_or_else(|| todo!("expected identifier"))?;
            let (_left_curly, tail) = eat_token(tail, Token::is_left_brace)
                .ok_or_else(|| todo!("expected left curly"))?;
            let (matchers, tail) = self.munch_object_contents(tail)?;
            let muncher = compile_object_muncher(matchers, 0)?;
            let object = Value::Object(Rc::new(Object {
                name: name.source,
                properties: Default::default(),
                muncher,
            }));
            Ok((object, tail))
        } else {
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
        let return_value = block_interp.block(block)?;
        self.munch_calls(return_value, tokens)
    }

    fn munch_object_contents<'src>(&mut self, mut tokens: &'src [Token]) -> Result<(Vec<Matcher>, &'src [Token])> {
        let mut matchers = Vec::new();
        while !tokens[0].is_right_brace() {
            let mut pattern = Vec::new();
            if !can_start_call(&tokens[0]) {
                todo!("can't start call with token {:?}", tokens[0].source);
            }
            while !tokens[0].is_left_brace() && !tokens[0].is_right_brace() {
                if tokens[0].is_dollar() {
                    let (bind, tail) = eat_token(tokens, |t| t.kind == TokenKind::Ident)
                        .ok_or_else(|| todo!("expected identifier"))?;
                    let (_colon, tail) = eat_token(tail, Token::is_colon)
                        .ok_or_else(|| todo!("expected colon"))?;
                    let (kind, tail) = eat_token(tail, |t| t.kind == TokenKind::Ident)
                        .ok_or_else(|| todo!("expected identifier"))?;
                    pattern.push(Pattern::Var { kind, bind });
                    tokens = tail;
                } else {
                    pattern.push(Pattern::Token(tokens[0].clone()));
                    tokens = &tokens[1..];
                }
            }
            if tokens[0].is_right_brace() {
                todo!("method is missing its body error");
            }
            let (body, rest) = self.munch_source_block(&tokens[1..]);
            tokens = rest;
            let body = Rc::new(Block::Source(body));
            matchers.push(Matcher { pattern, body });
        }
        Ok((matchers, &tokens[1..]))
    }

    fn munch_source_block<'src>(&mut self, mut tokens: &'src [Token]) -> (SourceBlock, &'src [Token]) {
        let mut contents = Vec::new();
        let mut depth = 0;
        while depth != 0 || !tokens[0].is_right_brace() {
            contents.push(tokens[0].clone());
            if tokens[0].is_left_brace() {
                depth += 1;
            } else if tokens[0].is_right_brace() {
                depth -= 1;
            }
            tokens = &tokens[1..];
        }
        let block = SourceBlock {
            closure: self.env.clone(),
            tokens: contents,
        };
        (block, &tokens[1..])
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
        todo!("multiple matchers are identical");
    }
    if done.len() > 0 && (simple.len() > 0 || meta.len() > 0) {
        todo!("matcher is prefix of other matcher");
    }
    if done.len() > 0 {
        return Ok(Rc::new(crate::muncher::CompleteMuncher {
            block: done[0].body.clone(),
        }));
    }
    if simple.len() > 0 && meta.len() > 0 {
        todo!("overlapping matchers");
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
    todo!("compile meta matchers");
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

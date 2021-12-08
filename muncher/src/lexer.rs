use logos::Logos;
use std::rc::Rc;
use crate::{Pos, Result, Span, Error, SourceBlock, interpreter::Env};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TokenKind {
    #[regex(r"[a-zA-Z0-9_]+")]
    Ident,
    #[regex(r#""([^\\"\n]|(\\["nrt\\]))*""#)]
    String,
    #[regex(r#"[^a-zA-Z0-9_" \t\r\n]"#)]
    Symbol,
    #[token("object")]
    Object,
    #[token("self")]
    This,
    #[token("let")]
    Let,
    #[regex(r"[ \t\r\n]+")]
    Whitespace,
    #[error]
    Error,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) span: Span,
    pub(crate) kind: TokenKind,
    pub(crate) source: Rc<str>,
}

impl Token {
    pub(crate) fn is_ident(&self) -> bool {
        self.kind == TokenKind::Ident
    }

    pub(crate) fn is_eq(&self) -> bool {
        &*self.source == "="
    }

    pub(crate) fn is_semi(&self) -> bool {
        &*self.source == ";"
    }

    pub(crate) fn is_dot(&self) -> bool {
        &*self.source == "."
    }

    pub(crate) fn is_left_paren(&self) -> bool {
        &*self.source == "(" // ) rainbow brackets!
    }

    pub(crate) fn is_right_paren(&self) -> bool {
        // ( rainbow brackets!
        &*self.source == ")"
    }

    pub(crate) fn is_left_brace(&self) -> bool {
        &*self.source == "{" // } rainbow brackets!
    }

    pub(crate) fn is_right_brace(&self) -> bool {
        // { rainbow brackets!
        &*self.source == "}"
    }

    pub(crate) fn is_dollar(&self) -> bool {
        &*self.source == "$"
    }

    pub(crate) fn is_colon(&self) -> bool {
        &*self.source == ":"
    }
}

fn lex(source: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut pos = Pos { line: 1, col: 1 };
    for (kind, span) in TokenKind::lexer(source).spanned() {
        let source = &source[span.clone()];
        let start = pos;
        for c in source.chars() {
            if c == '\n' {
                pos.line += 1;
                pos.col = 1;
            } else {
                pos.col += 1;
            }
        }
        let span = Span { start, end: pos };
        match kind {
            TokenKind::Whitespace => {}
            TokenKind::Error => return Err(Error {
                msg: "bad token".into(),
                span,
            }),
            _ => {
                tokens.push(Token {
                    span,
                    kind,
                    source: source.into(),
                });
            }
        }
    }
    Ok(tokens)
}

pub(crate) fn lex_program(source: &str) -> Result<Vec<Token>> {
    let tokens = lex(source)?;
    // TODO: verify that parentheses are balanced
    Ok(tokens)
}

pub(crate) fn unescape_string(str: &str) -> String {
    // strip quotes
    let str = &str[1..(str.len() - 1)];
    let mut res = String::new();
    let mut chars = str.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            res.push(match chars.next().unwrap() {
                '\\' => '\\',
                '"' => '"',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => unreachable!(),
            });
        } else {
            res.push(c);
        }
    }
    res
}

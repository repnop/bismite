use crate::{
    ast,
    token::{Token, TokenKind},
};
use logos::{Lexer, Logos};
use std::collections::VecDeque;

pub struct Parser<'source> {
    lexer: Lexer<TokenKind, &'source str>,
    token_queue: VecDeque<Token<'source>>,
}

type ParseResult<T> = Result<T, ParserError>;

impl<'parser, 'source: 'parser> Parser<'source> {
    pub fn new(input: &'source str) -> Parser<'source> {
        Self {
            lexer: TokenKind::lexer(input),
            token_queue: VecDeque::new(),
        }
    }

    fn eat(&'parser mut self, expected: TokenKind) -> Result<Token<'source>, TokenKind> {
        let tkn = self.next()?;

        if tkn.kind == expected {
            Ok(tkn)
        } else {
            Err(tkn.kind)
        }
    }

    fn next(&'parser mut self) -> Result<Token<'source>, TokenKind> {
        if self.token_queue.is_empty() {
            let kind = self.lexer.token;

            let ret = match kind {
                TokenKind::Error | TokenKind::Eof => Err(kind),
                kind => Ok(Token::new(kind, self.lexer.slice(), self.lexer.range())),
            };

            self.lexer.advance();

            ret
        } else {
            match self.token_queue.pop_front() {
                Some(tkn) => Ok(tkn),
                None => Err(TokenKind::Eof),
            }
        }
    }

    fn peek_n(&'parser mut self, n: usize) -> Result<Token<'source>, TokenKind> {
        let mut tokens = VecDeque::new();

        let loop_len = if n >= self.token_queue.len() {
            n - self.token_queue.len()
        } else {
            return Ok(self.token_queue[n]);
        };

        for _ in 0..loop_len {
            tokens.push_back(self.next()?);
        }

        self.token_queue.extend(tokens);
        Ok(*self.token_queue.back().unwrap())
    }

    fn peek(&'parser mut self) -> Result<Token<'source>, TokenKind> {
        self.peek_n(1)
    }

    fn peek2(&'parser mut self) -> Result<Token<'source>, TokenKind> {
        self.peek_n(2)
    }
}

/// Represents a parser error.
#[derive(Debug)]
pub enum ParserError {}

impl ::std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_test() {}
}

use crate::{
    ast,
    token::{Token, TokenKind},
};
use codespan::ByteSpan;
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

    pub fn parse(&'parser mut self) -> Result<ast::Decls<'source>, Token<'source>> {
        let mut decls = ast::Decls::new();

        loop {
            let peek = self.peek();

            if let Err(Token {
                kind: TokenKind::Eof,
                ..
            }) = peek
            {
                break;
            }

            let peek = peek?;

            match peek.kind {
                TokenKind::Struct => decls.push_struct(self.parse_struct()?),
                // TokenKind::Fn => decls.push_fn(self.parse_fn()?),
                _ => return Err(peek),
            }
        }

        Ok(decls)
    }

    fn parse_struct(&'parser mut self) -> Result<ast::StructDecl<'source>, Token<'source>> {
        let span_begin = self.eat(TokenKind::Struct)?.span;
        let ident = self.eat(TokenKind::Ident)?;

        self.eat(TokenKind::LBrace)?;

        let fields;

        match self.peek()? {
            Token {
                kind: TokenKind::Ident,
                ..
            } => {
                fields = self.parse_fields()?;
            }
            tkn => return Err(tkn),
        }

        let end_span = self.eat(TokenKind::RBrace)?.span;

        Ok(ast::StructDecl {
            ident,
            fields,
            span: ByteSpan::new(span_begin.start(), end_span.end()),
        })
    }

    fn parse_fields(&'parser mut self) -> Result<Vec<ast::FieldDecl<'source>>, Token<'source>> {
        let mut fields = Vec::new();

        loop {
            let peek = self.peek()?;

            match peek.kind {
                TokenKind::Ident => {
                    let ident = self.eat(TokenKind::Ident)?;
                    self.eat(TokenKind::Colon)?;
                    let field_type = self.eat(TokenKind::Ident)?;

                    if let TokenKind::Comma = self.peek()?.kind {
                        self.eat(TokenKind::Comma)?;
                    }

                    fields.push(ast::FieldDecl {
                        ident,
                        field_type,
                        span: ByteSpan::new(ident.span.start(), field_type.span.end()),
                    })
                }
                TokenKind::RBrace => break,
                _ => return Err(peek)?,
            }
        }

        Ok(fields)
    }

    fn eat(&'parser mut self, expected: TokenKind) -> Result<Token<'source>, Token<'source>> {
        let tkn = self.next()?;

        if tkn.kind == expected {
            Ok(tkn)
        } else {
            Err(tkn)
        }
    }

    fn next(&'parser mut self) -> Result<Token<'source>, Token<'source>> {
        if self.token_queue.is_empty() {
            let tkn = Token::new(self.lexer.token, self.lexer.slice(), self.lexer.range());

            let ret = match &tkn.kind {
                TokenKind::Error | TokenKind::Eof => Err(tkn),
                _ => Ok(tkn),
            };

            self.lexer.advance();

            ret
        } else {
            match self.token_queue.pop_front() {
                Some(tkn) => Ok(tkn),
                None => Err(Token::new(
                    TokenKind::Eof,
                    self.lexer.slice(),
                    self.lexer.range(),
                )),
            }
        }
    }

    fn peek_n(&'parser mut self, n: usize) -> Result<Token<'source>, Token<'source>> {
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

    fn peek(&'parser mut self) -> Result<Token<'source>, Token<'source>> {
        self.peek_n(1)
    }

    fn peek2(&'parser mut self) -> Result<Token<'source>, Token<'source>> {
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

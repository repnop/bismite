use crate::{
    ast,
    token::{Token, TokenKind},
};
use codespan::ByteSpan;
use lazy_static::lazy_static;
use logos::{Lexer, Logos};
use std::{collections::VecDeque, sync::RwLock};
use string_interner::{DefaultStringInterner, Sym};

lazy_static! {
    pub static ref GLOBAL_INTERNER: RwLock<DefaultStringInterner> =
        RwLock::new(DefaultStringInterner::new());
}

pub struct Parser<'source> {
    lexer: Lexer<TokenKind, &'source str>,
    token_queue: VecDeque<Token<'source>>,
}

type ParseResult<'a, T> = Result<T, Token<'a>>;

impl<'parser, 'source: 'parser> Parser<'source> {
    pub fn new(input: &'source str) -> Parser<'source> {
        Self {
            lexer: TokenKind::lexer(input),
            token_queue: VecDeque::new(),
        }
    }

    pub fn parse(&'parser mut self) -> ParseResult<'source, Vec<ast::Decls<'source>>> {
        let mut decls = Vec::new();

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
                TokenKind::Struct => decls.push(ast::Decls::Struct(self.parse_struct()?)),
                // TokenKind::Fn => decls.push_fn(self.parse_fn()?),
                _ => return Err(peek),
            }
        }

        Ok(decls)
    }

    fn parse_struct(&'parser mut self) -> ParseResult<'source, ast::StructDecl> {
        let span_begin = self.eat(TokenKind::Struct)?.span;
        let ident = self.parse_ident()?;

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

    fn parse_fields(&'parser mut self) -> ParseResult<'source, Vec<ast::FieldDecl>> {
        let mut fields = Vec::new();

        loop {
            let peek = self.peek()?;

            match peek.kind {
                TokenKind::Ident => {
                    let ident = self.parse_ident()?;
                    self.eat(TokenKind::Colon)?;
                    let field_type = self.parse_type()?;

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

    fn parse_ident(&'parser mut self) -> ParseResult<'source, ast::Ident> {
        let tkn = self.eat(TokenKind::Ident)?;

        let sym = GLOBAL_INTERNER.write().unwrap().get_or_intern(tkn.lit);

        Ok(ast::Ident {
            span: tkn.span,
            id: sym,
        })
    }

    fn parse_type(&'parser mut self) -> ParseResult<'source, ast::Type> {
        let peek = self.peek()?;

        let kind = match peek.kind {
            TokenKind::Ident | TokenKind::PathSeparator => {
                let path = self.parse_path()?;

                ast::TypeKind::Path(path)
            }
            TokenKind::LBracket => {
                self.eat(TokenKind::LBracket)?;
                let ty = self.parse_type()?;
                self.eat(TokenKind::Semicolon)?;
                self.parse_array()?
            }
            _ => return Err(peek),
        };

        Ok(ast::Type {
            span: ByteSpan::new(peek.span.start(), self.peek()?.span.end()),
            kind,
        })
    }

    fn parse_path(&'parser mut self) -> ParseResult<'source, ast::Path> {
        let mut peek = self.peek()?;
        let mut segments = Vec::new();

        if peek.kind == TokenKind::PathSeparator {
            self.eat(TokenKind::PathSeparator)?;
            peek = self.peek()?;
        }

        while peek.kind == TokenKind::Ident {
            let ident = self.parse_ident()?;
            segments.push(ast::PathSegment { ident });

            if !self.eat_optional(TokenKind::PathSeparator)?.is_some() {
                break;
            }

            peek = self.peek()?;
        }

        let span_start = segments[0].ident.span.start();
        let span_end = segments[segments.len() - 1].ident.span.end();

        Ok(ast::Path {
            span: ByteSpan::new(span_start, span_end),
            segments,
        })
    }

    fn parse_literal(&'parser mut self) -> ParseResult<'source, ast::LiteralKind> {
        use std::str::FromStr;

        match self.peek()?.kind {
            TokenKind::DecLit => Ok(ast::LiteralKind::Int(
                i128::from_str_radix(self.eat(TokenKind::DecLit)?.lit, 10).unwrap(),
            )),
            TokenKind::HexLit => Ok(ast::LiteralKind::Int(
                i128::from_str_radix(&self.eat(TokenKind::HexLit)?.lit[2..], 16).unwrap(),
            )),
            TokenKind::OctLit => Ok(ast::LiteralKind::Int(
                i128::from_str_radix(&self.eat(TokenKind::OctLit)?.lit[2..], 8).unwrap(),
            )),
            TokenKind::BinLit => Ok(ast::LiteralKind::Int(
                i128::from_str_radix(&self.eat(TokenKind::BinLit)?.lit[2..], 2).unwrap(),
            )),
            TokenKind::FloatLit => Ok(ast::LiteralKind::Float(
                f64::from_str(self.eat(TokenKind::BinLit)?.lit).unwrap(),
            )),
            TokenKind::FloatLit => Ok(ast::LiteralKind::Float(
                f64::from_str(self.eat(TokenKind::BinLit)?.lit).unwrap(),
            )),
        }
    }

    fn eat(&'parser mut self, expected: TokenKind) -> ParseResult<'source, Token<'source>> {
        let tkn = self.next()?;

        if tkn.kind == expected {
            Ok(tkn)
        } else {
            Err(tkn)
        }
    }

    fn eat_optional(
        &'parser mut self,
        expected: TokenKind,
    ) -> ParseResult<'source, Option<Token<'source>>> {
        let tkn = self.peek()?;

        if tkn.kind == expected {
            Ok(Some(self.next()?))
        } else {
            Ok(None)
        }
    }

    fn next(&'parser mut self) -> ParseResult<'source, Token<'source>> {
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

    fn peek_n(&'parser mut self, n: usize) -> ParseResult<'source, Token<'source>> {
        self.token_queue.clear();

        let loop_len = if n >= self.token_queue.len() {
            n - self.token_queue.len()
        } else {
            return Ok(self.token_queue[n]);
        };

        for _ in 0..loop_len {
            self.token_queue.push_back(self.next()?);
        }

        Ok(*self.token_queue.back().unwrap())
    }

    fn peek(&'parser mut self) -> ParseResult<'source, Token<'source>> {
        self.peek_n(1)
    }

    fn peek2(&'parser mut self) -> ParseResult<'source, Token<'source>> {
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

use std::iter::Peekable;

use ast::*;

use token::*;
use token_stream::*;

/// Uses a `TokenStream` to parse the source code.
#[derive(Debug)]
pub struct Parser<'a>(pub Peekable<TokenStream<'a>>);

impl<'a> Parser<'a> {
    /// Grammar starts here. Attempts to match on declarations and parses them.
    /// Returns an AST or errors.
    pub fn begin_parse(&mut self) -> Result<Decls<'a>, ParserError<'a>> {
        let mut decls = Decls::new();

        loop {
            // TODO: find a way to not need to clone?
            let token = self.0.peek().cloned();

            // If we have a token, match on it and see if it is valid.
            if let Some(tkn) = token {
                let tkn = tkn?;

                match tkn.kind {
                    // Structs and Fns are the only valid first set at the moment.
                    TokenKind::Struct => {
                        // Parse the inner declarations.
                        decls.push_struct(self.struct_declaration()?)
                    }
                    TokenKind::Fn => decls.push_fn(self.fn_declaration()?),
                    // Return an error because it isn't valid code.
                    _ => return Err(ParserError::UnexpectedToken(tkn, TokenKind::Fn)),
                };
            } else {
                // No declarations is valid, or we've finished all of them and hit eof.
                break;
            }
        }

        println!("{:#?}", decls);
        Ok(decls)
    }

    /// Parses a declaration, e.g. `struct Foo { bar: baz }` or `fn myfunc(a: b) -> c { }`
    pub fn struct_declaration(&mut self) -> Result<StructDecl<'a>, ParserError<'a>> {
        let span_begin = self.eat_match(TokenKind::Struct)?.span;

        let ident = self.eat_match(TokenKind::Ident)?;

        self.eat_match(TokenKind::LBrace)?;

        let mut fields = Vec::new();

        match self.peek_match(TokenKind::Ident) {
            Some(Token {
                kind: TokenKind::Ident,
                ..
            }) => {
                fields = self.field_declaration()?;
            }
            Some(_) | None => match self.0.peek() {
                Some(Ok(Token {
                    kind: TokenKind::RBrace,
                    ..
                })) => {}
                Some(Ok(t)) => {
                    return Err(ParserError::UnexpectedToken(t.clone(), TokenKind::Ident));
                }
                Some(Err(ref e)) => return Err(e.clone().into()),
                _ => return Err(ParserError::UnexpectedEOF),
            },
        };

        let end_span = self.eat_match(TokenKind::RBrace)?.span;

        Ok(StructDecl {
            ident,
            fields,
            span: ByteSpan::new(span_begin.start(), end_span.end()),
        })
    }

    /// Parses a declaration, e.g. `struct Foo { bar: baz }` or `fn myfunc(a: b) -> c { }`
    pub fn fn_declaration(&mut self) -> Result<FnDecl<'a>, ParserError<'a>> {
        let span_begin = self.eat_match(TokenKind::Fn)?.span;

        let ident = self.eat_match(TokenKind::Ident)?;

        self.eat_match(TokenKind::LParen)?;

        let arguments = self.field_declaration()?;

        self.eat_match(TokenKind::RParen)?;

        let return_type = if let Some(Ok(Token {
            kind: TokenKind::Arrow,
            ..
        })) = self.0.peek()
        {
            self.eat_match(TokenKind::Arrow)?;
            Some(self.eat_match(TokenKind::Ident)?)
        } else {
            None
        };

        self.eat_match(TokenKind::LBrace)?;

        let statements = self.statements()?;

        let end_span = self.eat_match(TokenKind::RBrace)?.span;

        Ok(FnDecl {
            ident,
            arguments,
            return_type,
            statements,
            span: ByteSpan::new(span_begin.start(), end_span.end()),
        })
    }

    /// Parses a field declaration, e.g. a struct field or function argument
    fn field_declaration(&mut self) -> Result<Vec<FieldDecl<'a>>, ParserError<'a>> {
        let mut fields = Vec::new();

        // `loop` to parse zero or more field declarations.
        loop {
            let token = self.0.peek().cloned();

            if let Some(tkn) = token {
                let tkn = tkn?;

                // Parse `<Ident>: <Ident> ','`
                match tkn.kind {
                    TokenKind::Ident => {
                        //println!("<Ident>");
                        let ident = self.eat_match(TokenKind::Ident)?;
                        self.eat_match(TokenKind::Colon)?;
                        let field_type = self.eat_match(TokenKind::Ident)?;

                        if let None = self.peek_match(TokenKind::Comma) {
                            if let Some(_) = self.peek_match(TokenKind::Ident) {
                                // If we encounter this case, we know the next token isn't valid
                                // so we'll get a proper error by just returning an empty Vec, for now.
                                return Ok(Vec::new());
                            }
                        } else {
                            self.eat_match(TokenKind::Comma)?;
                        }

                        let ident_span = ident.span.start();
                        let field_span = field_type.span.end();

                        fields.push(FieldDecl {
                            ident,
                            field_type,
                            span: ByteSpan::new(ident_span, field_span),
                        });
                    }
                    // Otherwise we're done parsing <FieldDecl>s
                    _ => {
                        //println!("<FieldDecl>");
                        return Ok(fields);
                    }
                }
            } else {
                break;
            }
        }

        //println!("<FieldDecl>");
        Ok(fields)
    }

    fn statements(&mut self) -> Result<Vec<StatementDecl<'a>>, ParserError<'a>> {
        let mut stmts = Vec::new();

        while let Some(Ok(t)) = self.0.peek().cloned() {
            match t.kind {
                TokenKind::Let => stmts.push(StatementDecl::VarDecl(self.var_declaration()?)),
                _ => break,
            }
        }

        Ok(stmts)
    }

    fn var_declaration(&mut self) -> Result<VarDecl<'a>, ParserError<'a>> {
        let start_span = self.eat_match(TokenKind::Let)?.span.start();

        let ident = self.eat_match(TokenKind::Ident)?;

        let var_type = if let Some(_) = self.peek_match(TokenKind::Colon) {
            self.eat_match(TokenKind::Colon)?;
            Some(self.eat_match(TokenKind::Ident)?)
        } else {
            None
        };

        self.eat_match(TokenKind::Assign)?;

        let expr = self.expression()?;

        let end_span = self.eat_match(TokenKind::Semicolon)?.span.end();

        Ok(VarDecl {
            ident,
            var_type,
            value: expr,
            span: ByteSpan::new(start_span, end_span),
        })
    }

    fn expression(&mut self) -> Result<Expression, ParserError<'a>> {
        let expr = if let Some(Ok(_)) = self.0.peek().cloned() {
            let tkn = self.0.next().unwrap().unwrap();
            match tkn.kind {
                intlit @ TokenKind::IntLit => Expression::Literal(intlit),
                _ => Expression::Other,
            }
        } else {
            return Err(ParserError::UnexpectedEOF);
        };

        Ok(expr)
    }

    /// Consumes the next token, returning it on success when comparing, otherwise
    /// returns a `ParseError`
    fn eat_match(&mut self, tk: TokenKind) -> Result<Token<'a>, ParserError<'a>> {
        // Match the next token (returning an error if we're eof).
        let token = match self.0.next() {
            Some(tkn) => tkn,
            None => return Err(ParserError::UnexpectedEOF),
        }?;

        // Compare to the expected `TokenKind`
        if token.kind == tk {
            //println!("Matched {:?}", token.kind);
            Ok(token)
        } else {
            Err(ParserError::UnexpectedToken(token, tk))
        }
    }

    /// Peeks at the next token, returning `None` if the token types are different.
    fn peek_match(&mut self, tk: TokenKind) -> Option<Token<'a>> {
        // TODO: fix unwrap
        let token = match self.0.peek().unwrap() {
            Ok(tkn) => tkn,
            Err(_) => return None,
        };

        if token.kind == tk {
            Some(token.clone())
        } else {
            None
        }
    }

    //fn make_error(&mut self, err: NumberedError)
}

/// Represents a parser error.
#[derive(Debug)]
pub enum ParserError<'a> {
    /// Generic, placeholder error for now.
    InvalidToken(TokenError),
    /// Error for finding an unexpected token type.
    // TODO: Add what it expected.
    UnexpectedToken(Token<'a>, TokenKind),
    /// Unexpectedly hit end of file.
    UnexpectedEOF,
}

impl<'a> ::std::fmt::Display for ParserError<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::ParserError::*;

        match self {
            &InvalidToken(ref te) => write!(f, "{}", te),
            // TODO: write actual display implementation for tokens.
            &UnexpectedToken(ref t, ref k) => write!(
                f,
                "Unexpected token: found {}, expected {}",
                t.kind.name(),
                k.name()
            ),
            UnexpectedEOF => write!(f, "Unexpectedly hit end of file."),
        }
    }
}

impl<'a> From<TokenError> for ParserError<'a> {
    fn from(te: TokenError) -> ParserError<'a> {
        ParserError::InvalidToken(te)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_test() {
        let code = "struct Foo { bar: baz, }";
        let mut parser = Parser(TokenStream::new(code).peekable());
        let result = parser.begin_parse();
        assert!(result.is_ok(), format!("{:?}", result));
    }
}

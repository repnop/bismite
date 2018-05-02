use std::iter::Peekable;
use token::*;
use token_stream::*;

/// Uses a `TokenStream` to parse the source code.
#[derive(Debug)]
pub struct Parser<'a>(pub Peekable<TokenStream<'a>>);

impl<'a> Parser<'a> {
    /// Grammar starts here. Attempts to match on declarations and parses them.
    /// Returns an AST or errors.
    pub fn begin_parse(&mut self) -> Result<Ast, ParserError> {
        loop {
            // TODO: find a way to not need to clone?
            let token = self.0.peek().cloned();

            // If we have a token, match on it and see if it is valid.
            if let Some(tkn) = token {
                let tkn = tkn?;

                match tkn.kind {
                    // Structs and Fns are the only valid first set at the moment.
                    TokenKind::Keyword(Keyword::Struct) | TokenKind::Keyword(Keyword::Fn) => {
                        // Parse the inner declarations.
                        self.declaration()?
                    }
                    // Return an error because it isn't valid code.
                    _ => {
                        return Err(ParserError::UnexpectedToken(
                            tkn,
                            TokenKind::Keyword(Keyword::Fn),
                        ))
                    }
                };
            } else {
                // No declarations is valid, or we've finished all of them and hit eof.
                break;
            }
        }

        //println!("<Begin>");
        Ok(Ast::Placeholder("<Begin>"))
    }

    /// Parses a declaration, e.g. `struct Foo { bar: baz }` or `fn myfunc(a: b) -> c { }`
    pub fn declaration(&mut self) -> Result<Ast, ParserError> {
        // TODO: find a way to not need to clone?
        let token = self.0.peek().cloned();

        if let Some(tkn) = token {
            let tkn = tkn?;

            match tkn.kind {
                // Parse a struct declaration.
                TokenKind::Keyword(Keyword::Struct) => {
                    //println!("<StructDecl>");

                    // Match `struct <Ident> '{' <FieldDecl>* '}'`
                    self.eat_match(TokenKind::Keyword(Keyword::Struct))?;
                    self.eat_match(TokenKind::Ident(String::new()))?;
                    self.eat_match(TokenKind::Symbol(Symbol::LBrace))?;

                    match self.peek_match(TokenKind::Ident(String::new())) {
                        Some(Token {
                            kind: TokenKind::Ident(_),
                            ..
                        }) => {
                            self.field_declaration()?;
                        }
                        Some(_) | None => match self.0.peek() {
                            Some(Ok(Token {
                                kind: TokenKind::Symbol(Symbol::RBrace),
                                ..
                            })) => {}
                            Some(Ok(t)) => {
                                return Err(ParserError::UnexpectedToken(
                                    t.clone(),
                                    TokenKind::Ident(String::new()),
                                ));
                            }
                            Some(Err(ref e)) => return Err(e.clone().into()),
                            _ => return Err(ParserError::UnexpectedEOF),
                        },
                    };

                    self.eat_match(TokenKind::Symbol(Symbol::RBrace))?;
                }
                // Parse a function declaration.
                TokenKind::Keyword(Keyword::Fn) => {
                    //println!("<FnDecl>");
                    self.eat_match(TokenKind::Keyword(Keyword::Fn))?;
                }
                // Invalid declaration.
                _ => {
                    return Err(ParserError::UnexpectedToken(
                        tkn,
                        TokenKind::Keyword(Keyword::Fn),
                    ))
                }
            };

            Ok(Ast::Placeholder("<Decl>"))
        } else {
            Ok(Ast::Placeholder("<Decl>"))
        }
    }

    /// Parses a field declaration, e.g. a struct field or function argument
    fn field_declaration(&mut self) -> Result<Ast, ParserError> {
        // `loop` to parse zero or more field declarations.
        loop {
            let token = self.0.peek().cloned();

            if let Some(tkn) = token {
                let tkn = tkn?;

                // Parse `<Ident>: <Ident> ','`
                match tkn.kind {
                    TokenKind::Ident(_) => {
                        //println!("<Ident>");
                        self.eat_match(TokenKind::Ident(String::new()))?;
                        self.eat_match(TokenKind::Symbol(Symbol::Colon))?;
                        self.eat_match(TokenKind::Ident(String::new()))?;
                        self.eat_match(TokenKind::Symbol(Symbol::Comma))?;
                    }
                    // Otherwise we're done parsing <FieldDecl>s
                    _ => {
                        //println!("<FieldDecl>");
                        return Ok(Ast::Placeholder("<FieldDecl>"));
                    }
                }
            } else {
                break;
            }
        }

        //println!("<FieldDecl>");
        Ok(Ast::Placeholder("<FieldDecl>"))
    }

    /// Consumes the next token, returning it on success when comparing, otherwise
    /// returns a `ParseError`
    fn eat_match(&mut self, tk: TokenKind) -> Result<TokenKind, ParserError> {
        // Match the next token (returning an error if we're eof).
        let token = match self.0.next() {
            Some(tkn) => tkn,
            None => return Err(ParserError::UnexpectedEOF),
        }?;

        // Compare to the expected `TokenKind`
        if token.kind == tk {
            //println!("Matched {:?}", token.kind);
            Ok(token.kind)
        } else {
            Err(ParserError::UnexpectedToken(token, tk))
        }
    }

    /// Peeks at the next token, returning `None` if the token types are different.
    fn peek_match(&mut self, tk: TokenKind) -> Option<Token> {
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
}

/// Represents a parser error.
#[derive(Debug)]
pub enum ParserError {
    /// Generic, placeholder error for now.
    InvalidToken(TokenError),
    /// Error for finding an unexpected token type.
    // TODO: Add what it expected.
    UnexpectedToken(Token, TokenKind),
    /// Unexpectedly hit end of file.
    UnexpectedEOF,
}

impl ::std::fmt::Display for ParserError {
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

impl From<TokenError> for ParserError {
    fn from(te: TokenError) -> ParserError {
        ParserError::InvalidToken(te)
    }
}

/// Placeholder AST structure.
#[derive(Debug)]
pub enum Ast {
    /// Placeholder.
    Placeholder(&'static str),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_test() {
        let code = "struct Foo { bar: baz, }";
        let mut parser = Parser(TokenStream::new(code).peekable());

        assert!(parser.begin_parse().is_ok());
    }
}

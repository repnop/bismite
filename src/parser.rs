use std::iter::Peekable;

use ast::*;

use token::*;
use token_stream::*;

type ParseResult<'a, T> = Result<T, ParserError<'a>>;

/// Uses a `TokenStream` to parse the source code.
#[derive(Debug)]
pub struct Parser<'a>(pub Peekable<TokenStream<'a>>);

impl<'a> Parser<'a> {
    /// Grammar starts here. Attempts to match on declarations and parses them.
    /// Returns an AST or errors.
    pub fn begin_parse(&mut self) -> ParseResult<'a, Decls<'a>> {
        let mut decls = Decls::new();

        loop {
            // TODO: find a way to not need to clone?
            let token = self.0.peek().map(|&r| r);

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

        //println!("{:#?}", decls);
        Ok(decls)
    }

    /// Parses a declaration, e.g. `struct Foo { bar: baz }` or `fn myfunc(a: b) -> c { }`
    pub fn struct_declaration(&mut self) -> ParseResult<'a, StructDecl<'a>> {
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
    pub fn fn_declaration(&mut self) -> ParseResult<'a, FnDecl<'a>> {
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
    fn field_declaration(&mut self) -> ParseResult<'a, Vec<FieldDecl<'a>>> {
        let mut fields = Vec::new();

        // `loop` to parse zero or more field declarations.
        loop {
            let token = self.0.peek().map(|&r| r);

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

    fn statements(&mut self) -> ParseResult<'a, Vec<StatementDecl<'a>>> {
        let mut stmts = Vec::new();

        while let Some(Ok(t)) = self.0.peek().map(|&r| r) {
            match t.kind {
                TokenKind::Let => stmts.push(StatementDecl::VarDecl(self.var_declaration()?)),
                _ => break,
            }
        }

        Ok(stmts)
    }

    fn var_declaration(&mut self) -> ParseResult<'a, VarDecl<'a>> {
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

    fn expression(&mut self) -> ParseResult<'a, Expression<'a>> {
        Ok(self.tier_four_expr()?)
    }

    fn parenthesized_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        self.eat_match(TokenKind::LParen)?;
        let expr = self.expression()?;
        self.eat_match(TokenKind::RParen)?;

        Ok(expr)
    }

    fn unary_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        if let Some(_) = self.peek_group(&TIER1_OPS) {
            let op = self.eat_group(&TIER1_OPS)?;
            let expr = self.expression()?;

            Ok(Expression::Unary(op, Box::new(expr)))
        } else {
            Ok(self.base_expr()?)
        }
    }

    fn base_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        if let Some(token) = self.peek_group(&TIER0_KINDS) {
            return match token.kind {
                TokenKind::LParen => Ok(self.parenthesized_expr()?),
                TokenKind::IntLit => Ok({
                    let tkn = self.eat_match(TokenKind::IntLit).unwrap();
                    Expression {
                        ty: Type {
                            span: tkn.span,
                            kind,
                        },
                        kind: ExpressionKind::Literal(Literal {
                            token: tkn,
                            kind: LiteralKind::Integer(u128::from_str_radix(tkn.lit, 10).map_err(
                                |_| ParserError::InvalidToken(TokenError::InvalidLiteral(tkn.span)),
                            )?),
                        }),
                    }
                }),
                TokenKind::Ident => Ok(self.parse_ident()?),
                _ => Err(self.eat_group(&TIER0_KINDS).unwrap_err()),
            };
        }

        Err(self.eat_group(&TIER0_KINDS).unwrap_err())
    }

    fn tier_two_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        let mut expr = self.unary_expr()?;

        if let Some(_) = self.peek_group(&TIER2_OPS) {
            let op = self.eat_group(&TIER2_OPS)?;
            expr = Expression::Binary(Box::new(expr), op, Box::new(self.expression()?));
        }

        Ok(expr)
    }

    fn tier_three_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        let mut expr = self.tier_two_expr()?;

        if let Some(_) = self.peek_group(&TIER3_OPS) {
            let op = self.eat_group(&TIER3_OPS)?;
            expr = Expression::Binary(Box::new(expr), op, Box::new(self.expression()?));
        }

        Ok(expr)
    }

    fn tier_four_expr(&mut self) -> ParseResult<'a, Expression<'a>> {
        let mut expr = self.tier_three_expr()?;

        if let Some(_) = self.peek_group(&TIER4_OPS) {
            let op = self.eat_group(&TIER4_OPS)?;
            expr = Expression::Binary(Box::new(expr), op, Box::new(self.expression()?));
        }

        Ok(expr)
    }

    fn parse_ident(&mut self) -> ParseResult<'a, Expression<'a>> {
        let ident = self.eat_match(TokenKind::Ident)?;

        Ok(if let Some(Ok(tkn)) = self.0.peek().map(|&r| r) {
            match tkn.kind {
                TokenKind::LParen => Expression::FnCall(ident, self.parse_fn_call()?),
                //TokenKind::Period => Expression::
                _ => Expression::Identifier(ident),
            }
        } else {
            Expression::Identifier(ident)
        })
    }

    fn parse_fn_call(&mut self) -> ParseResult<'a, Vec<Expression<'a>>> {
        self.eat_match(TokenKind::LParen)?;

        let mut exprs = Vec::new();

        while let None = self.peek_match(TokenKind::RParen) {
            exprs.push(self.expression()?);

            if let None = self.peek_match(TokenKind::Comma) {
                break;
            } else {
                self.eat_match(TokenKind::Comma)?;
            }
        }

        self.eat_match(TokenKind::RParen)?;

        Ok(exprs)
    }

    //fn parse_ident_with_optional_fields(&mut self) ->

    /// Consumes the next token, returning it on success when comparing, otherwise
    /// returns a `ParseError`
    fn eat_match(&mut self, tk: TokenKind) -> ParseResult<'a, Token<'a>> {
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

    fn eat_group(&mut self, kinds: &'a [TokenKind]) -> ParseResult<'a, Token<'a>> {
        let token = match self.0.next() {
            Some(tkn) => tkn,
            None => return Err(ParserError::UnexpectedEOF),
        }?;

        if kinds.contains(&token.kind) {
            Ok(token)
        } else {
            Err(ParserError::UnexpectedInGroup(token, kinds))
        }
    }

    /// Peeks at the next token, returning `None` if the token types are different.
    fn peek_match(&mut self, tk: TokenKind) -> Option<Token<'a>> {
        let token = match self.0.peek() {
            Some(Ok(tkn)) => tkn,
            _ => return None,
        };

        if token.kind == tk {
            Some(token.clone())
        } else {
            None
        }
    }

    fn peek_group(&mut self, kinds: &'a [TokenKind]) -> Option<Token<'a>> {
        let token = match self.0.peek() {
            Some(Ok(tkn)) => tkn,
            _ => return None,
        };

        if kinds.contains(&token.kind) {
            Some(token.clone())
        } else {
            None
        }
    }
}

/// Represents a parser error.
#[derive(Debug)]
pub enum ParserError<'a> {
    /// Generic, placeholder error for now.
    InvalidToken(TokenError<'a>),
    /// Error for finding an unexpected token type.
    // TODO: Add what it expected.
    UnexpectedToken(Token<'a>, TokenKind),
    UnexpectedInGroup(Token<'a>, &'a [TokenKind]),
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
            &UnexpectedInGroup(ref tkn, ref kinds) => {
                write!(
                    f,
                    "Unexpected token: found {}, expected one of the following: ",
                    tkn.kind.name()
                )?;
                for kind in kinds.iter() {
                    write!(f, "{}, ", kind.name())?;
                }
                Ok(())
            }
            UnexpectedEOF => write!(f, "Unexpectedly hit end of file."),
        }
    }
}

impl<'a> From<TokenError<'a>> for ParserError<'a> {
    fn from(te: TokenError<'a>) -> ParserError<'a> {
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

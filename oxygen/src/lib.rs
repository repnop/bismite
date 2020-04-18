pub mod ast;
mod lexer;

pub use lexer::{Token, TokenKind};

use ast::*;
use codespan::Span;
use logos::Lexer;
use std::collections::VecDeque;

pub type Result<T> = std::result::Result<T, ParseError>;

pub enum Either<T, U> {
    Left(T),
    Right(U),
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Eof,
    BadToken(Token),
    BadBinOp,
    Fucc,
}

pub struct Parser<'a> {
    lexer: Lexer<'a, TokenKind>,
    peeks: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            peeks: VecDeque::new(),
        }
    }

    pub fn guess(&mut self) -> Result<Option<AstNode>> {
        if self.peek() == Err(ParseError::Eof) {
            return Ok(None);
        }

        match self.peek()?.kind {
            _ => match self.statement_or_expression()? {
                Either::Left(stmt) => Ok(Some(AstNode::Statement(stmt))),
                Either::Right(expr) => Ok(Some(AstNode::Expression(expr))),
            },
        }
    }

    pub fn statement_or_expression(&mut self) -> Result<Either<Statement, Expression>> {
        match self.peek()?.kind {
            TokenKind::Let => Ok(Either::Left(self.statement()?)),
            _ => {
                let expr = self.expression()?;

                if self.peek().map(|t| t.kind) == Ok(TokenKind::Semicolon) {
                    self.eat(TokenKind::Semicolon)?;
                    Ok(Either::Left(Statement::Expression(expr)))
                } else {
                    Ok(Either::Right(expr))
                }
            }
        }
    }

    pub fn statement(&mut self) -> Result<Statement> {
        match self.peek()?.kind {
            TokenKind::Let => Ok(Statement::VariableBinding(self.variable_binding()?)),
            TokenKind::Identifier(_) => {
                let expr = self.expression()?;
                self.eat(TokenKind::Semicolon)?;
                Ok(Statement::Expression(expr))
            }
            _ => todo!(),
        }
    }

    pub fn variable_binding(&mut self) -> Result<VariableBinding> {
        let let_span = self.eat(TokenKind::Let)?;

        let mutable = if self.peek()?.kind == TokenKind::Mut {
            self.eat(TokenKind::Mut)?;
            true
        } else {
            false
        };

        let name = self.identifier()?;

        let ty = if self.peek()?.kind == TokenKind::Colon {
            self.eat(TokenKind::Colon)?;
            Some(self.ty()?)
        } else {
            None
        };

        self.eat(TokenKind::Eq)?;
        let value = self.expression()?;
        let end = self.eat(TokenKind::Semicolon)?;

        Ok(VariableBinding {
            mutable,
            name,
            ty,
            value,
            span: let_span.merge(end),
        })
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.inner_expr(None)
    }

    fn inner_expr(&mut self, curr_binop: Option<BinOp>) -> Result<Expression> {
        let mut primary = self.parse_primary_expr()?;

        loop {
            if self.peek().is_err() {
                return Ok(primary);
            }

            match self.peek()?.kind {
                t if t.is_binop() => {
                    let binop = self.binop()?;

                    if matches!(curr_binop, Some(curr) if curr != binop) {
                        return Err(ParseError::BadBinOp);
                    }

                    let rhs = self.parse_primary_expr()?;
                    let span = primary.span.merge(rhs.span);

                    primary = Expression {
                        kind: ExpressionKind::BinaryOperation(
                            Box::new(primary),
                            binop,
                            Box::new(rhs),
                        ),
                        span,
                    };
                }
                TokenKind::Period => {
                    self.eat(TokenKind::Period)?;
                    let ident = self.identifier()?;
                    let span = primary.span.merge(ident.span);

                    primary = Expression {
                        kind: ExpressionKind::FieldAccess(Box::new(primary), ident),
                        span,
                    };
                }
                TokenKind::Eq => {
                    self.eat(TokenKind::Eq)?;
                    let rhs = self.expression()?;
                    let span = primary.span.merge(rhs.span);

                    return Ok(Expression {
                        kind: ExpressionKind::Assignment(Box::new(primary), Box::new(rhs)),
                        span,
                    });
                }
                TokenKind::LeftParen => {
                    let mut exprs = Vec::new();
                    self.eat(TokenKind::LeftParen)?;

                    while self.peek()?.kind != TokenKind::RightParen {
                        exprs.push(self.expression()?);

                        if self.peek()?.kind == TokenKind::Comma {
                            self.eat(TokenKind::Comma)?;
                        }
                    }

                    let end_span = self.eat(TokenKind::RightParen)?;
                    let start_span = primary.span;

                    primary = Expression {
                        kind: ExpressionKind::FnCall(Box::new(primary), exprs),
                        span: start_span.merge(end_span),
                    };
                }
                _ => return Ok(primary),
            }
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expression> {
        let token = self.token()?;
        let span = token.span();

        match token.kind {
            TokenKind::Integer(n) => Ok(Expression {
                kind: ExpressionKind::Integer(n),
                span,
            }),
            TokenKind::LeftParen => {
                let mut expr = self.expression()?;
                let end_span = self.eat(TokenKind::RightParen)?;
                expr.span = span.merge(end_span);

                Ok(expr)
            }
            TokenKind::Identifier(value) => Ok(Expression {
                kind: ExpressionKind::Identifier(Identifier { value, span }),
                span,
            }),
            _ => Err(ParseError::BadToken(token)),
        }
    }

    pub fn ty(&mut self) -> Result<Type> {
        let token = self.token()?;
        let span = token.span();

        match token.kind {
            TokenKind::Identifier(s) => Ok(Type {
                kind: TypeKind::Named(s),
                span,
            }),
            _ => Err(ParseError::Fucc),
        }
    }

    pub fn identifier(&mut self) -> Result<Identifier> {
        let token = self.token()?;
        let span = token.span();

        match token.kind {
            TokenKind::Identifier(value) => Ok(Identifier { value, span }),
            _ => Err(ParseError::BadToken(token)),
        }
    }

    pub fn eat(&mut self, kind: TokenKind) -> Result<Span> {
        let token = self.token()?;

        if token.kind() == &kind {
            Ok(token.span())
        } else {
            Err(ParseError::Fucc)
        }
    }

    pub fn peek(&mut self) -> Result<Token> {
        match self.peeks.front() {
            Some(tkn) => Ok(tkn.clone()),
            None => {
                let token = self.token()?;
                self.peeks.push_front(token.clone());
                Ok(token)
            }
        }
    }

    pub fn peek2(&mut self) -> Result<Token> {
        match self.peeks.back() {
            Some(tkn) if self.peeks.len() == 2 => Ok(tkn.clone()),
            _ => {
                for _ in 0..(2 - self.peeks.len()) {
                    loop {
                        let token = self.lexer.next().ok_or(ParseError::Eof)?;

                        if token == TokenKind::Whitespace {
                            continue;
                        }

                        let span = self.lexer.span();
                        let span = Span::new(span.start as u32, span.end as u32);
                        self.peeks.push_back(Token::new(span, token));
                        break;
                    }
                }

                Ok(self.peeks.back().cloned().unwrap())
            }
        }
    }

    pub fn binop(&mut self) -> Result<BinOp> {
        let token = self.token()?;

        match token.kind {
            TokenKind::Plus => Ok(BinOp::Plus),
            TokenKind::Minus => Ok(BinOp::Minus),
            TokenKind::Star => Ok(BinOp::Mult),
            TokenKind::Slash => Ok(BinOp::Divide),
            _ => Err(ParseError::BadToken(token)),
        }
    }

    pub fn token(&mut self) -> Result<Token> {
        match self.peeks.pop_front() {
            Some(tkn) => Ok(tkn),
            None => loop {
                let token = self.lexer.next().ok_or(ParseError::Eof)?;

                if token == TokenKind::Whitespace {
                    continue;
                }

                let span = self.lexer.span();
                let span = Span::new(span.start as u32, span.end as u32);

                break Ok(Token::new(span, token));
            },
        }
    }
}

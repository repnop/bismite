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
        let peek2 = self.peek2().map(|t| t.kind);
        match self.peek()?.kind {
            TokenKind::Let => Ok(Either::Left(self.statement()?)),
            TokenKind::Identifier(_) if peek2 == Ok(TokenKind::Eq) => {
                Ok(Either::Left(self.statement()?))
            }
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
        let peek2 = self.peek2()?.kind;
        match self.peek()?.kind {
            TokenKind::Let => Ok(Statement::VariableBinding(self.variable_binding()?)),
            TokenKind::Identifier(_) if peek2 == TokenKind::Eq => {
                Ok(Statement::Assignment(self.assignment()?))
            }
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
        let name = self.identifier()?.0;
        self.eat(TokenKind::Colon)?;
        let ty = self.ty()?;
        self.eat(TokenKind::Eq)?;
        let value = self.expression()?;
        let end = self.eat(TokenKind::Semicolon)?;

        Ok(VariableBinding {
            name,
            ty,
            value,
            span: let_span.merge(end),
        })
    }

    pub fn assignment(&mut self) -> Result<Assignment> {
        let (ident, start_span) = self.identifier()?;
        self.eat(TokenKind::Eq)?;
        let value = self.expression()?;
        let end_span = self.eat(TokenKind::Semicolon)?;

        Ok(Assignment {
            ident,
            value,
            span: start_span.merge(end_span),
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

            if self.peek()?.is_binop() {
                let binop = self.binop()?;

                if matches!(curr_binop, Some(curr) if curr != binop) {
                    return Err(ParseError::BadBinOp);
                }

                let rhs = self.parse_primary_expr()?;
                let span = primary.span.merge(rhs.span);

                primary = Expression {
                    kind: ExpressionKind::BinaryOperation(Box::new(primary), binop, Box::new(rhs)),
                    span,
                };
            } else if self.peek()?.kind == TokenKind::LeftParen {
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
            } else {
                return Ok(primary);
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
                let expr = self.expression()?;
                self.eat(TokenKind::RightParen)?;

                Ok(expr)
            }
            TokenKind::Identifier(i) => Ok(Expression {
                kind: ExpressionKind::Identifier(i),
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

    pub fn identifier(&mut self) -> Result<(String, Span)> {
        let token = self.token()?;
        let span = token.span();

        match token.kind {
            TokenKind::Identifier(s) => Ok((s, span)),
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

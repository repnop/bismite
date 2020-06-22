#![allow(clippy::match_bool)]

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
    BadToken { got: Token, expected: Vec<&'static str> },
    BadBinOp,
}

pub struct Parser<'a> {
    lexer: Lexer<'a, TokenKind>,
    peeks: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { lexer: Lexer::new(input), peeks: VecDeque::new() }
    }

    pub fn guess(&mut self) -> Result<Option<AstNode>> {
        if self.peek() == Err(ParseError::Eof) {
            return Ok(None);
        }

        match self.peek()?.kind {
            TokenKind::Fn | TokenKind::Struct | TokenKind::Module | TokenKind::Use => {
                Ok(Some(AstNode::Item(self.item()?)))
            }
            _ => match self.statement_or_expression()? {
                Either::Left(stmt) => Ok(Some(AstNode::Statement(stmt))),
                Either::Right(expr) => Ok(Some(AstNode::Expression(expr))),
            },
        }
    }

    pub fn geode(&mut self) -> Result<Geode> {
        let module = self.module(true)?;
        let span = module.span;

        Ok(Geode { module, span })
    }

    pub fn module(&mut self, implicit: bool) -> Result<Module> {
        let (start_span, name) = match implicit {
            true => match self.peek() {
                Ok(token) => (token.span(), Identifier::dummy()),
                Err(ParseError::Eof) => {
                    return Ok(Module { name: Identifier::dummy(), items: Vec::new(), span: Span::new(0, 0) })
                }
                Err(e) => return Err(e),
            },
            false => {
                let start = self.eat(TokenKind::Module)?;
                let name = self.identifier()?;
                self.eat(TokenKind::LeftBrace)?;

                (start, name)
            }
        };

        let mut items = Vec::new();

        loop {
            match implicit {
                true => {
                    if let Err(ParseError::Eof) = self.peek() {
                        break;
                    }
                }
                false if self.peek()?.kind == TokenKind::RightBrace => break,
                _ => {}
            }

            items.push(self.item()?);
        }

        let end_span = match implicit {
            true => items
                .last()
                .map(|i| i.span())
                .unwrap_or_else(|| Span::new(start_span.start(), start_span.end() + codespan::ByteOffset::from(1))),
            false => self.eat(TokenKind::RightBrace)?,
        };

        let span = start_span.merge(end_span);

        Ok(Module { name, items, span })
    }

    pub fn item(&mut self) -> Result<Item> {
        match self.peek()?.kind {
            TokenKind::Fn => Ok(Item::Function(self.function()?)),
            TokenKind::Struct => Ok(Item::Struct(self.r#struct()?)),
            TokenKind::Module => Ok(Item::Module(self.module(false)?)),
            TokenKind::Use => Ok(Item::Use(self.usage()?)),
            _ => Err(ParseError::BadToken { got: self.peek()?, expected: vec!["fn", "struct", "module", "use"] }),
        }
    }

    pub fn function(&mut self) -> Result<Function> {
        let start_span = self.eat(TokenKind::Fn)?;
        let name = self.identifier()?;

        self.eat(TokenKind::LeftParen)?;

        let parameters = self.list(Self::function_parameter, TokenKind::RightParen)?;
        self.eat(TokenKind::RightParen)?;

        let return_ty = if self.peek()?.kind == TokenKind::ThinArrow {
            self.eat(TokenKind::ThinArrow)?;

            Some(self.ty()?)
        } else {
            None
        };

        let body = self.block()?;
        let span = start_span.merge(body.span);

        Ok(Function { name, parameters, return_ty, body, span })
    }

    pub fn function_parameter(&mut self) -> Result<FunctionParameter> {
        let name = self.identifier()?;
        self.eat(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.merge(ty.span);

        Ok(FunctionParameter { name, ty, span })
    }

    pub fn r#struct(&mut self) -> Result<Struct> {
        let start_span = self.eat(TokenKind::Struct)?;
        let name = self.identifier()?;
        self.eat(TokenKind::LeftBrace)?;
        let members = self.list(Self::struct_member, TokenKind::RightBrace)?;
        let end_span = self.eat(TokenKind::RightBrace)?;
        let span = start_span.merge(end_span);

        Ok(Struct { name, members, span })
    }

    pub fn struct_member(&mut self) -> Result<StructMember> {
        let name = self.identifier()?;
        self.eat(TokenKind::Colon)?;
        let ty = self.ty()?;
        let span = name.span.merge(ty.span);

        Ok(StructMember { name, ty, span })
    }

    /// Note: does not consume the delimiter
    pub fn list<T, F>(&mut self, mut f: F, delimiter: TokenKind) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut instances = Vec::new();

        while self.peek()?.kind != delimiter {
            instances.push(f(self)?);

            if self.peek()?.kind == TokenKind::Comma {
                self.eat(TokenKind::Comma)?;
            } else {
                break;
            }
        }

        Ok(instances)
    }

    pub fn block(&mut self) -> Result<Block> {
        let start_span = self.eat(TokenKind::LeftBrace)?;

        let mut items = Vec::new();
        let mut statements = Vec::new();
        let mut return_expr = None;
        let mut ate_expr = false;

        while self.peek()?.kind != TokenKind::RightBrace {
            match self.peek()?.kind {
                TokenKind::Fn | TokenKind::Struct => items.push(self.item()?),
                _ => match self.statement_or_expression()? {
                    Either::Left(stmt) if !ate_expr => statements.push(stmt),
                    Either::Right(expr) if return_expr.is_none() => {
                        return_expr = Some(expr);
                        ate_expr = true;
                    }
                    _ => break,
                },
            }
        }

        let end_span = self.eat(TokenKind::RightBrace)?;
        let span = start_span.merge(end_span);

        Ok(Block { items, statements, return_expr, span })
    }

    pub fn r#if(&mut self) -> Result<IfExpr> {
        let mut ifs = Vec::new();
        let mut r#else = None;

        let start_span = self.eat(TokenKind::If)?;
        let condition = self.expression()?;
        let body = self.block()?;
        let span = start_span.merge(body.span);

        ifs.push(If { condition, body, span });

        while let Ok(Token { kind: TokenKind::Else, .. }) = self.peek() {
            let start_span = self.eat(TokenKind::Else)?;

            if let TokenKind::If = self.peek()?.kind {
                self.eat(TokenKind::If)?;
                let condition = self.expression()?;
                let body = self.block()?;
                let span = start_span.merge(body.span);

                ifs.push(If { condition, body, span });
            } else {
                let body = self.block()?;
                r#else = Some(body);
                break;
            }
        }

        let span = start_span.merge(match &r#else {
            Some(block) => block.span,
            None => ifs.last().unwrap().span,
        });

        Ok(IfExpr { ifs, r#else, span })
    }

    pub fn statement_or_expression(&mut self) -> Result<Either<Statement, Expression>> {
        match self.peek()?.kind {
            TokenKind::Let => Ok(Either::Left(self.statement()?)),
            _ => {
                let expr = self.expression()?;

                if self.peek().map(|t| t.kind) == Ok(TokenKind::Semicolon) {
                    let end_span = self.eat(TokenKind::Semicolon)?;
                    let span = expr.span.merge(end_span);
                    Ok(Either::Left(Statement { kind: StatementKind::Expression(expr), span }))
                } else {
                    Ok(Either::Right(expr))
                }
            }
        }
    }

    pub fn statement(&mut self) -> Result<Statement> {
        match self.peek()?.kind {
            TokenKind::Let => {
                let binding = self.variable_binding()?;
                let span = binding.span;
                let kind = StatementKind::VariableBinding(binding);
                Ok(Statement { kind, span })
            }
            TokenKind::Identifier(_) => {
                let expr = self.expression()?;
                let end_span = self.eat(TokenKind::Semicolon)?;
                let span = expr.span.merge(end_span);

                Ok(Statement { kind: StatementKind::Expression(expr), span })
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

        Ok(VariableBinding { mutable, name, ty, value, span: let_span.merge(end) })
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.inner_expr(None)
    }

    fn inner_expr(&mut self, curr_binop: Option<BinOp>) -> Result<Expression> {
        let mut primary = self.primary_expr()?;

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

                    let rhs = self.inner_expr(Some(binop))?;
                    let span = primary.span.merge(rhs.span);

                    primary = Expression {
                        kind: ExpressionKind::BinaryOperation(Box::new(primary), binop, Box::new(rhs)),
                        span,
                    };
                }
                TokenKind::Period => {
                    self.eat(TokenKind::Period)?;
                    let ident = self.identifier()?;
                    let span = primary.span.merge(ident.span);

                    primary = Expression { kind: ExpressionKind::FieldAccess(Box::new(primary), ident), span };
                }
                TokenKind::Eq => {
                    self.eat(TokenKind::Eq)?;
                    let rhs = self.expression()?;
                    let span = primary.span.merge(rhs.span);

                    return Ok(Expression { kind: ExpressionKind::Assignment(Box::new(primary), Box::new(rhs)), span });
                }
                TokenKind::LeftParen => {
                    let mut exprs = Vec::new();
                    self.eat(TokenKind::LeftParen)?;

                    while self.peek()?.kind != TokenKind::RightParen {
                        exprs.push(self.expression()?);

                        if self.peek()?.kind == TokenKind::Comma {
                            self.eat(TokenKind::Comma)?;
                        } else {
                            break;
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

    fn primary_expr(&mut self) -> Result<Expression> {
        let peek = self.peek()?;
        let span = peek.span();

        match peek.kind {
            TokenKind::Integer(n) => {
                self.eat(TokenKind::Integer(n))?;

                Ok(Expression { kind: ExpressionKind::Integer(n), span })
            }
            TokenKind::LeftParen => {
                let start_span = self.eat(TokenKind::LeftParen)?;
                let mut expr = self.expression()?;
                let end_span = self.eat(TokenKind::RightParen)?;
                expr.span = start_span.merge(end_span);

                Ok(expr)
            }
            TokenKind::Identifier(_) => {
                let path = self.path()?;

                match self.peek() {
                    Ok(Token { kind: TokenKind::LeftBrace, .. }) => {
                        let struct_expr = Box::new(self.struct_expr(path)?);
                        let span = struct_expr.span;

                        Ok(Expression { kind: ExpressionKind::Struct(struct_expr), span })
                    }
                    _ => Ok(Expression { kind: ExpressionKind::Path(path), span }),
                }
            }
            TokenKind::If => {
                let if_expr = self.r#if()?;
                let span = if_expr.span;
                Ok(Expression { kind: ExpressionKind::If(Box::new(if_expr)), span })
            }
            TokenKind::Unit => {
                self.token()?;
                Ok(Expression { kind: ExpressionKind::Unit, span })
            }
            TokenKind::LeftBrace => {
                let block = self.block()?;
                let span = block.span;
                Ok(Expression { kind: ExpressionKind::Block(Box::new(block)), span })
            }
            TokenKind::Minus => {
                let start = self.eat(TokenKind::Minus)?;
                let expr = self.expression()?;
                let span = start.merge(expr.span);

                Ok(Expression { kind: ExpressionKind::Unary(UnaryOp::Minus, Box::new(expr)), span })
            }
            b @ TokenKind::True | b @ TokenKind::False => {
                let value = match &b {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => unreachable!(),
                };

                self.eat(b)?;

                Ok(Expression { kind: ExpressionKind::Boolean(value), span })
            }
            _ => Err(ParseError::BadToken { got: peek, expected: vec!["expression"] }),
        }
    }

    pub fn struct_expr(&mut self, name: Path) -> Result<StructExpr> {
        self.eat(TokenKind::LeftBrace)?;
        let members = self.list(Self::struct_expr_member, TokenKind::RightBrace)?;
        let end_span = self.eat(TokenKind::RightBrace)?;
        let span = name.span.merge(end_span);

        Ok(StructExpr { name, members, span })
    }

    pub fn struct_expr_member(&mut self) -> Result<StructExprMember> {
        let name = self.identifier()?;
        self.eat(TokenKind::Colon)?;
        let expression = self.expression()?;
        let span = name.span.merge(expression.span);

        Ok(StructExprMember { name, expression, span })
    }

    pub fn ty(&mut self) -> Result<Type> {
        let token = self.peek()?;
        let span = token.span();

        match token.kind {
            TokenKind::Int => {
                self.eat(TokenKind::Int)?;
                Ok(Type { kind: TypeKind::Integer, span })
            }
            TokenKind::Bool => {
                self.eat(TokenKind::Bool)?;
                Ok(Type { kind: TypeKind::Bool, span })
            }
            TokenKind::Identifier(_) => Ok(Type { kind: TypeKind::Named(self.path()?), span }),
            _ => Err(ParseError::BadToken { got: token, expected: vec!["type"] }),
        }
    }

    pub fn identifier(&mut self) -> Result<Identifier> {
        let token = self.token()?;
        let span = token.span();

        match token.kind {
            TokenKind::Identifier(value) => Ok(Identifier { value, span }),
            _ => Err(ParseError::BadToken { got: token, expected: vec!["identifier"] }),
        }
    }

    pub fn path(&mut self) -> Result<Path> {
        let mut segments = Vec::new();

        segments.push(self.identifier()?);

        while let Ok(Token { kind: TokenKind::PathSep, .. }) = self.peek() {
            self.eat(TokenKind::PathSep)?;
            segments.push(self.identifier()?);
        }

        let span = {
            let start = segments.first().unwrap().span;
            let end = segments.last().unwrap().span;

            start.merge(end)
        };

        Ok(Path { segments, span })
    }

    pub fn usage(&mut self) -> Result<Use> {
        let start_span = self.eat(TokenKind::Use)?;
        let path = self.path()?;
        let end_span = self.eat(TokenKind::Semicolon)?;
        let span = start_span.merge(end_span);

        Ok(Use { path, span })
    }

    pub fn eat(&mut self, kind: TokenKind) -> Result<Span> {
        let token = self.token()?;

        if token.kind() == &kind {
            Ok(token.span())
        } else {
            Err(ParseError::BadToken { got: token, expected: vec![kind.as_str()] })
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
            TokenKind::Plus => Ok(BinOp::Add),
            TokenKind::Minus => Ok(BinOp::Subtract),
            TokenKind::Star => Ok(BinOp::Multiply),
            TokenKind::Slash => Ok(BinOp::Divide),
            TokenKind::DoubleEq => Ok(BinOp::Equal),
            _ => Err(ParseError::BadToken { got: token, expected: vec!["binary operator"] }),
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

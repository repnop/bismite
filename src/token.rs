pub use codespan::{ByteIndex, ByteSpan};
use lazy_static::lazy_static;
use logos::Logos;
use std::collections::HashMap;

pub const TIER0_KINDS: [TokenKind; 3] = [TokenKind::LParen, TokenKind::Ident, TokenKind::IntLit];

pub const TIER1_OPS: [TokenKind; 3] = [TokenKind::Minus, TokenKind::Not, TokenKind::Mult];

pub const TIER2_OPS: [TokenKind; 4] = [
    TokenKind::Mult,
    TokenKind::Div,
    TokenKind::LShift,
    TokenKind::RShift,
];

pub const TIER3_OPS: [TokenKind; 2] = [TokenKind::Plus, TokenKind::Minus];

pub const TIER4_OPS: [TokenKind; 5] = [
    TokenKind::Gt,
    TokenKind::GtEq,
    TokenKind::Lt,
    TokenKind::LtEq,
    TokenKind::NotEq,
];

/// Token type. Contains the kind of token, and the location within the source
/// that it exists.
#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lit: &'a str,
    pub span: ByteSpan,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lit: &'a str, span: std::ops::Range<usize>) -> Token<'a> {
        Self {
            kind,
            lit,
            span: ByteSpan::new(ByteIndex(span.start as u32), ByteIndex(span.end as u32)),
        }
    }
}

/// Toke error type.
#[derive(Debug, Clone, Copy)]
pub enum TokenError<'a> {
    /// Invalid token and position in source.
    InvalidToken(&'a str, ByteSpan),
    /// Invalid (integer) literal.
    InvalidLiteral(ByteSpan),
}

impl<'a> TokenError<'a> {
    pub fn span(&self) -> ByteSpan {
        use self::TokenError::*;

        match self {
            &InvalidToken(_, ref span) => span.clone(),
            &InvalidLiteral(ref span) => span.clone(),
        }
    }
}

impl<'a> ::std::fmt::Display for TokenError<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::TokenError::*;

        match self {
            &InvalidToken(ref t, _) => write!(f, "Invalid token `{}` found.", t),
            &InvalidLiteral(_) => write!(f, "Invalid literal."),
        }
    }
}

/// The type of token.
#[derive(Logos, Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    #[end]
    Eof,
    #[error]
    Error,
    /// Integer literals.
    #[regex = "(-|\\+)?[0-9]+"]
    IntLit,
    /// Identifiers.
    #[regex = "[_A-Za-z][_A-Za-z0-9]*"]
    Ident,
    /// `(`
    #[token = "("]
    LParen,
    /// `)`
    #[token = ")"]
    RParen,
    /// `{`
    #[token = "{"]
    LBrace,
    /// `}`
    #[token = "}"]
    RBrace,
    /// `[`
    #[token = "["]
    LBracket,
    /// `]`
    #[token = "]"]
    RBracket,
    /// `,`
    #[token = ","]
    Comma,
    /// `.`
    #[token = "."]
    Period,
    /// `;`
    #[token = ";"]
    Semicolon,
    /// `:`
    #[token = ":"]
    Colon,
    /// `->`
    #[token = "->"]
    Arrow,
    /// `let`
    #[token = "let"]
    Let,
    /// `fn`
    #[token = "fn"]
    Fn,
    /// `if`
    #[token = "if"]
    If,
    /// `while`
    #[token = "while"]
    While,
    /// `for`
    #[token = "for"]
    For,
    /// `struct`
    #[token = "struct"]
    Struct,
    /// Addition.
    /// `+`
    #[token = "+"]
    Plus,
    /// Addition and assignment.
    /// `+=`
    #[token = "+="]
    PlusEq,
    /// Subtraction.
    /// `-`
    #[token = "-"]
    Minus,
    /// Subtraction and assignment.
    /// `-=`
    #[token = "-="]
    MinusEq,
    /// Multiplication.
    /// `*`
    #[token = "*"]
    Mult,
    /// Multiplication and assignment.
    /// `*=`
    #[token = "*="]
    MultEq,
    /// Division.
    /// `/`
    #[token = "/"]
    Div,
    /// Division and assignment.
    /// `/=`
    #[token = "/="]
    DivEq,
    /// Right bitshift.
    /// `>>`
    #[token = ">>"]
    RShift,
    /// Left bitshift.
    /// `<<`
    #[token = "<<"]
    LShift,
    /// Negation.
    /// `!`
    #[token = "!"]
    Not,
    /// Assign.
    /// `=`
    #[token = "="]
    Assign,
    /// Equals to.
    /// `==`
    #[token = "=="]
    EqTo,
    /// Less than.
    /// `<`
    #[token = "<"]
    Lt,
    /// Greater than.
    /// `>`
    #[token = ">"]
    Gt,
    /// Less than or equal to.
    /// `<=`
    #[token = "<="]
    LtEq,
    /// Greater than or equal to.
    /// `>=`
    #[token = ">="]
    GtEq,
    /// Not equal to.
    /// `!=`
    #[token = "!="]
    NotEq,
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        use self::TokenKind::*;

        match self {
            &IntLit => "integer literal",
            &Ident => "identifier",
            &LParen => "symbol `(`",
            &RParen => "symbol `)`",
            &LBrace => "symbol `{`",
            &RBrace => "symbol `}`",
            &LBracket => "symbol `[`",
            &RBracket => "symbol `]`",
            &Comma => "symbol `,`",
            &Period => "symbol `.`",
            &Semicolon => "symbol `;`",
            &Colon => "symbol `:`",
            &Arrow => "symbol `->`",
            &Let => "keyword `let`",
            &Fn => "keyword `fn`",
            &If => "keyword `if`",
            &While => "keyword `while`",
            &For => "keyword `for`",
            &Struct => "keyword `struct`",
            &Plus => "operator `+`",
            &PlusEq => "operator `+=`",
            &Minus => "operator `-`",
            &MinusEq => "operator `-=`",
            &Mult => "operator `*`",
            &MultEq => "operator `*=`",
            &Div => "operator `/`",
            &DivEq => "operator `/=`",
            &RShift => "operator `>>`",
            &LShift => "operator `<<`",
            &Not => "operator `!`",
            &Assign => "operator `=`",
            &EqTo => "operator `==`",
            &Lt => "operator `<`",
            &Gt => "operator `>`",
            &LtEq => "operator `<=`",
            &GtEq => "operator `>=`",
            &NotEq => "operator `!=`",
        }
    }
}

/// Reserved keywords.
lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut kws = HashMap::new();

        kws.insert("let", TokenKind::Let);
        kws.insert("fn", TokenKind::Fn);
        kws.insert("if", TokenKind::If);
        kws.insert("for", TokenKind::For);
        kws.insert("while", TokenKind::While);
        kws.insert("struct", TokenKind::Struct);

        kws
    };
}

/// Type of comment.
#[derive(Debug, PartialEq)]
pub enum Comment {
    /// Single-line comment style.
    /// `// comment`
    Single,
    /// Multi-line comment style.
    /// `/* comment */`
    Multi,
}

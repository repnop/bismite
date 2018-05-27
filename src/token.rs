pub use codespan::ByteSpan;
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
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lit: &'a str,
    pub span: ByteSpan,
}

/// Toke error type.
#[derive(Debug, Clone)]
pub enum TokenError {
    /// Invalid token and position in source.
    InvalidToken(String, ByteSpan),
    /// Invalid (integer) literal.
    InvalidLiteral(ByteSpan),
}

impl TokenError {
    pub fn span(&self) -> ByteSpan {
        use self::TokenError::*;

        match self {
            &InvalidToken(_, ref span) => span.clone(),
            &InvalidLiteral(ref span) => span.clone(),
        }
    }
}

impl ::std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::TokenError::*;

        match self {
            &InvalidToken(ref t, _) => write!(f, "Invalid token `{}` found.", t),
            &InvalidLiteral(_) => write!(f, "Invalid literal."),
        }
    }
}

/// The type of token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    /// Integer literals.
    IntLit,
    /// Identifiers.
    Ident,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `,`
    Comma,
    /// `.`
    Period,
    /// `;`
    Semicolon,
    /// `:`
    Colon,
    /// `->`
    Arrow,
    /// `let`
    Let,
    /// `fn`
    Fn,
    /// `if`
    If,
    /// `while`
    While,
    /// `for`
    For,
    /// `struct`
    Struct,
    /// Addition.
    /// `+`
    Plus,
    /// Addition and assignment.
    /// `+=`
    PlusEq,
    /// Subtraction.
    /// `-`
    Minus,
    /// Subtraction and assignment.
    /// `-=`
    MinusEq,
    /// Multiplication.
    /// `*`
    Mult,
    /// Multiplication and assignment.
    /// `*=`
    MultEq,
    /// Division.
    /// `/`
    Div,
    /// Division and assignment.
    /// `/=`
    DivEq,
    /// Right bitshift.
    /// `>>`
    RShift,
    /// Left bitshift.
    /// `<<`
    LShift,
    /// Negation.
    /// `!`
    Not,
    /// Assign.
    /// `=`
    Assign,
    /// Equals to.
    /// `==`
    EqTo,
    /// Less than.
    /// `<`
    Lt,
    /// Greater than.
    /// `>`
    Gt,
    /// Less than or equal to.
    /// `<=`
    LtEq,
    /// Greater than or equal to.
    /// `>=`
    GtEq,
    /// Not equal to.
    /// `!=`
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

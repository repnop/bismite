pub use codespan::ByteSpan;

/// Token type. Contains the kind of token, and the location within the source
/// that it exists.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
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
            &InvalidToken(ref t, ref span) => span.clone(),
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
#[derive(Debug, Clone)]
pub enum TokenKind {
    /// Integer literals.
    IntLit(usize, bool),
    /// Identifiers.
    Ident(String),
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
    /// If statement.
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
            &IntLit(_, _) => "integer literal",
            &Ident(_) => "identifier",
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

impl PartialEq for TokenKind {
    fn eq(&self, other: &TokenKind) -> bool {
        use self::TokenKind::*;

        match self {
            &IntLit(n, _) => match other {
                &IntLit(m, _) => n == m,
                _ => false,
            },
            &Ident(_) => match other {
                &Ident(_) => true,
                _ => false,
            },
            tknkind => ::std::mem::discriminant(tknkind) == ::std::mem::discriminant(other),
        }
    }

    fn ne(&self, other: &TokenKind) -> bool {
        !self.eq(other)
    }
}

/// Reserved keywords.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {}

pub const KEYWORDS: [(&str, TokenKind); 6] = [
    ("let", TokenKind::Let),
    ("fn", TokenKind::Fn),
    ("if", TokenKind::If),
    ("for", TokenKind::For),
    ("while", TokenKind::While),
    ("struct", TokenKind::Struct),
];

/// Operators for operations, e.g. addition, subtraction, etc.
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {}

/// Misc. characters with no specific purpose.
#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {}

/// Comparison operators
#[derive(Debug, PartialEq, Clone)]
pub enum Comparison {}

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

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
    InvalidLiteral,
}

/// The type of token.
#[derive(Debug, Clone)]
pub enum TokenKind {
    /// Integer literals.
    IntLit(usize),
    /// Identifiers.
    Ident(String),
    /// Symbols. (Misc symbols, e.g. `(`, `)`, `[`, `]`, etc)
    Symbol(Symbol),
    /// Reserved words.
    Keyword(Keyword),
    /// Operators for performing operations.
    Operator(Operator),
    /// Comparison operators.
    Comparison(Comparison),
}

impl PartialEq for TokenKind {
    fn eq(&self, other: &TokenKind) -> bool {
        let discm_self = ::std::mem::discriminant(self);
        let discm_other = ::std::mem::discriminant(other);

        discm_self == discm_other
    }

    fn ne(&self, other: &TokenKind) -> bool {
        !self.eq(other)
    }
}

/// Reserved keywords.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    /// Variable declaration/binding.
    /// `let`
    Let,
    /// Function declaration.
    /// `fn`
    Fn,
    /// If statement.
    /// `if`
    If,
    /// While statement.
    /// `while`
    While,
    /// For statement.
    /// `for`
    For,
    /// Struct/type declaration.
    /// `struct`
    Struct,
}

const KEYWORDS: [(&str, Keyword); 6] = [
    ("let", Keyword::Let),
    ("fn", Keyword::Fn),
    ("if", Keyword::If),
    ("for", Keyword::For),
    ("while", Keyword::While),
    ("struct", Keyword::Struct),
];

impl Keyword {
    pub fn keywords() -> &'static [(&'static str, Keyword)] {
        &KEYWORDS
    }
}

/// Operators for operations, e.g. addition, subtraction, etc.
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
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
    Eq,
}

/// Misc. characters with no specific purpose.
#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
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
    Arrow,
}

/// Comparison operators
#[derive(Debug, PartialEq, Clone)]
pub enum Comparison {
    /// Equals to.
    /// `==`
    Eq,
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

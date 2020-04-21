use codespan::Span;
use logos::Logos;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum TokenKind {
    #[error]
    Error,

    // Trivia
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("->")]
    ThinArrow,
    #[token("=>")]
    ThickArrow,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,

    // Keywords
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("while")]
    While,
    #[token("let")]
    Let,
    #[token("use")]
    Use,
    #[token("mut")]
    Mut,
    #[token("struct")]
    Struct,

    #[token("\"")]
    DoubleQuote,
    #[token("'")]
    SingleQuote,
    #[regex(r#"(?x:
        [\p{XID_Start}_]
        \p{XID_Continue}*
        (\u{3F} | \u{21} | (\u{3F}\u{21}) | \u{2048})? # ? ! ?! ⁈
    )"#, |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("\\d+", |lex| lex.slice().parse())]
    Integer(i128),

    Generic(String),
    Character(char),

    #[regex("[\n\r\t ]+")]
    Whitespace,
}

impl TokenKind {
    pub fn is_binop(&self) -> bool {
        match self {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => true,
            _ => false,
        }
    }

    pub fn as_str(&self) -> &'static str {
        use TokenKind::*;
        match self {
            LeftParen => "(",
            RightParen => ")",
            LeftBracket => "[",
            RightBracket => "]",
            ThinArrow => "->",
            ThickArrow => "=>",
            Comma => ",",
            Semicolon => ";",
            LeftBrace => "{",
            RightBrace => "}",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Eq => "=",
            Colon => ":",
            Period => ".",
            Fn => "fn",
            If => "if",
            While => "while",
            Let => "let",
            Use => "use",
            Mut => "mut",
            Struct => "struct",
            DoubleQuote => "\"",
            SingleQuote => "'",
            Identifier(_) => "identifier",
            Integer(_) => "integer",
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

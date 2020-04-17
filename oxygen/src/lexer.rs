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

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum TokenKind {
    #[error]
    Error,

    // Trivia
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "["]
    LeftBracket,
    #[token = "]"]
    RightBracket,
    #[token = "->"]
    ThinArrow,
    #[token = "=>"]
    ThickArrow,
    #[token = ","]
    Comma,
    #[token = ";"]
    Semicolon,
    #[token = "{"]
    LeftBrace,
    #[token = "}"]
    RightBrace,
    #[token = "+"]
    Plus,
    #[token = "-"]
    Minus,
    #[token = "*"]
    Star,
    #[token = "="]
    Eq,
    #[token = ":"]
    Colon,
    #[token = "."]
    Period,

    // Keywords
    #[token = "fn"]
    Fn,
    #[token = "if"]
    If,
    #[token = "while"]
    While,
    #[token = "let"]
    Let,
    #[token = "use"]
    Use,

    #[token = "\""]
    DoubleQuote,
    #[token = "'"]
    SingleQuote,
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("\\d+", |lex| lex.slice().parse())]
    Integer(i128),

    Generic(String),
    Character(char),

    #[regex = "[\n\r\t]+"]
    Whitespace,
}

impl TokenKind {
    pub fn is_binop(&self) -> bool {
        match self {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star => true,
            _ => false,
        }
    }
}

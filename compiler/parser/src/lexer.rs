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
    #[token("==")]
    DoubleEq,
    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[token("::")]
    PathSep,

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
    #[token("module")]
    Module,
    #[token("else")]
    Else,

    // FIXME: Move to an enum when we can
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("Int")]
    Int,
    #[token("Bool")]
    Bool,
    #[token("Unit")]
    Unit,

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

    #[regex("[\n\r\t ]+", logos::skip)]
    Whitespace,
}

impl TokenKind {
    pub fn is_binop(&self) -> bool {
        matches!(self, TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash | TokenKind::DoubleEq)
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
            DoubleEq => "==",
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
            Module => "module",
            Else => "else",
            True => "true",
            False => "false",
            Int => "Int",
            Bool => "Bool",
            Unit => "Unit",
            DoubleQuote => "\"",
            SingleQuote => "'",
            PathSep => "::",
            Identifier(_) => "identifier",
            Integer(_) => "integer",
            Error | Generic(_) | Character(_) | Whitespace => unreachable!(),
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

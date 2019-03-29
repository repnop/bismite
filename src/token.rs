pub use codespan::{ByteIndex, ByteSpan};
use lazy_static::lazy_static;
use logos::Logos;
use std::collections::HashMap;

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
            InvalidToken(_, span) => *span,
            InvalidLiteral(span) => *span,
        }
    }
}

impl<'a> ::std::fmt::Display for TokenError<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::TokenError::*;

        match self {
            InvalidToken(ref t, _) => write!(f, "Invalid token `{}` found.", t),
            InvalidLiteral(_) => write!(f, "Invalid literal."),
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
    DecLit,
    #[regex = "0x[A-Fa-f0-9]+"]
    HexLit,
    #[regex = "0o[0-7]+"]
    OctLit,
    #[regex = "0b[0-1]+"]
    BinLit,
    /// Float literals.
    #[regex = "(-|\\+)?[0-9]+(\\.[0-9]+|e[0-9]+)"]
    FloatLit,
    /// String literal.
    #[regex = r#""([^"]|\\")*""#]
    RawStr,
    /// Char literal.
    #[regex = "'([^']|\')'"]
    RawChar,
    /// Identifiers.
    #[regex = "[_A-Za-z][_A-Za-z0-9]*"]
    Ident,
    /// Generic type.
    #[regex = "'[^'0-9\\s]+"]
    #[callback = "generic_or_char"]
    Generic,
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
    /// `::`
    #[token = "::"]
    PathSeparator,
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
    /// Bitwise/Logical XOR.
    /// `^`
    #[token = "^"]
    Xor,
    /// Bitwise AND.
    /// `&`
    #[token = "&"]
    BitAnd,
    /// Bitwise OR.
    /// `|`
    #[token = "|"]
    BitOr,
    /// Logical AND.
    /// `&&`
    #[token = "&&"]
    LogicalAnd,
    /// Logical OR.
    /// `||`
    #[token = "||"]
    LogicalOr,
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        use self::TokenKind::*;

        match self {
            Eof => "unexpected eof",
            Error => "unexpected error",
            HexLit | OctLit | BinLit | DecLit => "numeric literal",
            FloatLit => "floating point literal",
            Ident => "identifier",
            LParen => "symbol `(`",
            RParen => "symbol `)`",
            LBrace => "symbol `{`",
            RBrace => "symbol `}`",
            LBracket => "symbol `[`",
            RBracket => "symbol `]`",
            Comma => "symbol `,`",
            Period => "symbol `.`",
            Semicolon => "symbol `;`",
            Colon => "symbol `:`",
            PathSeparator => "symbol `::`",
            Arrow => "symbol `->`",
            Let => "keyword `let`",
            Fn => "keyword `fn`",
            If => "keyword `if`",
            While => "keyword `while`",
            For => "keyword `for`",
            Struct => "keyword `struct`",
            Plus => "operator `+`",
            PlusEq => "operator `+=`",
            Minus => "operator `-`",
            MinusEq => "operator `-=`",
            Mult => "operator `*`",
            MultEq => "operator `*=`",
            Div => "operator `/`",
            DivEq => "operator `/=`",
            RShift => "operator `>>`",
            LShift => "operator `<<`",
            Not => "operator `!`",
            Assign => "operator `=`",
            EqTo => "operator `==`",
            Lt => "operator `<`",
            Gt => "operator `>`",
            LtEq => "operator `<=`",
            GtEq => "operator `>=`",
            NotEq => "operator `!=`",
            Xor => "operator `^`",
            BitAnd => "operator `&`",
            BitOr => "operator `|`",
            LogicalAnd => "operator `&&`",
            LogicalOr => "operator `||`",
            RawStr => "string literal",
            RawChar => "character literal",
            Generic => "generic parameter",
        }
    }
}

fn generic_or_char<'source, Src: logos::Source<'source>>(lexer: &mut logos::Lexer<TokenKind, Src>) {
    use logos::internal::LexerInternal;

    if lexer.read() == b'\'' {
        lexer.bump(1);
        lexer.token = TokenKind::RawChar;
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

#[cfg(test)]
mod tests {
    #[test]
    fn num_literals() {
        use super::TokenKind;
        use logos::Logos;

        let mut lexer = TokenKind::lexer(
            "0xABCDEFabcdef0123456789 0b10101010 0o012345671 1234 1.0 -1.2 1e10 +0.253",
        );

        macro_rules! assert_and_advance {
            ($($t:tt)+) => {
                assert_eq!(lexer.token, $($t)+);
                lexer.advance();
            };
        }

        assert_and_advance!(TokenKind::HexLit);
        assert_and_advance!(TokenKind::BinLit);
        assert_and_advance!(TokenKind::OctLit);
        assert_and_advance!(TokenKind::DecLit);
        assert_and_advance!(TokenKind::FloatLit);
        assert_and_advance!(TokenKind::FloatLit);
        assert_and_advance!(TokenKind::FloatLit);
        assert_and_advance!(TokenKind::FloatLit);
    }

    #[test]
    fn raw_str() {
        use super::TokenKind;
        use logos::Logos;

        let lexer = TokenKind::lexer(r#""fooabc d e f    h i j\"""#);

        assert_eq!(lexer.token, TokenKind::RawStr);
        assert_eq!(lexer.slice(), r#""fooabc d e f    h i j\"""#);
    }

    #[test]
    fn generics_and_chars() {
        use super::TokenKind;
        use logos::Logos;

        let mut lexer = TokenKind::lexer(r#"'a 'ABCD 'x' 'y' '👌' '👌 '\''"#);

        macro_rules! assert_and_advance {
            ($($t:tt)+) => {
                assert_eq!(lexer.token, $($t)+, "{}", lexer.slice());
                lexer.advance();
            };
        }

        assert_and_advance!(TokenKind::Generic);
        assert_and_advance!(TokenKind::Generic);
        assert_and_advance!(TokenKind::RawChar);
        assert_and_advance!(TokenKind::RawChar);
        assert_and_advance!(TokenKind::RawChar);
        assert_and_advance!(TokenKind::Generic);
        assert_and_advance!(TokenKind::RawChar);
    }
}

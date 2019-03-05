use codespan::ByteSpan;
use std::borrow::Cow;

pub struct ParserError {
    additional_help: Option<Cow<'static, str>>,
    kind: Kind,
    message: Cow<'static, str>,
    span: ByteSpan,
}

impl ParserError {
    fn new<T: Into<Cow<'static, str>>, U: Into<Option<T>>>(
        message: impl Into<Cow<'static, str>>,
        kind: Kind,
        span: ByteSpan,
        additional_help: U,
    ) -> ParserError {
        Self {
            additional_help: additional_help.into().map(|s| s.into()),
            kind,
            message: message.into(),
            span,
        }
    }
}

pub enum Kind {
    Fatal,
    Recoverable,
    Warning,
}

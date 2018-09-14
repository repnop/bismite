use codespan::ByteIndex;
use std::{self, str::CharIndices};
use token::*;

#[derive(Debug)]
pub struct PeekableCharIndices<'a> {
    iter: CharIndices<'a>,
}

impl<'a> PeekableCharIndices<'a> {
    pub fn new(iter: CharIndices<'a>) -> Self {
        Self { iter }
    }

    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.iter.clone().next()
    }
}

impl<'a> std::ops::Deref for PeekableCharIndices<'a> {
    type Target = CharIndices<'a>;

    fn deref(&self) -> &CharIndices<'a> {
        &self.iter
    }
}

impl<'a> std::ops::DerefMut for PeekableCharIndices<'a> {
    fn deref_mut(&mut self) -> &mut CharIndices<'a> {
        &mut self.iter
    }
}

/// Iterates over tokens within a source file.
#[derive(Debug)]
pub struct TokenStream<'a>(PeekableCharIndices<'a>);

impl<'a> TokenStream<'a> {
    /// Create a new `TokenStream`.
    pub fn new(contents: &'a str) -> TokenStream<'a> {
        TokenStream(PeekableCharIndices::new(contents.char_indices()))
    }
}

impl<'a> Iterator for TokenStream<'a> {
    /// Success or failure of the token parse.
    type Item = Result<Token<'a>, TokenError<'a>>;

    /// Lexes and returns the next token.
    fn next(&mut self) -> Option<Result<Token<'a>, TokenError<'a>>> {
        while let Some((_, c)) = self.0.peek() {
            if c.is_whitespace() {
                self.0.next().unwrap();
            } else {
                break;
            }
        }

        let (pos_start, ch) = self.0.peek()?;

        let pos_end;
        let mut str_slice = self.0.as_str();

        //pos_start += 1;

        return Some(match ch {
            // Parse numbers.
            '0'...'9' => match eat_number(&mut self.0) {
                Ok(idx) => Ok(Token {
                    kind: TokenKind::IntLit,
                    lit: &str_slice[..idx as usize - pos_start],
                    span: ByteSpan::new(ByteIndex(pos_start as u32 + 1), ByteIndex(idx + 1)),
                }),
                Err(n) => Err(TokenError::InvalidLiteral(ByteSpan::new(
                    ByteIndex(pos_start as u32),
                    ByteIndex(if let Some((idx, _)) = self.0.peek() {
                        idx as u32
                    } else {
                        n
                    }),
                ))),
            },

            // Parse identifiers.
            '_' | 'a'...'z' | 'A'...'Z' => {
                pos_end = eat_ident(&mut self.0);
                str_slice = &str_slice[..pos_end as usize - pos_start];

                Ok(Token {
                    kind: if let Some(kw) = check_reserved(str_slice) {
                        kw
                    } else {
                        TokenKind::Ident
                    },
                    lit: str_slice,
                    span: ByteSpan::new(ByteIndex(pos_start as u32), ByteIndex(pos_end + 1)),
                })
            }

            // Parse individual operators or symbols.
            '>' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '>')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::RShift,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::GtEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Gt,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '<' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '<')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::LShift,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::LtEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Lt,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '!' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::NotEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Not,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '.' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Period,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            '-' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::MinusEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some((_, '>')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::Arrow,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Minus,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '+' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::PlusEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Plus,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '*' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::MultEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Mult,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '/' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::DivEq,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some((_, '/')) => {
                        eat_comment(&mut self.0, Comment::Single).unwrap();
                        // TODO: fix?
                        return self.next();
                    }
                    Some((_, '*')) => {
                        eat_comment(&mut self.0, Comment::Multi);
                        // TODO: fix?
                        return self.next();
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Div,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '=' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some((_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::EqTo,
                            lit: &str_slice[..2],
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Assign,
                        lit: &str_slice[..1],
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32 + 1),
                            ByteIndex(pos_start as u32 + 2),
                        ),
                    }),
                }
            }
            '(' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LParen,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            ')' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RParen,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            '{' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LBrace,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            '}' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RBrace,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            '[' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LBracket,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            ']' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RBracket,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            ',' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Comma,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            ';' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Semicolon,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }
            ':' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Colon,
                    lit: &str_slice[..1],
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32 + 1),
                        ByteIndex(pos_start as u32 + 2),
                    ),
                })
            }

            _ => Err(eat_invalid(&mut self.0, pos_start as u32)),
        });
    }
}

/// Consumes invalid tokens.
fn eat_invalid<'a>(iter: &mut PeekableCharIndices<'a>, start: u32) -> TokenError<'a> {
    let mut end_idx = start as usize;
    let s = iter.as_str();

    while let Some((idx, ch)) = iter.peek() {
        if ch.is_whitespace() {
            end_idx = idx + 1;
            break;
        } else {
            let (ei, _) = iter.next().unwrap();
            end_idx = ei + 1;
        }
    }

    TokenError::InvalidToken(
        &s[0..(end_idx - start as usize - 1)],
        ByteSpan::new(ByteIndex(start + 1), ByteIndex(end_idx as u32 + 1)),
    )
}

fn eat_ident<'a>(iter: &mut PeekableCharIndices<'a>) -> u32 {
    let mut pos_end = iter.peek().unwrap().0;

    while let Some((_, ch)) = iter.peek() {
        if !ch.is_alphanumeric() && ch != '_' {
            break;
        }
        pos_end = iter.next().unwrap().0;
    }

    pos_end as u32 + 1
}

const DIGIT_VALUES: [Option<(u8, usize)>; 128] = [
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some((0, 2)),
    Some((1, 2)),
    Some((2, 8)),
    Some((3, 8)),
    Some((4, 8)),
    Some((5, 8)),
    Some((6, 8)),
    Some((7, 8)),
    Some((8, 10)),
    Some((9, 10)),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some((10, 16)),
    Some((11, 16)),
    Some((12, 16)),
    Some((13, 16)),
    Some((14, 16)),
    Some((15, 16)),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some((10, 16)),
    Some((11, 16)),
    Some((12, 16)),
    Some((13, 16)),
    Some((14, 16)),
    Some((15, 16)),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
];

/// Consume a integer literal. Returns the ending byte position within the source file, and
/// the parsed number, or `None` which signifies an invalid number.
fn eat_number<'a>(iter: &mut PeekableCharIndices<'a>) -> Result<u32, u32> {
    let (start, c) = iter.peek().unwrap();
    let start = start as u32;
    let mut end_index = start;

    let radix = if c == '0' {
        iter.next().unwrap();
        get_radix(iter).map_err(|_| start + 1)?
    } else {
        10
    };

    while let Some((_, c)) = iter.peek() {
        if c.is_whitespace()
            || (!c.is_digit(radix as u32) && DIGIT_VALUES[c as u8 as usize] == None
                && !c.is_alphabetic())
        {
            break;
        }

        if let Some((_, rdx)) = DIGIT_VALUES[c as u8 as usize] {
            end_index = iter.next().unwrap().0 as u32;
            if rdx > radix {
                return Err(eat_invalid(iter, start).span().end().0);
            }
        } else {
            return Err(eat_invalid(iter, start).span().end().0);
        }
    }

    Ok(end_index + 1)
}

fn get_radix<'a>(iter: &mut PeekableCharIndices<'a>) -> Result<usize, ()> {
    let radix = match iter.peek() {
        Some((_, 'x')) => {
            iter.next().unwrap();
            16
        }
        Some((_, 'o')) => {
            iter.next().unwrap();
            8
        }
        Some((_, 'b')) => {
            iter.next().unwrap();
            2
        }
        Some((_, c)) => {
            if !c.is_digit(10) {
                return Err(());
            } else {
                10
            }
        }
        None => 10,
    };

    Ok(radix)
}

/// Consumes a comment.
fn eat_comment<'a>(iter: &mut PeekableCharIndices<'a>, t: Comment) -> Result<(), ()> {
    match t {
        Comment::Single => {
            while let Some((_, ch)) = iter.next() {
                if ch == '\n' {
                    break;
                }
            }

            Ok(())
        }
        Comment::Multi => {
            iter.next().unwrap();
            while let Some((_, ch)) = iter.next() {
                if ch == '*' {
                    if let Some((_, ch)) = iter.next() {
                        if ch == '/' {
                            break;
                        }
                    } else {
                        return Err(());
                    }
                }
            }

            Ok(())
        }
    }
}

/// Checks if an identifier is a reserved word.
fn check_reserved(s: &str) -> Option<TokenKind> {
    KEYWORDS.get(s).map(|&t| t)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn number() {
        let nums = [
            "0",
            "11",
            "0xA",
            "0b11",
            "0o10",
            "18446744073709551615",
            "18446744073709551616",
        ];

        let fails = ["0b2", "0xJ", "0o8", "0xFFasdf"];

        for num in &nums {
            let mut ts = TokenStream::new(num);
            let tkn = ts.next();
            println!("{:?}", tkn);
            let Token { kind: a, .. } = tkn.expect("Some").expect("Ok");

            assert_eq!(a, TokenKind::IntLit);

            assert!(ts.next().is_none());
        }

        for fail in &fails {
            let mut ts = TokenStream::new(fail);
            let v = ts.next().expect("Some");
            println!("{}", fail);
            assert!(v.is_err());

            assert!(ts.next().is_none());
        }
    }

    #[test]
    fn idents() {
        let idents = ["asdf", "a1234", "_", "_abc", "_1234"];

        for ident in &idents {
            let mut ts = TokenStream::new(ident);
            assert!(ts.0.peek().is_some());
            if let Some(Ok(token)) = ts.next() {
                if let Token {
                    kind: TokenKind::Ident,
                    lit: s,
                    ..
                } = token
                {
                    assert_eq!(s, *ident, "Valid ident not parsed correctly.");
                } else {
                    panic!("{}", format!("Valid ident {} not parsed correctly.", ident));
                }
            } else {
                panic!("{}", format!("Valid ident {} not parsed correctly.", ident));
            }
        }
    }

    #[test]
    fn tokens() {
        let good_tkns = "let fn ident 1 0x1 0b1 0o1 + += - -= * *= / /= >> << ! = == < > <= >= != ( ) { } [ ] , . ;";

        let mut ts = TokenStream::new(good_tkns);

        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Let);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Fn);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Ident);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::IntLit);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::IntLit);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::IntLit);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::IntLit);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Plus);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::PlusEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Minus);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::MinusEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Mult);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::MultEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Div);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::DivEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::RShift);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::LShift);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Not);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Assign);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::EqTo);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Lt);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Gt);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::LtEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::GtEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::NotEq);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::LParen);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::RParen);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::LBrace);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::RBrace);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::LBracket);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::RBracket);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Comma);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Period);
        assert_eq!(ts.next().unwrap().unwrap().kind, TokenKind::Semicolon);
        assert!(ts.next().is_none());
    }
}

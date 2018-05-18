use codespan::ByteIndex;
use std::{iter::Peekable, str::CharIndices};
use token::*;

/// Iterates over tokens within a source file.
#[derive(Debug)]
pub struct TokenStream<'a>(Peekable<CharIndices<'a>>);

impl<'a> TokenStream<'a> {
    /// Create a new `TokenStream`.
    pub fn new(contents: &'a str) -> TokenStream<'a> {
        TokenStream(contents.char_indices().peekable())
    }
}

impl<'a> Iterator for TokenStream<'a> {
    /// Success or failure of the token parse.
    type Item = Result<Token, TokenError>;

    /// Lexes and returns the next token.
    fn next(&mut self) -> Option<Result<Token, TokenError>> {
        while let Some(&(_, c)) = self.0.peek() {
            if c.is_whitespace() {
                self.0.next().unwrap();
                continue;
            } else {
                break;
            }
        }

        let &(mut pos_start, ch) = self.0.peek()?;
        let mut pos_end = 0;

        pos_start += 1;

        return Some(match ch {
            // Parse numbers.
            '0'...'9' => match eat_number(&mut self.0) {
                Ok((idx, n, of)) => Ok(Token {
                    kind: TokenKind::IntLit(n, of),
                    span: ByteSpan::new(ByteIndex(pos_start as u32), ByteIndex(idx + 1)),
                }),
                Err(n) => Err(TokenError::InvalidLiteral(ByteSpan::new(
                    ByteIndex(pos_start as u32),
                    ByteIndex(if let Some(&(idx, _)) = self.0.peek() {
                        idx as u32
                    } else {
                        n
                    }),
                ))),
            },

            // Parse identifiers.
            '_' | 'a'...'z' | 'A'...'Z' => {
                let mut ident = String::new();

                'id: while let Some(&(idx, ch)) = self.0.peek() {
                    if !ch.is_alphanumeric() && ch != '_' {
                        pos_end = idx as u32;
                        break 'id;
                    }

                    ident.push(self.0.next().unwrap().1);
                }

                if pos_end == 0 {
                    pos_end = pos_start as u32 + 1;
                }

                Ok(Token {
                    kind: if let Some(kw) = check_reserved(&ident) {
                        kw
                    } else {
                        TokenKind::Ident(ident)
                    },
                    span: ByteSpan::new(ByteIndex(pos_start as u32), ByteIndex(pos_end + 1)),
                })
            }

            // Parse individual operators or symbols.
            '>' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '>')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::RShift,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::GtEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Gt,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '<' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '<')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::LShift,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::LtEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Lt,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '!' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::NotEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Not,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '.' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Period,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            '-' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::MinusEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some(&(_, '>')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::Arrow,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Minus,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '+' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::PlusEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Plus,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '*' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::MultEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Mult,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '/' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::DivEq,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    Some(&(_, '/')) => {
                        eat_comment(&mut self.0, Comment::Single).unwrap();
                        // TODO: fix?
                        return self.next();
                    }
                    Some(&(_, '*')) => {
                        eat_comment(&mut self.0, Comment::Multi);
                        // TODO: fix?
                        return self.next();
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Div,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '=' => {
                self.0.next().unwrap();

                match self.0.peek() {
                    Some(&(_, '=')) => {
                        let idx = self.0.next().unwrap().0 + 1;

                        Ok(Token {
                            kind: TokenKind::EqTo,
                            span: ByteSpan::new(
                                ByteIndex(pos_start as u32),
                                ByteIndex(idx as u32 + 1),
                            ),
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::Assign,
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    }),
                }
            }
            '(' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LParen,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            ')' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RParen,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            '{' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LBrace,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            '}' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RBrace,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            '[' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::LBracket,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            ']' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::RBracket,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            ',' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Comma,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            ';' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Semicolon,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }
            ':' => {
                self.0.next().unwrap();

                Ok(Token {
                    kind: TokenKind::Colon,
                    span: ByteSpan::new(
                        ByteIndex(pos_start as u32),
                        ByteIndex(pos_start as u32 + 1),
                    ),
                })
            }

            _ => Err(eat_invalid(&mut self.0, pos_start as u32)),
        });
    }
}

/// Consumes invalid tokens.
fn eat_invalid<'a>(iter: &mut Peekable<CharIndices<'a>>, start: u32) -> TokenError {
    let mut invalid = String::new();
    let mut end_idx = start as usize;

    while let Some(&(idx, ch)) = iter.peek() {
        if ch.is_whitespace() {
            end_idx = idx + 1;
            break;
        } else {
            let (ei, ch) = iter.next().unwrap();
            end_idx = ei + 1;
            invalid.push(ch);
        }
    }

    TokenError::InvalidToken(
        invalid,
        ByteSpan::new(ByteIndex(start), ByteIndex(end_idx as u32 + 1)),
    )
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
fn eat_number<'a>(iter: &mut Peekable<CharIndices<'a>>) -> Result<(u32, usize, bool), u32> {
    let mut result = 0usize;
    let mut end_index = 0;
    let mut overflow = false;
    let start = iter.peek().unwrap().0 as u32;

    let radix = if let Some(&(_, '0')) = iter.peek() {
        iter.next().unwrap();
        get_radix(iter)
    } else {
        10usize
    };

    while let Some(&(_, c)) = iter.peek() {
        if c.is_whitespace() || !c.is_digit(radix as u32) {
            break;
        }

        if let Some((val, rdx)) = DIGIT_VALUES[c as u8 as usize] {
            iter.next().unwrap();

            if rdx > radix {
                return Err(eat_invalid(iter, start).span().end().0);
            }

            end_index += 1;

            let (r, of) = result.overflowing_mul(radix as usize);
            result = r;

            if !overflow {
                overflow = of;
            }

            let (r, of) = result.overflowing_add(val as usize);
            result = r;

            if !overflow {
                overflow = of;
            }
        } else {
            return Err(eat_invalid(iter, start).span().end().0);
        }
    }

    // Free me from this lexing hell

    Ok((end_index, result, overflow))
}

fn get_radix<'a>(iter: &mut Peekable<CharIndices<'a>>) -> usize {
    match iter.peek() {
        Some(&(_, 'x')) => {
            iter.next().unwrap();
            16
        }
        Some(&(_, 'o')) => {
            iter.next().unwrap();
            8
        }
        Some(&(_, 'b')) => {
            iter.next().unwrap();
            2
        }
        _ => 10,
    }
}

/// Consumes a comment.
fn eat_comment<'a>(iter: &mut Peekable<CharIndices<'a>>, t: Comment) -> Result<(), ()> {
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
    for &(rep, ref keyword) in (&KEYWORDS).iter() {
        if s == rep {
            return Some(keyword.clone());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn number() {
        let nums = [
            ("0", 0),
            ("11", 11),
            ("0xA", 0xA),
            ("0b11", 0b11),
            ("0o10", 0o10),
            ("18446744073709551615", 18446744073709551615),
            ("18446744073709551616", 0),
        ];

        let fails = ["0b2", "0xJ", "0o8", "0xFFasdf"];

        for num in &nums {
            let mut ts = TokenStream::new(num.0);
            let Token { kind: a, .. } = ts.next().expect("Some").expect("Ok");

            match a {
                TokenKind::IntLit(n, _) => assert!(n == num.1, format!("{} != {}", n, num.1)),
                _ => unreachable!(),
            }

            assert!(ts.next().is_none());
        }

        for fail in &fails {
            let mut ts = TokenStream::new(fail);
            let v = ts.next().expect("Some");
            assert!(v.is_err());

            assert!(ts.next().is_none());
        }
    }

    #[test]
    fn idents() {
        let idents = ["asdf", "a1234", "_", "_abc", "_1234"];

        for ident in &idents {
            let mut ts = TokenStream::new(ident);

            if let Some(Ok(token)) = ts.next() {
                if let Token {
                    kind: TokenKind::Ident(s),
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
}

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
        'mainlp: loop {
            let &(mut pos_start, ch) = self.0.peek()?;
            let mut pos_end = 0;

            pos_start += 1;

            if ch.is_whitespace() {
                self.0.next().unwrap();
                continue;
            }

            return Some(match ch {
                // Parse numbers.
                '0'...'9' => match eat_number(&mut self.0) {
                    Some((idx, n)) => Ok(Token {
                        kind: TokenKind::IntLit(n),
                        span: ByteSpan::new(ByteIndex(pos_start as u32), ByteIndex(idx + 1)),
                    }),
                    None => Err(TokenError::InvalidLiteral),
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

                    Ok(Token {
                        kind: check_reserved(ident),
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
                                kind: TokenKind::Operator(Operator::RShift),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        Some(&(_, '=')) => {
                            let idx = self.0.next().unwrap().0 + 1;

                            Ok(Token {
                                kind: TokenKind::Comparison(Comparison::GtEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Comparison(Comparison::Gt),
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
                                kind: TokenKind::Operator(Operator::LShift),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        Some(&(_, '=')) => {
                            let idx = self.0.next().unwrap().0 + 1;

                            Ok(Token {
                                kind: TokenKind::Comparison(Comparison::LtEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Comparison(Comparison::Lt),
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
                                kind: TokenKind::Comparison(Comparison::NotEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Not),
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
                        kind: TokenKind::Symbol(Symbol::Period),
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
                                kind: TokenKind::Operator(Operator::MinusEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        Some(&(_, '>')) => {
                            let idx = self.0.next().unwrap().0 + 1;

                            Ok(Token {
                                kind: TokenKind::Symbol(Symbol::Arrow),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Minus),
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
                                kind: TokenKind::Operator(Operator::PlusEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Plus),
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
                                kind: TokenKind::Operator(Operator::MultEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Mult),
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
                                kind: TokenKind::Operator(Operator::DivEq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        Some(&(_, '/')) => {
                            eat_comment(&mut self.0, Comment::Single).unwrap();
                            // TODO: fix?
                            self.next()?
                        }
                        Some(&(_, '*')) => {
                            eat_comment(&mut self.0, Comment::Multi);
                            // TODO: fix?
                            self.next()?
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Div),
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
                                kind: TokenKind::Comparison(Comparison::Eq),
                                span: ByteSpan::new(
                                    ByteIndex(pos_start as u32),
                                    ByteIndex(idx as u32 + 1),
                                ),
                            })
                        }
                        _ => Ok(Token {
                            kind: TokenKind::Operator(Operator::Eq),
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
                        kind: TokenKind::Symbol(Symbol::LParen),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                ')' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::RParen),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                '{' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::LBrace),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                '}' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::RBrace),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                '[' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::LBracket),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                ']' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::RBracket),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                ',' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::Comma),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                ';' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::Semicolon),
                        span: ByteSpan::new(
                            ByteIndex(pos_start as u32),
                            ByteIndex(pos_start as u32 + 1),
                        ),
                    })
                }
                ':' => {
                    self.0.next().unwrap();

                    Ok(Token {
                        kind: TokenKind::Symbol(Symbol::Colon),
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
}

/// Consumes invalid tokens.
fn eat_invalid<'a>(iter: &mut Peekable<CharIndices<'a>>, start: u32) -> TokenError {
    let mut invalid = String::new();
    let mut end_idx = 0;

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

/// Consume a integer literal. Returns the ending byte position within the source file, and
/// the parsed number, or `None` which signifies an invalid number.
fn eat_number<'a>(iter: &mut Peekable<CharIndices<'a>>) -> Option<(u32, usize)> {
    let mut result: usize = 0;
    let mut base = 10;
    let mut end_index = 0;

    let (start, mut ch) = iter.next().unwrap();

    if ch == '0' {
        match iter.peek() {
            Some(&(_, 'b')) => {
                base = 2;
                iter.next().unwrap();
                // TODO: fix this hack
                digit(iter.peek()?.1, base)?;
            }
            Some(&(_, 'o')) => {
                base = 8;
                iter.next().unwrap();
                digit(iter.peek()?.1, base)?;
            }
            Some(&(_, 'x')) => {
                base = 16;
                iter.next().unwrap();
                digit(iter.peek()?.1, base)?;
            }
            Some(&(_, '0'...'9')) => {}
            Some(&(idx, ' ')) | Some(&(idx, '\t')) => return Some((idx as u32, 0)),
            // TODO: Fix hack here.
            Some(_) => return None,
            None => return Some(((start + 1) as u32, 0)),
        };
    }

    loop {
        result = result.checked_mul(base)?.checked_add(digit(ch, base)?)?;

        match iter.peek() {
            Some(&(_, '0'...'9')) | Some(&(_, 'A'...'F')) | Some(&(_, 'a'...'f')) => {
                let (ei, ch2) = iter.next().unwrap();
                end_index = ei;
                ch = ch2;
            }
            Some(&p) => {
                end_index = p.0;
                break;
            }
            None => break,
        }
    }

    Some((end_index as u32, result))
}

/// Validates digit and calculates value.
fn digit(ch: char, base: usize) -> Option<usize> {
    match ch {
        '0'...'9' => if (ch as u8 - b'0') > (base - 1) as u8 {
            None
        } else {
            Some((ch as u8 - b'0') as usize)
        },
        'A'...'F' => if base == 16 {
            Some((ch as u8 - b'A') as usize + 10)
        } else {
            None
        },
        'a'...'f' => if base == 16 {
            Some((ch as u8 - b'a') as usize + 10)
        } else {
            None
        },
        _ => None,
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
fn check_reserved(s: String) -> TokenKind {
    for &(rep, keyword) in Keyword::keywords() {
        if s == rep {
            return TokenKind::Keyword(keyword);
        }
    }

    TokenKind::Ident(s)
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
        ];

        let fails = ["18446744073709551616", "0b2", "0xJ", "0o8"];

        for num in &nums {
            let mut ts = TokenStream::new(num.0);
            let Token { kind: a, .. } = ts.next().expect("Some").expect("Ok");

            match a {
                TokenKind::IntLit(n) => assert!(n == num.1, format!("{} != {}", n, num.1)),
                _ => unreachable!(),
            }
        }

        for fail in &fails {
            let mut ts = TokenStream::new(fail);
            let v = ts.next().expect("Some");
            assert!(v.is_err());
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

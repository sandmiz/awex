use core::panic;
use std::str::Chars;

pub struct Lexer<'a> {
    pub source: Chars<'a>,
    pub state: State,
    pub buf: String,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    _SOF,
    _List,
    _Tuple,
    _Args,
    _Anno,
    _Block,
    _Path,
    _EOF,
    ID,
    Str,
    Number,
    Bool,
    Nothing,
    Annotation,
    If,
    Else,
    Elif,
    For,
    ForEach,
    Forever,
    While,
    Match,
    Try,
    Except,
    Case,
    Shard,
    Aggr,
    Mod,
    Get,
    As,
    LRound,
    RRound,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
    Dot,
    Comma,
    Colon,
    Semicolon,
    RArrow,
    FatRArrow,
    Or,
    And,
    Exclam,
    Equal,
    NEqual,
    GreatThan,
    SmallThan,
    GreatEq,
    SmallEq,
    Plus,
    Minus,
    Asterisk,
    TiltBar,
    Power,
    Hash,
    HashExclam,
    HashQuest,
    Mailbox,
    Break,
}

pub enum State {
    Initial,
    Letter,
    Number,
    Symbol,
    Chain,
    Binary,
    Octa,
    Decimal,
    Float,
    Hex,
    Str,
    Annotation,
    Comment,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.source.next();
        let mut ret: Option<Self::Item> = None;

        self.state = match ch {
            Some(ch) => match self.state {
                State::Initial => {
                    self.buf = ch.to_string();

                    if ch.is_ascii_alphabetic() {
                        State::Letter
                    } else if ch.is_ascii_digit() {
                        if ch == '0' {
                            State::Number
                        } else {
                            State::Decimal
                        }
                    } else if ch.is_ascii_punctuation() {
                        State::Symbol
                    } else if ch.is_ascii_whitespace() {
                        State::Initial
                    } else {
                        panic!("Invalid character: {}", ch)
                    }
                }
                State::Letter => {
                    if ch.is_ascii_alphanumeric() {
                        self.buf.push(ch);

                        State::Chain
                    } else if ch == '@' {
                        self.buf.push(ch);

                        State::Annotation
                    } else if ch.is_ascii() {
                        ret = Some(Token::ID);

                        if ch.is_ascii_punctuation() {
                            self.buf = ch.to_string();

                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Invalid character: {}", ch)
                    }
                }
                State::Number => {
                    if ch.is_ascii_digit() {
                        self.buf.push(ch);

                        State::Octa
                    } else if ch == 'x' {
                        self.buf.push(ch);

                        State::Hex
                    } else if ch == 'b' {
                        self.buf.push(ch);

                        State::Binary
                    } else if ch == '.' {
                        State::Float
                    } else if ch.is_ascii() {
                        ret = Some(Token::Number);

                        if ch.is_ascii_alphabetic() {
                            self.buf = ch.to_string();

                            State::Letter
                        } else if ch.is_ascii_punctuation() {
                            self.buf = ch.to_string();

                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Invalid character: {}", ch)
                    }
                }
                State::Symbol => {
                    if ch.is_ascii_punctuation()
                        && !"{}()[]\"`".contains(&self.buf)
                        && !"{}()[]\"`".contains(ch)
                    {
                        self.buf.push(ch);

                        State::Symbol
                    } else if self.buf == "\"" {
                        self.buf = String::new();

                        if ch == '"' {
                            ret = Some(Token::Str);
                            State::Initial
                        } else {
                            self.buf.push(ch);
                            State::Str
                        }
                    } else if self.buf == "`" {
                        if ch != '\n' {
                            State::Comment
                        } else {
                            State::Initial
                        }
                    } else if ch.is_ascii() {
                        ret = Some(match self.buf.as_str() {
                            "+" => Token::Plus,
                            "-" => Token::Minus,
                            "*" => Token::Asterisk,
                            "/" => Token::TiltBar,
                            "**" => Token::Power,
                            ":" => Token::Colon,
                            ";" => Token::Semicolon,
                            "," => Token::Comma,
                            "." => Token::Dot,
                            "!" => Token::Exclam,
                            "||" => Token::Or,
                            "&&" => Token::And,
                            "==" => Token::Equal,
                            "!=" => Token::NEqual,
                            "<" => Token::SmallThan,
                            "<=" => Token::SmallEq,
                            ">" => Token::GreatThan,
                            ">=" => Token::GreatEq,
                            "=<" => Token::Mailbox,
                            "=>" => Token::FatRArrow,
                            "->" => Token::RArrow,
                            "!!!" => Token::Break,
                            "#" => Token::Hash,
                            "#!" => Token::HashExclam,
                            "#?" => Token::HashQuest,
                            "(" => Token::LRound,
                            "[" => Token::LSquare,
                            "{" => Token::LCurly,
                            ")" => Token::RRound,
                            "]" => Token::RSquare,
                            "}" => Token::RCurly,
                            _ => panic!("Unexpected chain: {}", self.buf),
                        });

                        self.buf = ch.to_string();
                        if ch.is_ascii_alphabetic() {
                            State::Letter
                        } else if ch.is_ascii_digit() {
                            if ch == '0' {
                                State::Number
                            } else {
                                State::Decimal
                            }
                        } else if ch.is_ascii_punctuation() {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Chain => {
                    if ch.is_ascii_alphanumeric() {
                        self.buf.push(ch);

                        State::Chain
                    } else if ch.is_ascii() {
                        ret = Some(match self.buf.as_str() {
                            "if" => Token::If,
                            "else" => Token::Else,
                            "elif" => Token::Elif,
                            "for" => Token::For,
                            "foreach" => Token::ForEach,
                            "forever" => Token::Forever,
                            "while" => Token::While,
                            "match" => Token::Match,
                            "case" => Token::Case,
                            "mod" => Token::Mod,
                            "get" => Token::Get,
                            "as" => Token::As,
                            "try" => Token::Try,
                            "except" => Token::Except,
                            "shard" => Token::Shard,
                            "aggr" => Token::Aggr,
                            "true" | "false" => Token::Bool,
                            "nothing" => Token::Nothing,
                            _ => Token::ID,
                        });

                        if ch.is_ascii_punctuation() {
                            self.buf = ch.to_string();

                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Decimal => {
                    if ch.is_ascii_digit() || ch == '.' {
                        self.buf.push(ch);

                        if ch == '.' {
                            State::Float
                        } else {
                            State::Decimal
                        }
                    } else if ch.is_ascii() && !ch.is_ascii_alphabetic() {
                        ret = Some(Token::Number);

                        self.buf = ch.to_string();
                        if ch.is_ascii_punctuation() {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Float => {
                    if ch.is_ascii_digit() {
                        self.buf.push(ch);

                        State::Float
                    } else if ch.is_ascii() && !ch.is_ascii_alphabetic() {
                        ret = Some(Token::Number);

                        self.buf = ch.to_string();
                        if ch.is_ascii_punctuation() && ch != '.' {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Hex => {
                    if ch.is_ascii_hexdigit() {
                        self.buf.push(ch);

                        State::Hex
                    } else if ch.is_ascii() && !ch.is_ascii_alphabetic() {
                        ret = Some(Token::Number);

                        self.buf = ch.to_string();
                        if ch.is_ascii_punctuation() && ch != '.' {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Octa => {
                    if ch.is_ascii_digit() && (ch as u32) < 56 {
                        self.buf.push(ch);

                        State::Octa
                    } else if ch.is_ascii() && !ch.is_ascii_alphabetic() {
                        ret = Some(Token::Number);

                        self.buf = ch.to_string();
                        if ch.is_ascii_punctuation() && ch != '.' {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Binary => {
                    if ch == '0' || ch == '1' {
                        self.buf.push(ch);

                        State::Binary
                    } else if ch.is_ascii() && !ch.is_ascii_alphabetic() {
                        ret = Some(Token::Number);

                        self.buf = ch.to_string();
                        if ch.is_ascii_punctuation() && ch != '.' {
                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
                State::Str => {
                    if ch == '"' {
                        ret = Some(Token::Str);
                        State::Initial
                    } else {
                        State::Str
                    }
                }
                State::Comment => {
                    if ch == '\n' {
                        State::Initial
                    } else {
                        State::Comment
                    }
                }
                State::Annotation => {
                    if ch.is_ascii_alphanumeric() {
                        self.buf.push(ch);

                        State::Annotation
                    } else if ch.is_ascii() {
                        ret = Some(Token::Annotation);

                        if ch.is_ascii_punctuation() {
                            self.buf = ch.to_string();

                            State::Symbol
                        } else {
                            State::Initial
                        }
                    } else {
                        panic!("Unexpected character: {}", ch)
                    }
                }
            },
            None => return Some(Token::_EOF),
        };

        match ret {
            Some(_) => ret,
            None => self.next(),
        }
    }
}

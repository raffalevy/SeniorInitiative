pub mod token;

use regex::Regex;
use token::*;

pub struct Lexer<'a> {
    slice: &'a str,
    patterns: Vec<(Regex, fn(String) -> Token)>,
    err_pattern: Regex
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            slice: input,
            patterns: init_patterns().unwrap(),
            err_pattern: Regex::new(r"^\S+").unwrap()
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, &'a str>;

    fn next(&mut self) -> Option<Result<Token, &'a str>> {
        if self.slice.is_empty() {
            return None;
        }

        self.slice = self.slice.trim_start();

        for (pattern, cb) in &self.patterns {
            if let Some(matched) = pattern.find(self.slice) {
                self.slice = &self.slice[matched.end()..];
                return Some(Ok(cb(matched.as_str().to_owned())));
            }
        }

        Some(Err(self.err_pattern.find(self.slice)?.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_lex_1() {
        use token::Token;
        let lexer = Lexer::new("def square(x) = x * x");
        let expected = vec![
            Token::Keyword("def".to_owned()),
            Token::Identifier("square".to_owned()),
            Token::OpenParen,
            Token::Identifier("x".to_owned()),
            Token::CloseParen,
            Token::Equal,
            Token::Identifier("x".to_owned()),
            Token::Operator("*".to_owned()),
            Token::Identifier("x".to_owned()),
        ];
        for (res, expected) in lexer.zip(expected) {
            assert_eq!(res.unwrap(), expected);
        }
    }

    #[test]
    fn t_lex_2() {
        use token::Token;
        let lexer = Lexer::new("match x | 3 => <> | x => <9.234e-2>");
        let expected = vec![
            Token::Keyword("match".to_owned()),
            Token::Identifier("x".to_owned()),
            Token::Pipe,
            Token::IntLiteral(3),
            Token::DoubleArrow,
            Token::Diamond,
            Token::Pipe,
            Token::Identifier("x".to_owned()),
            Token::DoubleArrow,
            Token::OpenAngle,
            Token::FloatLiteral(9.234e-2),
            Token::CloseAngle
        ];
        for (res, expected) in lexer.zip(expected) {
            assert_eq!(res.unwrap(), expected);
        }
    }
}
use regex::Regex;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Diamond,

    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenAngle,
    CloseAngle,

    Comma,

    DoubleArrow,
    SingleArrow,

    Equal,

    Dot,

    OpAssign(String),

    Operator(String),

    Pipeline,
    Pipe,

    Keyword(String),
    Identifier(String),
    FloatLiteral(f64),
    IntLiteral(u64),

    StringLiteral(String)
}

pub fn init_patterns() -> Result<Vec<(Regex, fn(String) -> Token)>, regex::Error> {
    Ok(vec![
        (Regex::new(r"^<>")?, |_| Token::Diamond),
        (Regex::new(r"^\(")?, |_| Token::OpenParen),
        (Regex::new(r"^\)")?, |_| Token::CloseParen),
        (Regex::new(r"\[")?, |_| Token::OpenSquare),
        (Regex::new(r"^\]")?, |_| Token::CloseSquare),
        (Regex::new(r"^,")?, |_| Token::Comma),
        (Regex::new(r"^=>")?, |_| Token::DoubleArrow),
        (Regex::new(r"^->")?, |_| Token::SingleArrow),
        (Regex::new(r"^=")?, |_| Token::Equal),
        (Regex::new(r"^\.")?, |_| Token::Dot),
        (Regex::new(r"^(\+=|-=|\*=|/=)")?, Token::OpAssign),
        (Regex::new(r"^(/\\|\+|-|\*|/|\?|==|!=|>|<|<=|>=)")?, Token::Operator),
        (Regex::new(r"^<")?, |_| Token::OpenAngle),
        (Regex::new(r"^>")?, |_| Token::CloseAngle),
        (
            Regex::new(r"^(def|let|match|if|else|in|forall|for|trait|type|await|while|true|false|where|intrinsic|mut|_)")?,
            Token::Keyword,
        ),
        (Regex::new(r"^\|>")?, |_| Token::Pipeline),
        (Regex::new(r"^\|")?, |_| Token::Pipe),
        (Regex::new(r"^([a-zA-Z][a-zA-Z0-9_]*)")?, Token::Identifier),
        (
            Regex::new(r"^([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][+-]?[0-9]+)?")?,
            parse_float_literal,
        ),
        (Regex::new(r"^[0-9]")?, parse_int_literal),
        (Regex::new(r#""(?:\\"|[^"])*""#)?, Token::StringLiteral)
    ])
}

#[inline]
fn parse_float_literal(lit: String) -> Token {
    Token::FloatLiteral(lit.parse().unwrap())
}

#[inline]
fn parse_int_literal(lit: String) -> Token {
    Token::IntLiteral(lit.parse().unwrap())
}

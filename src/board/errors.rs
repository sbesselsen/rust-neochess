use std::fmt::Display;

#[derive(Debug)]
pub struct FenParseError {
    message: String,
}

impl From<String> for FenParseError {
    fn from(message: String) -> Self {
        FenParseError { message }
    }
}

impl From<&str> for FenParseError {
    fn from(message: &str) -> Self {
        FenParseError {
            message: String::from(message),
        }
    }
}

impl Display for FenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("FenParseError: ")?;
        f.write_str(&self.message)
    }
}

#[derive(Debug)]
pub struct BoardMoveError {
    message: String,
}

impl From<String> for BoardMoveError {
    fn from(message: String) -> Self {
        BoardMoveError { message }
    }
}

impl From<&str> for BoardMoveError {
    fn from(message: &str) -> Self {
        BoardMoveError {
            message: String::from(message),
        }
    }
}

impl Display for BoardMoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("BoardMoveError: ")?;
        f.write_str(&self.message)
    }
}

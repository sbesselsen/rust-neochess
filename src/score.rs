use crate::board::{COLOR_BLACK, COLOR_WHITE};
use std::{cmp::Ordering, fmt::Display, ops::Neg};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Score {
    MinusInfinity,
    Value(i32),
    PlusInfinity,
}

impl Score {
    pub fn is_finite(&self) -> bool {
        matches!(self, Score::Value(_))
    }

    pub fn is_win(&self, color: usize) -> bool {
        match self {
            Score::PlusInfinity => color == COLOR_WHITE,
            Score::MinusInfinity => color == COLOR_BLACK,
            _ => false,
        }
    }
}

impl Neg for Score {
    type Output = Score;

    fn neg(self) -> Self::Output {
        match self {
            Score::MinusInfinity => Score::PlusInfinity,
            Score::PlusInfinity => Score::MinusInfinity,
            Score::Value(v) => Score::Value(-v),
        }
    }
}

impl std::hash::Hash for Score {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Score::Value(x) => {
                state.write_u8(0);
                state.write_i32(*x);
            }
            Score::MinusInfinity => {
                state.write_u8(1);
            }
            Score::PlusInfinity => {
                state.write_u8(2);
            }
        }
        state.finish();
    }
}

impl PartialOrd for Score {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Score {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return Ordering::Equal;
        }
        match (self, other) {
            (Score::MinusInfinity, Score::MinusInfinity) => Ordering::Equal,
            (Score::MinusInfinity, _) => Ordering::Less,
            (_, Score::MinusInfinity) => Ordering::Greater,
            (Score::PlusInfinity, Score::PlusInfinity) => Ordering::Equal,
            (Score::PlusInfinity, _) => Ordering::Greater,
            (_, Score::PlusInfinity) => Ordering::Less,
            (Score::Value(x), Score::Value(y)) => x.cmp(y),
        }
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Score::MinusInfinity => f.write_str("-Infinity"),
            Score::PlusInfinity => f.write_str("+Infinity"),
            Score::Value(x) => f.write_fmt(format_args!("{}", x)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use crate::score::Score;

    #[test]
    fn score_compare() {
        assert_eq!(
            Score::PlusInfinity.cmp(&Score::Value(100)),
            Ordering::Greater
        );
        assert_eq!(Score::MinusInfinity.cmp(&Score::Value(100)), Ordering::Less);
        assert_eq!(
            Score::PlusInfinity.cmp(&Score::PlusInfinity),
            Ordering::Equal
        );
        assert_eq!(Score::Value(100).cmp(&Score::Value(50)), Ordering::Greater);
    }

    #[test]
    fn score_neg() {
        assert_eq!(-Score::MinusInfinity, Score::PlusInfinity);
        assert_eq!(-Score::PlusInfinity, Score::MinusInfinity);
        assert_eq!(-Score::Value(100), Score::Value(-100));
    }

    #[test]
    fn score_finite() {
        assert!(!Score::MinusInfinity.is_finite());
        assert!(Score::Value(0).is_finite());
    }
}

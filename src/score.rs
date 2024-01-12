use std::{cmp::Ordering, fmt::Display, ops::Neg};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Score {
    WinIn(u32),
    Value(i32),
    LossIn(u32),
}

impl Score {
    pub fn is_finite(&self) -> bool {
        matches!(self, Score::Value(_))
    }

    pub fn add_value(self, value: i32) -> Score {
        match self {
            Score::Value(v) => Score::Value(v + value),
            _ => self,
        }
    }
}

impl Neg for Score {
    type Output = Score;

    fn neg(self) -> Self::Output {
        match self {
            Score::WinIn(n) => Score::LossIn(n),
            Score::LossIn(n) => Score::WinIn(n),
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
            Score::WinIn(n) => {
                state.write_u8(1);
                state.write_u32(*n);
            }
            Score::LossIn(n) => {
                state.write_u8(2);
                state.write_u32(*n);
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
            (Score::WinIn(a), Score::WinIn(b)) => b.cmp(a),
            (Score::WinIn(_), _) => Ordering::Greater,
            (Score::LossIn(a), Score::LossIn(b)) => a.cmp(b),
            (Score::LossIn(_), _) => Ordering::Less,
            (Score::Value(x), Score::Value(y)) => x.cmp(y),
            (Score::Value(_), Score::LossIn(_)) => Ordering::Greater,
            (Score::Value(_), Score::WinIn(_)) => Ordering::Less,
        }
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Score::WinIn(n) => f.write_fmt(format_args!("#{}", n)),
            Score::LossIn(n) => f.write_fmt(format_args!("#-{}", n)),
            Score::Value(x) => f.write_fmt(format_args!("{}", x)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::score::Score;

    #[test]
    fn score_compare() {
        assert!(Score::WinIn(5) > Score::WinIn(6));
        assert!(Score::WinIn(5) <= Score::WinIn(5));
        assert!(Score::WinIn(3) > Score::Value(1_000_000));
        assert!(Score::WinIn(3) > Score::LossIn(10));
        assert!(Score::Value(3) > Score::Value(2));
    }

    #[test]
    fn score_neg() {
        assert_eq!(-Score::WinIn(5), Score::LossIn(5));
        assert_eq!(-Score::LossIn(0), Score::WinIn(0));
        assert_eq!(-Score::Value(100), Score::Value(-100));
    }

    #[test]
    fn score_finite() {
        assert!(!Score::WinIn(3).is_finite());
        assert!(!Score::LossIn(3).is_finite());
        assert!(Score::Value(0).is_finite());
    }
}

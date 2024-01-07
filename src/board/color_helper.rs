use super::{COLOR_BLACK, COLOR_WHITE};

pub(crate) trait ColorHelper {
    fn wb<T>(&self, white: T, black: T) -> T;
    fn opponent(&self) -> Self;
}

impl ColorHelper for usize {
    fn wb<T>(&self, white: T, black: T) -> T {
        if *self == COLOR_WHITE {
            white
        } else {
            black
        }
    }

    fn opponent(&self) -> Self {
        self.wb(COLOR_BLACK, COLOR_WHITE)
    }
}

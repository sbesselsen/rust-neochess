pub trait BitwiseHelper {
    fn as_bit_index_iter(&self) -> BitIndexIterator;
    fn bit_at_index(&self, index: u32) -> bool;
    fn set_bit(&mut self, index: u32, value: bool);
    fn with_bit(&self, index: u32, value: bool) -> Self;
    fn move_bit(&mut self, index: u32, to_index: u32);
    fn shift_lr(&self, offset: i32) -> u64;
    fn discarding_shift_lr(&self, offset: i32) -> u64;
    fn from_bit(index: u32) -> u64;
    fn discarding_shl(&self, offset: u32) -> u64;
    fn discarding_shr(&self, offset: u32) -> u64;
}

impl BitwiseHelper for u64 {
    fn as_bit_index_iter(&self) -> BitIndexIterator {
        BitIndexIterator { n: *self }
    }
    fn bit_at_index(&self, index: u32) -> bool {
        debug_assert!(index < 64, "invalid index");
        *self & (1u64 << (63 - index)) > 0
    }
    fn set_bit(&mut self, index: u32, value: bool) {
        debug_assert!(index < 64, "invalid index");
        *self = if value {
            *self | (1u64 << (63 - index))
        } else {
            *self & (u64::MAX ^ (1u64 << (63 - index)))
        }
    }
    fn with_bit(&self, index: u32, value: bool) -> Self {
        let mut result = *self;
        result.set_bit(index, value);
        result
    }
    fn move_bit(&mut self, index: u32, to_index: u32) {
        self.set_bit(to_index, self.bit_at_index(index));
        self.set_bit(index, false);
    }
    fn shift_lr(&self, offset: i32) -> u64 {
        if offset < 0 {
            *self << -offset
        } else {
            *self >> offset
        }
    }
    fn discarding_shift_lr(&self, offset: i32) -> u64 {
        if (-63..=63).contains(&offset) {
            self.shift_lr(offset)
        } else {
            0
        }
    }
    fn from_bit(index: u32) -> u64 {
        debug_assert!(index < 64, "invalid index");
        1u64 << (63 - index)
    }
    fn discarding_shl(&self, offset: u32) -> u64 {
        if offset >= 64 {
            0u64
        } else {
            *self << offset
        }
    }
    fn discarding_shr(&self, offset: u32) -> u64 {
        if offset >= 64 {
            0u64
        } else {
            *self >> offset
        }
    }
}

pub struct BitIndexIterator {
    n: u64,
}

impl Iterator for BitIndexIterator {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.n == 0 {
            None
        } else {
            let result = self.n.leading_zeros();
            // Flip the first bit
            self.n ^= 1u64 << (63 - result);
            Some(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::BitwiseHelper;

    #[test]
    fn it_sets_bits() {
        let mut n: u64 = 1234;
        n.set_bit(4, true);
        let n_bit_4_toggled = n;

        assert!(n % 2048 == 1234);
        assert!(n > 1234);

        n.set_bit(5, true);
        assert!(n % 2048 == 1234);
        assert!(n == n_bit_4_toggled + (1u64 << 58));

        let n_bit_4_toggled_bit_5_untoggled = n.with_bit(5, false);
        assert!(n_bit_4_toggled_bit_5_untoggled == n_bit_4_toggled);

        let n_back_to_normal = n_bit_4_toggled_bit_5_untoggled.with_bit(4, false);
        assert_eq!(n_back_to_normal, 1234);
    }

    #[test]
    fn it_moves_bits() {
        let mut n: u64 = 1025;
        n.move_bit(63, 62);
        assert_eq!(n, 1026);
        n.move_bit(40, 43);
        assert_eq!(n, 1026);
        n.move_bit(53, 52);
        assert_eq!(n, 2050);
    }

    #[test]
    fn it_gets_bits() {
        assert!(129.bit_at_index(63));
        assert!(129.bit_at_index(56));
        assert!(!129.bit_at_index(3));
    }

    #[test]
    fn it_iterates_bits() {
        let bits: Vec<u32> = (65572u64).as_bit_index_iter().collect();
        assert_eq!(bits, vec![47, 58, 61]);
    }

    #[test]
    fn from_bit_works() {
        assert_eq!(u64::from_bit(53), 1024);
        assert_eq!(u64::from_bit(63), 1);
    }

    #[test]
    fn it_shifts_bits() {
        assert_eq!(1u64.shift_lr(-3), 8);
        assert_eq!(96u64.shift_lr(2), 24);
    }

    #[test]
    fn it_discard_shifts() {
        assert_eq!(8u64.discarding_shl(2), 32);
        assert_eq!(16u64.discarding_shl(0), 16);
        assert_eq!(20.discarding_shl(65), 0);

        assert_eq!(8u64.discarding_shr(2), 2);
        assert_eq!(16u64.discarding_shr(0), 16);
        assert_eq!(20.discarding_shr(65), 0);

        assert_eq!(8u64.discarding_shift_lr(-2), 32);
        assert_eq!(16u64.discarding_shift_lr(0), 16);
        assert_eq!(20.discarding_shift_lr(-65), 0);

        assert_eq!(8u64.discarding_shift_lr(2), 2);
        assert_eq!(16u64.discarding_shift_lr(0), 16);
        assert_eq!(20.discarding_shift_lr(65), 0);
    }
}

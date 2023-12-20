pub trait BitwiseHelper {
    fn as_bit_index_iter(&self) -> BitIndexIterator;
    fn bit_at_index(&self, index: u32) -> bool;
    fn set_bit(&mut self, index: u32, value: bool);
    fn with_bit(&self, index: u32, value: bool) -> Self;
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
}

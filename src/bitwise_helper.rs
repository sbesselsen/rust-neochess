pub trait BitwiseHelper {
    fn into_bit_index_iter(&self) -> BitIndexIterator;
    fn bit_at_index(&self, index: u32) -> bool;
    fn set_bit(&mut self, index: u32, value: bool);
    fn with_bit(&self, index: u32, value: bool) -> Self;
}

impl BitwiseHelper for u64 {
    fn into_bit_index_iter(&self) -> BitIndexIterator {
        BitIndexIterator { n: *self }
    }
    fn bit_at_index(&self, index: u32) -> bool {
        *self & (1u64 << (63 - index)) > 0
    }
    fn set_bit(&mut self, index: u32, value: bool) {
        *self = if value {
            *self | (1u64 << (63 - index))
        } else {
            *self & (u64::MAX ^ (1u64 << (63 - index)))
        }
    }
    fn with_bit(&self, index: u32, value: bool) -> Self {
        let mut result = self.clone();
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
            self.n = self.n ^ (1u64 << (63 - result));
            Some(result)
        }
    }
}

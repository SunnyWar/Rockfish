#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key(pub u64);

impl std::ops::BitXor<Key> for Key {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Key(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXorAssign<Key> for Key {
    fn bitxor_assign(&mut self, rhs: Key) {
        *self = *self ^ rhs;
    }
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:X}", self.0)
    }
}

#[cfg(test)]
mod key_tests {
    use crate::types::key::Key;

    #[test]
    fn test_bitxor() {
        let key1 = Key(0x1234);
        let key2 = Key(0x5678);
        let expected = Key(0x1234 ^ 0x5678);
        assert_eq!(key1 ^ key2, expected);
    }

    #[test]
    fn test_bitxor_assign() {
        let mut key1 = Key(0x1234);
        let key2 = Key(0x5678);
        key1 ^= key2;
        let expected = Key(0x1234 ^ 0x5678);
        assert_eq!(key1, expected);
    }

    #[test]
    fn test_display() {
        let key = Key(0x1234);
        assert_eq!(format!("{key}"), "1234");
    }
}



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bound(pub u32);

impl Bound {
    pub const NONE: Bound = Bound(0);
    pub const UPPER: Bound = Bound(1);
    pub const LOWER: Bound = Bound(2);
    pub const EXACT: Bound = Bound(3);
}

impl std::ops::BitAnd<Bound> for Bound {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Bound(self.0 & rhs.0)
    }
}

impl std::ops::BitOr<Bound> for Bound {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Bound(self.0 | rhs.0)
    }
}

impl std::cmp::PartialEq<u32> for Bound {
    fn eq(&self, rhs: &u32) -> bool {
        debug_assert!(*rhs == 0);
        self.0 == *rhs
    }
}

#[cfg(test)]
mod bound_tests {
    use super::*;

    #[test]
    fn test_bound_constants() {
        assert_eq!(Bound::NONE, Bound(0));
        assert_eq!(Bound::UPPER, Bound(1));
        assert_eq!(Bound::LOWER, Bound(2));
        assert_eq!(Bound::EXACT, Bound(3));
    }

    #[test]
    fn test_bitand() {
        assert_eq!(Bound::UPPER & Bound::LOWER, Bound(0));
        assert_eq!(Bound::UPPER & Bound::UPPER, Bound(1));
        assert_eq!(Bound::LOWER & Bound::LOWER, Bound(2));
        assert_eq!(Bound::EXACT & Bound::LOWER, Bound(2));
    }

    #[test]
    fn test_bitor() {
        assert_eq!(Bound::UPPER | Bound::LOWER, Bound(3));
        assert_eq!(Bound::UPPER | Bound::UPPER, Bound(1));
        assert_eq!(Bound::LOWER | Bound::LOWER, Bound(2));
        assert_eq!(Bound::NONE | Bound::EXACT, Bound(3));
    }

    #[test]
    fn test_partial_eq() {
        assert!(Bound::NONE == 0);
        assert!(Bound::UPPER != 0);
    }
}

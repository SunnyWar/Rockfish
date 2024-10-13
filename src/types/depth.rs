use super::MAX_PLY;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Depth(pub i32);

impl std::ops::Add<Depth> for Depth {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Depth(self.0 + rhs.0)
    }
}

impl std::ops::Sub<Depth> for Depth {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Depth(self.0 - rhs.0)
    }
}

impl std::ops::AddAssign<Depth> for Depth {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::SubAssign<Depth> for Depth {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl std::ops::Mul<i32> for Depth {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self {
        Depth(self.0 * rhs)
    }
}

impl std::ops::Mul<Depth> for i32 {
    type Output = Depth;
    fn mul(self, rhs: Depth) -> Depth {
        Depth(self * rhs.0)
    }
}

impl std::ops::Div<Depth> for Depth {
    type Output = i32;
    fn div(self, rhs: Depth) -> i32 {
        self.0 / rhs.0
    }
}

pub const ONE_PLY: Depth = Depth(1);

pub const DEPTH_ZERO: Depth = Depth(0);
pub const DEPTH_QS_CHECKS: Depth = Depth(0);
pub const DEPTH_QS_NO_CHECKS: Depth = Depth(-ONE_PLY.0);
pub const DEPTH_QS_RECAPTURES: Depth = Depth(-5 * ONE_PLY.0);

pub const DEPTH_NONE: Depth = Depth(-6 * ONE_PLY.0);
pub const DEPTH_MAX: Depth = Depth(MAX_PLY * ONE_PLY.0);

impl Depth {
    pub const ZERO: Depth = Depth(0);
    pub const QS_CHECKS: Depth = Depth(0);
    pub const QS_NO_CHECKS: Depth = Depth(-ONE_PLY.0);
    pub const QS_RECAPTURES: Depth = Depth(-5 * ONE_PLY.0);

    pub const NONE: Depth = Depth(-6 * ONE_PLY.0);
    pub const MAX: Depth = Depth(MAX_PLY * ONE_PLY.0);
}

#[cfg(test)]
mod depth_tests {
    use super::*;

    #[test]
    fn test_depth_constants() {
        assert_eq!(ONE_PLY, Depth(1));
        assert_eq!(DEPTH_ZERO, Depth(0));
        assert_eq!(DEPTH_QS_NO_CHECKS, Depth(-1));
        assert_eq!(DEPTH_QS_RECAPTURES, Depth(-5));
        assert_eq!(DEPTH_NONE, Depth(-6));
        // DEPTH_MAX can't be tested here because MAX_PLY is undefined in this snippet
    }

    #[test]
    fn test_add() {
        let d1 = Depth(3);
        let d2 = Depth(4);
        assert_eq!(d1 + d2, Depth(7));
    }

    #[test]
    fn test_sub() {
        let d1 = Depth(7);
        let d2 = Depth(4);
        assert_eq!(d1 - d2, Depth(3));
    }

    #[test]
    fn test_add_assign() {
        let mut d = Depth(3);
        d += Depth(4);
        assert_eq!(d, Depth(7));
    }

    #[test]
    fn test_sub_assign() {
        let mut d = Depth(7);
        d -= Depth(4);
        assert_eq!(d, Depth(3));
    }

    #[test]
    fn test_mul_i32() {
        let d = Depth(3);
        assert_eq!(d * 2, Depth(6));
    }

    #[test]
    fn test_mul_depth() {
        let d = Depth(3);
        assert_eq!(2 * d, Depth(6));
    }

    #[test]
    fn test_div() {
        let d1 = Depth(8);
        let d2 = Depth(2);
        assert_eq!(d1 / d2, 4);
    }
}

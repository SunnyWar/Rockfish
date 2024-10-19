use super::{Color, Square, WHITE};
use std::arch::x86_64::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Direction(pub i32);

impl std::ops::Neg for Direction {
    type Output = Self;
    fn neg(self) -> Self {
        Direction(-self.0)
    }
}

pub const NORTH: Direction = Direction(8);
pub const EAST: Direction = Direction(1);
pub const SOUTH: Direction = Direction(-8);
pub const WEST: Direction = Direction(-1);

pub const NORTH_EAST: Direction = Direction(9);
pub const NORTH_WEST: Direction = Direction(7);
pub const SOUTH_EAST: Direction = Direction(-7);
pub const SOUTH_WEST: Direction = Direction(-9);

impl std::ops::Add<Direction> for Direction {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        // the unsafe block replaces this: Direction(self.0 + rhs.0)
        unsafe {
            let left = _mm_set1_epi32(self.0);
            let right = _mm_set1_epi32(rhs.0);
            let result = _mm_add_epi32(left, right);
            Direction(_mm_extract_epi32(result, 0))
        }
    }
}

impl std::ops::Add<Direction> for Square {
    type Output = Square;
    fn add(self, rhs: Direction) -> Self {
        Square(u32::wrapping_add(self.0, rhs.0 as u32))
    }
}

impl std::ops::Sub<Direction> for Square {
    type Output = Square;
    fn sub(self, rhs: Direction) -> Self {
        Square(u32::wrapping_sub(self.0, rhs.0 as u32))
    }
}

impl std::ops::AddAssign<Direction> for Square {
    fn add_assign(&mut self, rhs: Direction) {
        *self = *self + rhs;
    }
}

impl std::ops::SubAssign<Direction> for Square {
    fn sub_assign(&mut self, rhs: Direction) {
        *self = *self - rhs;
    }
}

impl std::ops::Mul<Direction> for i32 {
    type Output = Direction;
    fn mul(self, rhs: Direction) -> Direction {
        Direction(self * rhs.0)
    }
}

pub fn pawn_push(c: Color) -> Direction {
    match c {
        WHITE => NORTH,
        _ => SOUTH,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BLACK, WHITE};

    #[test]
    fn test_neg() {
        assert_eq!(-NORTH, SOUTH);
        assert_eq!(-EAST, WEST);
        assert_eq!(-SOUTH, NORTH);
        assert_eq!(-WEST, EAST);
        assert_eq!(-NORTH_EAST, SOUTH_WEST);
        assert_eq!(-NORTH_WEST, SOUTH_EAST);
        assert_eq!(-SOUTH_EAST, NORTH_WEST);
        assert_eq!(-SOUTH_WEST, NORTH_EAST);
    }

    #[test]
    fn test_add() {
        assert_eq!(NORTH + EAST, Direction(9));
        assert_eq!(SOUTH + WEST, Direction(-9));
    }

    #[test]
    fn test_mul() {
        assert_eq!(2 * NORTH, Direction(16));
        assert_eq!(3 * EAST, Direction(3));
    }

    #[test]
    fn test_add_for_square() {
        let sq = Square(0);
        assert_eq!(sq + NORTH, Square(8));
        assert_eq!(sq + EAST, Square(1));
    }

    #[test]
    fn test_sub_for_square() {
        let sq = Square(8);
        assert_eq!(sq - NORTH, Square(0));
        assert_eq!(sq - EAST, Square(7));
    }

    #[test]
    fn test_add_assign_for_square() {
        let mut sq = Square(0);
        sq += NORTH;
        assert_eq!(sq, Square(8));
        sq += EAST;
        assert_eq!(sq, Square(9));
    }

    #[test]
    fn test_sub_assign_for_square() {
        let mut sq = Square(8);
        sq -= NORTH;
        assert_eq!(sq, Square(0));
        sq -= EAST;
        assert_eq!(sq, Square(u32::MAX)); // Using valid value within the range
    }

    #[test]
    fn test_pawn_push() {
        assert_eq!(pawn_push(WHITE), NORTH);
        assert_eq!(pawn_push(BLACK), SOUTH);
    }
}

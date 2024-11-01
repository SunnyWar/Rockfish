use super::{Color, Square, WHITE};
use std::arch::x86_64::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Direction(pub i32);

impl Direction {
    pub const NORTH: Direction = Direction(8);
    pub const EAST: Direction = Direction(1);
    pub const SOUTH: Direction = Direction(-8);
    pub const WEST: Direction = Direction(-1);
    
    pub const NORTH_EAST: Direction = Direction(9);
    pub const NORTH_WEST: Direction = Direction(7);
    pub const SOUTH_EAST: Direction = Direction(-7);
    pub const SOUTH_WEST: Direction = Direction(-9);
}

impl std::ops::Neg for Direction {
    type Output = Self;
    fn neg(self) -> Self {
        Direction(-self.0)
    }
}

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
        WHITE => Direction::NORTH,
        _ => Direction::SOUTH,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BLACK, WHITE};

    #[test]
    fn test_neg() {
        assert_eq!(-Direction::NORTH, Direction::SOUTH);
        assert_eq!(-Direction::EAST, Direction::WEST);
        assert_eq!(-Direction::SOUTH, Direction::NORTH);
        assert_eq!(-Direction::WEST, Direction::EAST);
        assert_eq!(-Direction::NORTH_EAST, Direction::SOUTH_WEST);
        assert_eq!(-Direction::NORTH_WEST, Direction::SOUTH_EAST);
        assert_eq!(-Direction::SOUTH_EAST, Direction::NORTH_WEST);
        assert_eq!(-Direction::SOUTH_WEST, Direction::NORTH_EAST);
    }

    #[test]
    fn test_add() {
        assert_eq!(Direction::NORTH + Direction::EAST, Direction(9));
        assert_eq!(Direction::SOUTH + Direction::WEST, Direction(-9));
    }

    #[test]
    fn test_mul() {
        assert_eq!(2 * Direction::NORTH, Direction(16));
        assert_eq!(3 * Direction::EAST, Direction(3));
    }

    #[test]
    fn test_add_for_square() {
        let sq = Square(0);
        assert_eq!(sq + Direction::NORTH, Square(8));
        assert_eq!(sq + Direction::EAST, Square(1));
    }

    #[test]
    fn test_sub_for_square() {
        let sq = Square(8);
        assert_eq!(sq - Direction::NORTH, Square(0));
        assert_eq!(sq - Direction::EAST, Square(7));
    }

    #[test]
    fn test_add_assign_for_square() {
        let mut sq = Square(0);
        sq += Direction::NORTH;
        assert_eq!(sq, Square(8));
        sq += Direction::EAST;
        assert_eq!(sq, Square(9));
    }

    #[test]
    fn test_sub_assign_for_square() {
        let mut sq = Square(8);
        sq -= Direction::NORTH;
        assert_eq!(sq, Square(0));
        sq -= Direction::EAST;
        assert_eq!(sq, Square(u32::MAX)); // Using valid value within the range
    }

    #[test]
    fn test_pawn_push() {
        assert_eq!(pawn_push(WHITE), Direction::NORTH);
        assert_eq!(pawn_push(BLACK), Direction::SOUTH);
    }
}

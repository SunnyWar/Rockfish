use super::{Color, Square};

#[derive(Clone, Copy, PartialEq, Eq)]
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
        Direction(self.0 + rhs.0)
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

// SPDX-License-Identifier: GPL-3.0-or-later

use direction::Direction;
pub mod bound;
pub mod depth;
pub mod direction;
pub mod key;
pub mod scale_factor;

pub const MAX_MOVES: usize = 256;
pub const MAX_PLY: i32 = 128;
pub const MAX_MATE_PLY: i32 = 128;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Color(pub u32);

impl Color {
    pub const WHITE: Color = Color(0);
    pub const BLACK: Color = Color(1);
}

impl std::ops::Not for Color {
    type Output = Color;
    fn not(self) -> Self {
        Color(self.0 ^ 1)
    }
}

impl std::ops::BitXor<bool> for Color {
    type Output = Self;
    fn bitxor(self, rhs: bool) -> Self {
        Color(self.0 ^ u32::from(rhs))
    }
}

impl IntoIterator for Color {
    type Item = Self;
    type IntoIter = ColorIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        ColorIntoIterator { color: self }
    }
}

pub struct ColorIntoIterator {
    color: Color,
}

impl Iterator for ColorIntoIterator {
    type Item = Color;

    fn next(&mut self) -> Option<Self::Item> {
        let sq = self.color.0;
        self.color.0 += 1;
        Some(Color(sq))
    }
}

pub struct White;
pub struct Black;

pub trait ColorTrait {
    type KingSide: CastlingRightTrait;
    type QueenSide: CastlingRightTrait;
    const COLOR: Color;
}

impl ColorTrait for White {
    type KingSide = WhiteOO;
    type QueenSide = WhiteOOO;
    const COLOR: Color = Color::WHITE;
}

impl ColorTrait for Black {
    type KingSide = BlackOO;
    type QueenSide = BlackOOO;
    const COLOR: Color = Color::BLACK;
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CastlingSide {
    KING,
    QUEEN,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastlingRight(pub u32);

impl CastlingRight {
    #[allow(dead_code)]
    pub const NO_CASTLING: CastlingRight = CastlingRight(0);
    pub const WHITE_OO: CastlingRight = CastlingRight(1);
    pub const WHITE_OOO: CastlingRight = CastlingRight(2);
    pub const BLACK_OO: CastlingRight = CastlingRight(4);
    pub const BLACK_OOO: CastlingRight = CastlingRight(8);
    pub const ANY_CASTLING: CastlingRight = CastlingRight(15);
}

pub trait CastlingRightTrait {
    const CR: CastlingRight;
}

pub struct WhiteOO;
pub struct WhiteOOO;
pub struct BlackOO;
pub struct BlackOOO;

impl CastlingRightTrait for WhiteOO {
    const CR: CastlingRight = CastlingRight::WHITE_OO;
}

impl CastlingRightTrait for WhiteOOO {
    const CR: CastlingRight = CastlingRight::WHITE_OOO;
}

impl CastlingRightTrait for BlackOO {
    const CR: CastlingRight = CastlingRight::BLACK_OO;
}

impl CastlingRightTrait for BlackOOO {
    const CR: CastlingRight = CastlingRight::BLACK_OOO;
}

impl CastlingRight {
    pub fn make(c: Color, cs: CastlingSide) -> CastlingRight {
        match (c, cs) {
            (Color::WHITE, CastlingSide::KING) => CastlingRight::WHITE_OO,
            (Color::WHITE, _) => CastlingRight::WHITE_OOO,
            (_, CastlingSide::KING) => CastlingRight::BLACK_OO,
            (_, _) => CastlingRight::BLACK_OOO,
        }
    }
}

impl std::ops::BitOr<CastlingSide> for Color {
    type Output = CastlingRight;
    fn bitor(self, rhs: CastlingSide) -> CastlingRight {
        CastlingRight(1u32 << ((rhs as u32) + 2 * self.0))
    }
}

impl std::ops::BitAnd<CastlingRight> for CastlingRight {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        CastlingRight(self.0 & rhs.0)
    }
}

impl std::ops::BitOr<CastlingRight> for CastlingRight {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        CastlingRight(self.0 | rhs.0)
    }
}

impl std::ops::BitAndAssign<CastlingRight> for CastlingRight {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl std::ops::BitOrAssign<CastlingRight> for CastlingRight {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl std::ops::Not for CastlingRight {
    type Output = CastlingRight;
    fn not(self) -> Self {
        CastlingRight(!self.0)
    }
}

impl std::cmp::PartialEq<u32> for CastlingRight {
    fn eq(&self, rhs: &u32) -> bool {
        debug_assert!(*rhs == 0);
        self.0 == *rhs
    }
}

pub type Phase = i32;

//pub const PHASE_ENDGAME: Phase = 0;
pub const PHASE_MIDGAME: Phase = 128;

pub const MG: usize = 0;
pub const EG: usize = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PieceType(pub u32);

impl PieceType {
    #[allow(dead_code)]
    pub const NO_PIECE_TYPE: PieceType = PieceType(0);
    pub const PAWN: PieceType = PieceType(1);
    pub const KNIGHT: PieceType = PieceType(2);
    pub const BISHOP: PieceType = PieceType(3);
    pub const ROOK: PieceType = PieceType(4);
    pub const QUEEN: PieceType = PieceType(5);
    pub const KING: PieceType = PieceType(6);
    pub const QUEEN_DIAGONAL: PieceType = PieceType(7);
    pub const ALL_PIECES: PieceType = PieceType(0);
}

pub struct Pawn;
pub struct Knight;
pub struct Bishop;
pub struct Rook;
pub struct Queen;
pub struct King;

pub trait PieceTypeTrait {
    const TYPE: PieceType;
}

impl PieceTypeTrait for Pawn {
    const TYPE: PieceType = PieceType::PAWN;
}

impl PieceTypeTrait for Knight {
    const TYPE: PieceType = PieceType::KNIGHT;
}

impl PieceTypeTrait for Bishop {
    const TYPE: PieceType = PieceType::BISHOP;
}

impl PieceTypeTrait for Rook {
    const TYPE: PieceType = PieceType::ROOK;
}

impl PieceTypeTrait for Queen {
    const TYPE: PieceType = PieceType::QUEEN;
}

impl PieceTypeTrait for King {
    const TYPE: PieceType = PieceType::KING;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece(pub u32);

impl Piece {
    pub const NO_PIECE: Piece = Piece(0);
    pub const W_PAWN: Piece = Piece(1);
    pub const W_BISHOP: Piece = Piece(3);
    pub const W_KING: Piece = Piece(6);
    pub const B_PAWN: Piece = Piece(9);
    pub const B_BISHOP: Piece = Piece(11);
    pub const B_KING: Piece = Piece(14);

    #[allow(dead_code)]
    pub const W_KNIGHT: Piece = Piece(2);
    #[allow(dead_code)]
    pub const W_ROOK: Piece = Piece(4);
    #[allow(dead_code)]
    pub const W_QUEEN: Piece = Piece(5);
    #[allow(dead_code)]
    pub const B_KNIGHT: Piece = Piece(10);
    #[allow(dead_code)]
    pub const B_ROOK: Piece = Piece(12);
    #[allow(dead_code)]
    pub const B_QUEEN: Piece = Piece(13);

    pub fn piece_type(self) -> PieceType {
        PieceType(self.0 & 7)
    }

    pub fn color(self) -> Color {
        Color(self.0 >> 3)
    }

    pub fn make(c: Color, pt: PieceType) -> Piece {
        Piece((c.0 << 3) + pt.0)
    }
}

impl IntoIterator for Piece {
    type Item = Self;
    type IntoIter = PieceIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        PieceIntoIterator { piece: self }
    }
}

pub struct PieceIntoIterator {
    piece: Piece,
}

impl Iterator for PieceIntoIterator {
    type Item = Piece;

    fn next(&mut self) -> Option<Self::Item> {
        let pc = self.piece.0;
        self.piece.0 += 1;
        Some(Piece(pc))
    }
}

impl std::ops::Not for Piece {
    type Output = Self;
    fn not(self) -> Self {
        Piece(self.0 ^ 8)
    }
}

impl std::ops::BitXor<bool> for Piece {
    type Output = Self;
    fn bitxor(self, rhs: bool) -> Self {
        Piece(self.0 ^ (u32::from(rhs) << 3))
    }
}

pub type File = u32;
pub type Rank = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Square(pub u32);

impl Square {
    pub const FILE_A: File = 0;
    pub const FILE_B: File = 1;
    pub const FILE_C: File = 2;
    pub const FILE_D: File = 3;
    pub const FILE_E: File = 4;
    //pub const FILE_F: File = 5;
    pub const FILE_G: File = 6;
    pub const FILE_H: File = 7;

    pub const RANK_1: Rank = 0;
    pub const RANK_2: Rank = 1;
    pub const RANK_3: Rank = 2;
    pub const RANK_4: Rank = 3;
    pub const RANK_5: Rank = 4;
    pub const RANK_6: Rank = 5;
    pub const RANK_7: Rank = 6;
    pub const RANK_8: Rank = 7;
}

pub fn relative_rank(c: Color, r: Rank) -> Rank {
    r ^ (c.0 * 7)
}

impl Square {
    pub const A1: Square = Square(0);
    pub const B1: Square = Square(1);
    pub const C1: Square = Square(2);
    pub const D1: Square = Square(3);
    pub const E1: Square = Square(4);
    pub const F1: Square = Square(5);
    pub const G1: Square = Square(6);
    pub const H1: Square = Square(7);
    pub const A2: Square = Square(8);
    pub const B2: Square = Square(9);
    pub const C2: Square = Square(10);
    pub const D2: Square = Square(11);
    pub const E2: Square = Square(12);
    pub const F2: Square = Square(13);
    pub const G2: Square = Square(14);
    pub const H2: Square = Square(15);
    pub const A3: Square = Square(16);
    pub const B3: Square = Square(17);
    pub const C3: Square = Square(18);
    pub const D3: Square = Square(19);
    pub const E3: Square = Square(20);
    pub const F3: Square = Square(21);
    pub const G3: Square = Square(22);
    pub const H3: Square = Square(23);
    pub const A4: Square = Square(24);
    pub const B4: Square = Square(25);
    pub const C4: Square = Square(26);
    pub const D4: Square = Square(27);
    pub const E4: Square = Square(28);
    pub const F4: Square = Square(29);
    pub const G4: Square = Square(30);
    pub const H4: Square = Square(31);
    pub const A5: Square = Square(32);
    pub const B5: Square = Square(33);
    pub const C5: Square = Square(34);
    pub const D5: Square = Square(35);
    pub const E5: Square = Square(36);
    pub const F5: Square = Square(37);
    pub const G5: Square = Square(38);
    pub const H5: Square = Square(39);
    pub const A6: Square = Square(40);
    pub const B6: Square = Square(41);
    pub const C6: Square = Square(42);
    pub const D6: Square = Square(43);
    pub const E6: Square = Square(44);
    pub const F6: Square = Square(45);
    pub const G6: Square = Square(46);
    pub const H6: Square = Square(47);
    pub const A7: Square = Square(48);
    pub const B7: Square = Square(49);
    pub const C7: Square = Square(50);
    pub const D7: Square = Square(51);
    pub const E7: Square = Square(52);
    pub const F7: Square = Square(53);
    pub const G7: Square = Square(54);
    pub const H7: Square = Square(55);
    pub const A8: Square = Square(56);
    pub const B8: Square = Square(57);
    pub const C8: Square = Square(58);
    pub const D8: Square = Square(59);
    pub const E8: Square = Square(60);
    pub const F8: Square = Square(61);
    pub const G8: Square = Square(62);
    pub const H8: Square = Square(63);

    pub const NONE: Square = Square(64);

    pub fn file(self) -> File {
        self.0 & 7
    }

    pub fn rank(self) -> Rank {
        self.0 >> 3
    }

    pub fn relative(self, c: Color) -> Self {
        Square(self.0 ^ (c.0 * 56))
    }

    pub fn relative_rank(self, c: Color) -> Rank {
        relative_rank(c, self.rank())
    }

    pub fn is_ok(self) -> bool {
        self >= Square::A1 && self <= Square::H8
    }

    pub fn make(f: File, r: Rank) -> Square {
        Square((r << 3) | f)
    }
}

pub fn relative_square(c: Color, s: Square) -> Square {
    s.relative(c)
}

impl std::ops::Not for Square {
    type Output = Self;
    fn not(self) -> Self {
        Square(self.0 ^ Square::A8.0)
    }
}

impl std::ops::BitXor<bool> for Square {
    type Output = Self;
    fn bitxor(self, rhs: bool) -> Self {
        Square(self.0 ^ if rhs { 0x38 } else { 0 })
    }
}

impl IntoIterator for Square {
    type Item = Self;
    type IntoIter = SquareIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        SquareIntoIterator { square: self }
    }
}

pub struct SquareIntoIterator {
    square: Square,
}

impl Iterator for SquareIntoIterator {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        let sq = self.square.0;
        self.square.0 += 1;
        Some(Square(sq))
    }
}

#[derive(Clone, Copy)]
pub struct Squares {
    pub start: Square,
    pub end: Square,
}

impl IntoIterator for Squares {
    type Item = Square;
    type IntoIter = SquaresIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        SquaresIntoIterator {
            current: self.start,
            end: self.end,
        }
    }
}

pub struct SquaresIntoIterator {
    current: Square,
    end: Square,
}

#[allow(clippy::if_not_else)] // s!= self.end usually true
impl Iterator for SquaresIntoIterator {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        let s = self.current;
        if s != self.end {
            self.current += Direction(1); // or however you increment a Square
            Some(s)
        } else {
            None
        }
    }
}

pub struct SquareList<'a> {
    list: &'a [Square],
    idx: usize,
}

impl<'a> SquareList<'a> {
    pub fn construct(list: &'a [Square]) -> SquareList<'a> {
        SquareList { list, idx: 0 }
    }
}

#[allow(clippy::if_not_else)] // s!= Square::NONE is usually true
impl Iterator for SquareList<'_> {
    type Item = Square;
    fn next(&mut self) -> Option<Self::Item> {
        let s = self.list[self.idx];
        if s != Square::NONE {
            self.idx += 1;
            Some(s)
        } else {
            None
        }
    }
}

pub fn opposite_colors(s1: Square, s2: Square) -> bool {
    let s = s1.0 ^ s2.0;
    (((s >> 3) ^ s) & 1) != 0
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct MoveType(pub u32);

impl MoveType {
    pub const NORMAL: MoveType = MoveType(0);
    pub const PROMOTION: MoveType = MoveType(1 << 14);
    pub const ENPASSANT: MoveType = MoveType(2 << 14);
    pub const CASTLING: MoveType = MoveType(3 << 14);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Move(pub u32);

impl Move {
    pub const NONE: Move = Move(0);
    pub const NULL: Move = Move(65);

    pub fn from(self) -> Square {
        Square((self.0 >> 6) & 0x3f)
    }

    pub fn to(self) -> Square {
        Square(self.0 & 0x3f)
    }

    pub fn from_to(self) -> u32 {
        self.0 & 0xfff
    }

    pub fn move_type(self) -> MoveType {
        MoveType(self.0 & (3 << 14))
    }

    pub fn promotion_type(self) -> PieceType {
        PieceType(((self.0 >> 12) & 3) + PieceType::KNIGHT.0)
    }

    pub fn is_ok(self) -> bool {
        self.from() != self.to()
    }

    pub fn make(from: Square, to: Square) -> Move {
        Move((from.0 << 6) + to.0)
    }

    pub fn make_prom(from: Square, to: Square, pt: PieceType) -> Move {
        Move(MoveType::PROMOTION.0 + ((pt.0 - PieceType::KNIGHT.0) << 12) + (from.0 << 6) + to.0)
    }

    pub fn make_special(mt: MoveType, from: Square, to: Square) -> Move {
        Move(mt.0 + (from.0 << 6) + to.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(pub i32);

impl Value {
    pub const ZERO: Value = Value(0);
    pub const DRAW: Value = Value(0);
    pub const KNOWN_WIN: Value = Value(10000);
    pub const MATE: Value = Value(32000);
    pub const INFINITE: Value = Value(32001);
    pub const NONE: Value = Value(32002);

    pub const MATE_IN_MAX_PLY: Value = Value(Value::MATE.0 - MAX_MATE_PLY - MAX_PLY);
    pub const MATED_IN_MAX_PLY: Value = Value(-Value::MATE.0 + MAX_MATE_PLY + MAX_PLY);

    // two separate values: PawnValueEg (Effective Gains) and PawnValueMg (Material Gains)
    // PawnValueEg is used for evaluating positions where the side to move had a material advantage
    // while PawnValueMg was used when the side to move was behind in material
    #[allow(non_upper_case_globals)]
    pub const PawnValueMg: Value = Value(171);
    #[allow(non_upper_case_globals)]
    pub const KnightValueMg: Value = Value(764);
    #[allow(non_upper_case_globals)]
    pub const BishopValueMg: Value = Value(826);
    #[allow(non_upper_case_globals)]
    pub const RookValueMg: Value = Value(1282);
    #[allow(non_upper_case_globals)]
    pub const QueenValueMg: Value = Value(2526);

    #[allow(non_upper_case_globals)]
    pub const PawnValueEg: Value = Value(240);
    #[allow(non_upper_case_globals)]
    pub const KnightValueEg: Value = Value(848);
    #[allow(non_upper_case_globals)]
    pub const BishopValueEg: Value = Value(891);
    #[allow(non_upper_case_globals)]
    pub const RookValueEg: Value = Value(1373);
    #[allow(non_upper_case_globals)]
    pub const QueenValueEg: Value = Value(2646);

    pub fn abs(self) -> Value {
        Value(self.0.abs())
    }
}

pub const MIDGAME_LIMIT: Value = Value(15258);
pub const ENDGAME_LIMIT: Value = Value(3915);

const PIECE_VALUE: [[Value; 16]; 2] = [
    [
        Value::ZERO,
        Value::PawnValueMg,
        Value::KnightValueMg,
        Value::BishopValueMg,
        Value::RookValueMg,
        Value::QueenValueMg,
        Value::ZERO,
        Value::ZERO,
        Value::ZERO,
        Value::PawnValueMg,
        Value::KnightValueMg,
        Value::BishopValueMg,
        Value::RookValueMg,
        Value::QueenValueMg,
        Value::ZERO,
        Value::ZERO,
    ],
    [
        Value::ZERO,
        Value::PawnValueEg,
        Value::KnightValueEg,
        Value::BishopValueEg,
        Value::RookValueEg,
        Value::QueenValueEg,
        Value::ZERO,
        Value::ZERO,
        Value::ZERO,
        Value::PawnValueEg,
        Value::KnightValueEg,
        Value::BishopValueEg,
        Value::RookValueEg,
        Value::QueenValueEg,
        Value::ZERO,
        Value::ZERO,
    ],
];

pub fn piece_value(phase: usize, pc: Piece) -> Value {
    PIECE_VALUE[phase][pc.0 as usize]
}

impl std::ops::Neg for Value {
    type Output = Self;
    fn neg(self) -> Self {
        Value(-self.0)
    }
}

impl std::ops::Add<Value> for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Value(self.0 + rhs.0)
    }
}

impl std::ops::Add<i32> for Value {
    type Output = Self;
    fn add(self, rhs: i32) -> Self {
        self + Value(rhs)
    }
}

impl std::ops::Sub<i32> for Value {
    type Output = Self;
    fn sub(self, rhs: i32) -> Self {
        self - Value(rhs)
    }
}

impl std::ops::Sub<Value> for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Value(self.0 - rhs.0)
    }
}

impl std::ops::AddAssign<Value> for Value {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::AddAssign<i32> for Value {
    fn add_assign(&mut self, rhs: i32) {
        *self = *self + rhs;
    }
}

impl std::ops::SubAssign<Value> for Value {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl std::ops::SubAssign<i32> for Value {
    fn sub_assign(&mut self, rhs: i32) {
        *self = *self - rhs;
    }
}

impl std::ops::Mul<i32> for Value {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self {
        Value(self.0 * rhs)
    }
}

impl std::ops::MulAssign<i32> for Value {
    fn mul_assign(&mut self, rhs: i32) {
        *self = *self * rhs;
    }
}

impl std::ops::Mul<Value> for i32 {
    type Output = Value;
    fn mul(self, rhs: Value) -> Value {
        Value(self * rhs.0)
    }
}

impl std::ops::Div<i32> for Value {
    type Output = Self;
    fn div(self, rhs: i32) -> Self {
        Value(self.0 / rhs)
    }
}

impl std::ops::DivAssign<i32> for Value {
    fn div_assign(&mut self, rhs: i32) {
        *self = *self / rhs;
    }
}

impl std::ops::Div<Value> for Value {
    type Output = i32;
    fn div(self, rhs: Self) -> i32 {
        self.0 / rhs.0
    }
}

pub fn mate_in(ply: i32) -> Value {
    Value::MATE - ply
}

pub fn mated_in(ply: i32) -> Value {
    -Value::MATE + ply
}

#[derive(Clone, Copy)]
pub struct Score(pub i32);

impl Score {
    pub const ZERO: Score = Score(0);

    pub fn eg(self) -> Value {
        Value(i32::from(((self.0 + 0x8000) >> 16) as i16))
    }

    pub fn mg(self) -> Value {
        Value(i32::from(self.0 as i16))
    }

    pub fn make(mg: i32, eg: i32) -> Self {
        Score((eg << 16) + mg)
    }
}

impl std::ops::Add<Score> for Score {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Score(self.0 + rhs.0)
    }
}

impl std::ops::AddAssign<Score> for Score {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub<Score> for Score {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Score(self.0 - rhs.0)
    }
}

impl std::ops::SubAssign<Score> for Score {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl std::ops::Neg for Score {
    type Output = Self;
    fn neg(self) -> Self {
        Score(-self.0)
    }
}

impl std::ops::Mul<i32> for Score {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self {
        Score::make(rhs * self.mg().0, rhs * self.eg().0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod color_tests {
        use super::*;

        #[test]
        fn test_not() {
            assert_eq!(!Color::WHITE, Color::BLACK);
            assert_eq!(!Color::BLACK, Color::WHITE);
        }

        #[test]
        fn test_bitxor() {
            assert_eq!(Color::WHITE ^ true, Color::BLACK);
            assert_eq!(Color::BLACK ^ true, Color::WHITE);
            assert_eq!(Color::WHITE ^ false, Color::WHITE);
            assert_eq!(Color::BLACK ^ false, Color::BLACK);
        }

        #[test]
        fn test_into_iterator() {
            let color = Color::WHITE;
            let mut iter = color.into_iter();
            assert_eq!(iter.next(), Some(Color::WHITE));
            assert_eq!(iter.next(), Some(Color(1)));
            assert_eq!(iter.next(), Some(Color(2)));
        }

        #[test]
        fn test_trait() {
            assert_eq!(White::COLOR, Color::WHITE);
            assert_eq!(Black::COLOR, Color::BLACK);
        }
    }

    mod white_black_tests {
        use super::*;

        #[test]
        fn test_color() {
            assert_eq!(White::COLOR, Color::WHITE);
            assert_eq!(Black::COLOR, Color::BLACK);
        }

        #[test]
        fn test_castling_sides() {
            type W = crate::types::White;
            type B = crate::types::Black;

            assert_eq!(
                <W as crate::types::ColorTrait>::KingSide::CR,
                CastlingRight::WHITE_OO
            );
            assert_eq!(
                <W as crate::types::ColorTrait>::QueenSide::CR,
                CastlingRight::WHITE_OOO
            );

            assert_eq!(
                <B as crate::types::ColorTrait>::KingSide::CR,
                CastlingRight::BLACK_OO
            );
            assert_eq!(
                <B as crate::types::ColorTrait>::QueenSide::CR,
                CastlingRight::BLACK_OOO
            );
        }
    }

    mod castling_right_tests {
        use super::*;

        #[test]
        fn test_values() {
            assert_eq!(CastlingRight::NO_CASTLING, CastlingRight(0));
            assert_eq!(CastlingRight::WHITE_OO, CastlingRight(1));
            assert_eq!(CastlingRight::WHITE_OOO, CastlingRight(2));
            assert_eq!(CastlingRight::BLACK_OO, CastlingRight(4));
            assert_eq!(CastlingRight::BLACK_OOO, CastlingRight(8));
            assert_eq!(CastlingRight::ANY_CASTLING, CastlingRight(15));
        }

        #[test]
        fn test_trait() {
            assert_eq!(WhiteOO::CR, CastlingRight::WHITE_OO);
            assert_eq!(WhiteOOO::CR, CastlingRight::WHITE_OOO);
            assert_eq!(BlackOO::CR, CastlingRight::BLACK_OO);
            assert_eq!(BlackOOO::CR, CastlingRight::BLACK_OOO);
        }

        #[test]
        fn test_make() {
            assert_eq!(
                CastlingRight::make(Color::WHITE, CastlingSide::KING),
                CastlingRight::WHITE_OO
            );
            assert_eq!(
                CastlingRight::make(Color::WHITE, CastlingSide::QUEEN),
                CastlingRight::WHITE_OOO
            );
            assert_eq!(
                CastlingRight::make(Color::BLACK, CastlingSide::KING),
                CastlingRight::BLACK_OO
            );
            assert_eq!(
                CastlingRight::make(Color::BLACK, CastlingSide::QUEEN),
                CastlingRight::BLACK_OOO
            );
        }

        #[test]
        fn test_bit_ops() {
            assert_eq!(
                Color::WHITE | crate::types::CastlingSide::KING,
                CastlingRight::WHITE_OO
            );
            assert_eq!(
                Color::BLACK | crate::types::CastlingSide::QUEEN,
                CastlingRight::BLACK_OOO
            );

            let cr1 = CastlingRight::WHITE_OO;
            let cr2 = CastlingRight::BLACK_OO;
            let mut combined = cr1 | cr2;
            assert_eq!(combined, CastlingRight(5));

            combined &= CastlingRight::WHITE_OO;
            assert_eq!(combined, CastlingRight::WHITE_OO);

            combined |= CastlingRight::BLACK_OOO;
            assert_eq!(combined, CastlingRight(9));
        }

        #[test]
        fn test_not() {
            assert_eq!(!CastlingRight::NO_CASTLING, CastlingRight(!0));
        }

        #[test]
        fn test_partial_eq() {
            assert!(CastlingRight(0) == 0);
            assert!(CastlingRight(1) != 0);
        }
    }

    mod piece_type_tests {
        use super::*;

        #[test]
        fn test_piece_type_constants() {
            assert_eq!(PieceType::NO_PIECE_TYPE, PieceType(0));
            assert_eq!(PieceType::PAWN, PieceType(1));
            assert_eq!(PieceType::KNIGHT, PieceType(2));
            assert_eq!(PieceType::BISHOP, PieceType(3));
            assert_eq!(PieceType::ROOK, PieceType(4));
            assert_eq!(PieceType::QUEEN, PieceType(5));
            assert_eq!(PieceType::KING, PieceType(6));
            assert_eq!(PieceType::QUEEN_DIAGONAL, PieceType(7));
            assert_eq!(PieceType::ALL_PIECES, PieceType(0));
        }

        #[test]
        fn test_piece_type_trait() {
            assert_eq!(Pawn::TYPE, PieceType::PAWN);
            assert_eq!(Knight::TYPE, PieceType::KNIGHT);
            assert_eq!(Bishop::TYPE, PieceType::BISHOP);
            assert_eq!(Rook::TYPE, PieceType::ROOK);
            assert_eq!(Queen::TYPE, PieceType::QUEEN);
            assert_eq!(King::TYPE, PieceType::KING);
        }

        #[test]
        fn test_clone() {
            let pt = PieceType::PAWN;
            let cloned = pt;
            assert_eq!(pt, cloned);
        }

        #[test]
        fn test_copy() {
            let pt = PieceType::QUEEN;
            let copied = pt;
            assert_eq!(pt, copied);
        }

        #[test]
        fn test_partial_eq() {
            let pt1 = PieceType::KNIGHT;
            let pt2 = PieceType::KNIGHT;
            let pt3 = PieceType::BISHOP;
            assert_eq!(pt1, pt2);
            assert_ne!(pt1, pt3);
        }
    }

    mod piece_tests {
        use super::*;

        #[test]
        fn test_piece_constants() {
            assert_eq!(Piece::NO_PIECE, Piece(0));
            assert_eq!(Piece::W_PAWN, Piece(1));
            assert_eq!(Piece::W_KNIGHT, Piece(2));
            assert_eq!(Piece::W_BISHOP, Piece(3));
            assert_eq!(Piece::W_ROOK, Piece(4));
            assert_eq!(Piece::W_QUEEN, Piece(5));
            assert_eq!(Piece::W_KING, Piece(6));
            assert_eq!(Piece::B_PAWN, Piece(9));
            assert_eq!(Piece::B_KNIGHT, Piece(10));
            assert_eq!(Piece::B_BISHOP, Piece(11));
            assert_eq!(Piece::B_ROOK, Piece(12));
            assert_eq!(Piece::B_QUEEN, Piece(13));
            assert_eq!(Piece::B_KING, Piece(14));
        }

        #[test]
        fn test_piece_type() {
            assert_eq!(Piece::W_PAWN.piece_type(), PieceType(1));
            assert_eq!(Piece::W_KNIGHT.piece_type(), PieceType(2));
            assert_eq!(Piece::W_BISHOP.piece_type(), PieceType(3));
        }

        #[test]
        fn test_color() {
            assert_eq!(Piece::W_PAWN.color(), Color::WHITE);
            assert_eq!(Piece::B_PAWN.color(), Color::BLACK);
        }

        #[test]
        fn test_make() {
            assert_eq!(Piece::make(Color::WHITE, PieceType(1)), Piece::W_PAWN);
            assert_eq!(Piece::make(Color::BLACK, PieceType(1)), Piece::B_PAWN);
        }

        #[test]
        fn test_piece_into_iterator() {
            let piece = Piece::W_PAWN;
            let mut iter = piece.into_iter();
            assert_eq!(iter.next(), Some(Piece::W_PAWN));
            assert_eq!(iter.next(), Some(Piece::W_KNIGHT));
            assert_eq!(iter.next(), Some(Piece::W_BISHOP));
        }

        #[test]
        fn test_not() {
            assert_eq!(!Piece::W_PAWN, Piece::B_PAWN);
            assert_eq!(!Piece::B_PAWN, Piece::W_PAWN);
        }

        #[test]
        fn test_bitxor() {
            assert_eq!(Piece::W_PAWN ^ true, Piece::B_PAWN);
            assert_eq!(Piece::B_PAWN ^ true, Piece::W_PAWN);
            assert_eq!(Piece::W_PAWN ^ false, Piece::W_PAWN);
            assert_eq!(Piece::B_PAWN ^ false, Piece::B_PAWN);
        }
    }

    mod square_tests {
        use super::*;

        #[test]
        fn test_square_constants() {
            assert_eq!(Square::A1, Square(0));
            assert_eq!(Square::H8, Square(63));
            assert_eq!(Square::NONE, Square(64));
        }

        #[test]
        fn test_file() {
            assert_eq!(Square::A1.file(), Square::FILE_A);
            assert_eq!(Square::H1.file(), Square::FILE_H);
            assert_eq!(Square::A8.file(), Square::FILE_A);
            assert_eq!(Square::H8.file(), Square::FILE_H);
        }

        #[test]
        fn test_rank() {
            assert_eq!(Square::A1.rank(), Square::RANK_1);
            assert_eq!(Square::A8.rank(), Square::RANK_8);
            assert_eq!(Square::H1.rank(), Square::RANK_1);
            assert_eq!(Square::H8.rank(), Square::RANK_8);
        }

        #[test]
        fn test_relative() {
            assert_eq!(Square::A1.relative(Color::WHITE), Square::A1);
            assert_eq!(Square::A1.relative(Color::BLACK), Square::A8);
            assert_eq!(Square::H1.relative(Color::WHITE), Square::H1);
            assert_eq!(Square::H1.relative(Color::BLACK), Square::H8);
        }

        #[test]
        fn test_relative_rank() {
            assert_eq!(Square::A1.relative_rank(Color::WHITE), Square::RANK_1);
            assert_eq!(Square::A1.relative_rank(Color::BLACK), Square::RANK_8);
            assert_eq!(Square::H1.relative_rank(Color::WHITE), Square::RANK_1);
            assert_eq!(Square::H1.relative_rank(Color::BLACK), Square::RANK_8);
        }

        #[test]
        fn test_is_ok() {
            assert!(Square::A1.is_ok());
            assert!(Square::H8.is_ok());
            assert!(!Square::NONE.is_ok());
        }

        #[test]
        fn test_make() {
            assert_eq!(Square::make(Square::FILE_A, Square::RANK_1), Square::A1);
            assert_eq!(Square::make(Square::FILE_H, Square::RANK_8), Square::H8);
        }

        #[test]
        fn test_relative_square() {
            assert_eq!(relative_square(Color::WHITE, Square::A1), Square::A1);
            assert_eq!(relative_square(Color::BLACK, Square::A1), Square::A8);
        }

        #[test]
        fn test_not() {
            assert_eq!(!Square::A1, Square(56));
            assert_eq!(!Square::H8, Square(7));
        }

        #[test]
        fn test_bitxor() {
            assert_eq!(Square::A1 ^ true, Square(56));
            assert_eq!(Square::H8 ^ true, Square(7));
        }

        #[test]
        fn test_square_into_iterator() {
            let square = Square::A1;
            let mut iter = square.into_iter();
            assert_eq!(iter.next(), Some(Square::A1));
            assert_eq!(iter.next(), Some(Square::B1));
            assert_eq!(iter.next(), Some(Square::C1));
        }
    }
}

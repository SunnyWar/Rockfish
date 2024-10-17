// SPDX-License-Identifier: GPL-3.0-or-later

#![allow(dead_code)]

use crate::types::{
    direction::Direction, direction::EAST, direction::NORTH, direction::NORTH_EAST,
    direction::NORTH_WEST, direction::SOUTH, direction::SOUTH_EAST, direction::SOUTH_WEST,
    direction::WEST, Color, File, PieceType, Rank, Square, BISHOP, BLACK, FILE_A, FILE_H, KING,
    KNIGHT, PAWN, QUEEN, ROOK, WHITE,
};
use crate::uci;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Bitboard(pub u64);

pub fn popcount(bb: Bitboard) -> u32 {
    bb.0.count_ones()
}

pub const ALL_SQUARES: Bitboard = Bitboard(!0u64);
pub const DARK_SQUARES: Bitboard = Bitboard(0xaa55_aa55_aa55_aa55);

pub const FILEA_BB: Bitboard = Bitboard(0x0101_0101_0101_0101);
pub const FILEB_BB: Bitboard = Bitboard(0x0202_0202_0202_0202);
pub const FILEC_BB: Bitboard = Bitboard(0x0404_0404_0404_0404);
pub const FILED_BB: Bitboard = Bitboard(0x0808_0808_0808_0808);
pub const FILEE_BB: Bitboard = Bitboard(0x1010_1010_1010_1010);
pub const FILEF_BB: Bitboard = Bitboard(0x2020_2020_2020_2020);
pub const FILEG_BB: Bitboard = Bitboard(0x4040_4040_4040_4040);
pub const FILEH_BB: Bitboard = Bitboard(0x8080_8080_8080_8080);

pub const RANK1_BB: Bitboard = Bitboard(0xff);
pub const RANK2_BB: Bitboard = Bitboard(0xff00);
pub const RANK3_BB: Bitboard = Bitboard(0x00ff_0000);
pub const RANK4_BB: Bitboard = Bitboard(0xff00_0000);
pub const RANK5_BB: Bitboard = Bitboard(0x00ff_0000_0000);
pub const RANK6_BB: Bitboard = Bitboard(0xff00_0000_0000);
pub const RANK7_BB: Bitboard = Bitboard(0x00ff_0000_0000_0000);
pub const RANK8_BB: Bitboard = Bitboard(0xff00_0000_0000_0000);

static mut SQUARE_DISTANCE: [u32; 64 * 64] = [0; 64 * 64];

static mut SQUARE_BB: [Bitboard; 64] = [Bitboard(0); 64];
static mut FILE_BB: [Bitboard; 8] = [Bitboard(0); 8];
static mut RANK_BB: [Bitboard; 8] = [Bitboard(0); 8];
static mut ADJACENT_FILES_BB: [Bitboard; 8] = [Bitboard(0); 8];
static mut FORWARD_RANKS_BB: [[Bitboard; 8]; 2] = [[Bitboard(0); 8]; 2];
static mut BETWEEN_BB: [[Bitboard; 64]; 64] = [[Bitboard(0); 64]; 64];
static mut LINE_BB: [[Bitboard; 64]; 64] = [[Bitboard(0); 64]; 64];
static mut DISTANCE_RING_BB: [[Bitboard; 8]; 64] = [[Bitboard(0); 8]; 64];
static mut FORWARD_FILE_BB: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
static mut PASSED_PAWN_MASK: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
static mut PAWN_ATTACK_SPAN: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
static mut PSEUDO_ATTACKS: [[Bitboard; 64]; 8] = [[Bitboard(0); 64]; 8];
static mut PAWN_ATTACKS: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];

struct Magics {
    masks: [Bitboard; 64],
    magics: [u64; 64],
    attacks: [&'static [Bitboard]; 64],
}

static mut ROOK_MAGICS: Magics = Magics {
    masks: [Bitboard(0); 64],
    magics: [0; 64],
    attacks: [&[]; 64],
};

static mut BISHOP_MAGICS: Magics = Magics {
    masks: [Bitboard(0); 64],
    magics: [0; 64],
    attacks: [&[]; 64],
};

static mut ATTACKS_TABLE: [Bitboard; 88772] = [Bitboard(0); 88772];

struct MagicInit {
    magic: u64,
    index: u32,
}

macro_rules! M {
    ($x:expr, $y:expr) => {
        MagicInit {
            magic: $x,
            index: $y,
        }
    };
}

const BISHOP_INIT: [MagicInit; 64] = [
    M!(0x007f_bfbf_bfbf_bfff, 5378),
    M!(0x0000_a060_4010_07fc, 4093),
    M!(0x0001_0040_0802_0000, 4314),
    M!(0x0000_8060_0400_0000, 6587),
    M!(0x0000_1004_0000_0000, 6491),
    M!(0x0000_21c1_00b2_0000, 6330),
    M!(0x0000_0400_4100_8000, 5609),
    M!(0x0000_0fb0_203f_ff80, 22236),
    M!(0x0000_0401_0040_1004, 6106),
    M!(0x0000_0200_8020_0802, 5625),
    M!(0x0000_0040_1020_2000, 16785),
    M!(0x0000_0080_6004_0000, 16817),
    M!(0x0000_0044_0200_0000, 6842),
    M!(0x0000_0008_0100_8000, 7003),
    M!(0x0000_07ef_e0bf_ff80, 4197),
    M!(0x0000_0008_2082_0020, 7356),
    M!(0x0000_4000_8080_8080, 4602),
    M!(0x0002_1f01_0040_0808, 4538),
    M!(0x0001_8000_c06f_3fff, 29531),
    M!(0x0000_2582_0080_1000, 45393),
    M!(0x0000_2400_8084_0000, 12420),
    M!(0x0000_1800_0c03_fff8, 15763),
    M!(0x0000_0a58_4020_8020, 5050),
    M!(0x0000_0200_0820_8020, 4346),
    M!(0x0000_8040_0081_0100, 6074),
    M!(0x0001_0119_0080_2008, 7866),
    M!(0x0000_8040_0081_0100, 32139),
    M!(0x0001_0040_3c04_03ff, 57673),
    M!(0x0007_8402_a880_2000, 55365),
    M!(0x0000_1010_0080_4400, 15818),
    M!(0x0000_0808_0010_4100, 5562),
    M!(0x0000_4004_c008_2008, 6390),
    M!(0x0001_0101_2000_8020, 7930),
    M!(0x0000_8080_9a00_4010, 13329),
    M!(0x0007_fefe_0881_0010, 7170),
    M!(0x0003_ff0f_833f_c080, 27267),
    M!(0x007f_e080_1900_3042, 53787),
    M!(0x003f_ffef_ea00_3000, 5097),
    M!(0x0000_1010_1000_2080, 6643),
    M!(0x0000_8020_0508_0804, 6138),
    M!(0x0000_8080_80a8_0040, 7418),
    M!(0x0000_1041_0020_0040, 7898),
    M!(0x0003_ffdf_7f83_3fc0, 42012),
    M!(0x0000_0088_4045_0020, 57350),
    M!(0x0000_7ffc_8018_0030, 22813),
    M!(0x007f_ffdd_8014_0028, 56693),
    M!(0x0002_0080_200a_0004, 5818),
    M!(0x0000_1010_1010_0020, 7098),
    M!(0x0007_ffdf_c180_5000, 4451),
    M!(0x0003_ffef_e0c0_2200, 4709),
    M!(0x0000_0008_2080_6000, 4794),
    M!(0x0000_0000_0840_3000, 13364),
    M!(0x0000_0001_0020_2000, 4570),
    M!(0x0000_0040_4080_2000, 4282),
    M!(0x0004_0100_4010_0400, 14964),
    M!(0x0000_6020_6018_03f4, 4026),
    M!(0x0003_ffdf_dfc2_8048, 4826),
    M!(0x0000_0008_2082_0020, 7354),
    M!(0x0000_0000_0820_8060, 4848),
    M!(0x0000_0000_0080_8020, 15946),
    M!(0x0000_0000_0100_2020, 14932),
    M!(0x0000_0004_0100_2008, 16588),
    M!(0x0000_0040_4040_4040, 6905),
    M!(0x007f_ff9f_df7f_f813, 16076),
];

const ROOK_INIT: [MagicInit; 64] = [
    M!(0x0028_0077_ffeb_fffe, 26304),
    M!(0x2004_0102_0109_7fff, 35520),
    M!(0x0010_0200_1005_3fff, 38592),
    M!(0x0040_0400_0800_4002, 8026),
    M!(0x7fd0_0441_ffff_d003, 22196),
    M!(0x4020_0088_87df_fffe, 80870),
    M!(0x0040_0088_8847_ffff, 76747),
    M!(0x0068_00fb_ff75_fffd, 30400),
    M!(0x0000_2801_0113_ffff, 11115),
    M!(0x0020_0402_01fc_ffff, 18205),
    M!(0x007f_e800_42ff_ffe8, 53577),
    M!(0x0000_1800_217f_ffe8, 62724),
    M!(0x0000_1800_073f_ffe8, 34282),
    M!(0x0000_1800_e05f_ffe8, 29196),
    M!(0x0000_1800_602f_ffe8, 23806),
    M!(0x0000_3000_2fff_ffa0, 49481),
    M!(0x0030_0018_010b_ffff, 2410),
    M!(0x0003_000c_0085_fffb, 36498),
    M!(0x0004_0008_0201_0008, 24478),
    M!(0x0004_0020_2002_0004, 10074),
    M!(0x0001_0020_0200_2001, 79315),
    M!(0x0001_0010_0080_1040, 51779),
    M!(0x0000_0040_4000_8001, 13586),
    M!(0x0000_0068_00cd_fff4, 19323),
    M!(0x0040_2000_1008_0010, 70612),
    M!(0x0000_0800_1004_0010, 83652),
    M!(0x0004_0100_0802_0008, 63110),
    M!(0x0000_0400_2020_0200, 34496),
    M!(0x0002_0080_1010_0100, 84966),
    M!(0x0000_0080_2001_0020, 54341),
    M!(0x0000_0080_2020_0040, 60421),
    M!(0x0000_8200_2000_4020, 86402),
    M!(0x00ff_fd18_0030_0030, 50245),
    M!(0x007f_ff7f_bfd4_0020, 76622),
    M!(0x003f_ffbd_0018_0018, 84676),
    M!(0x001f_ffde_8018_0018, 78757),
    M!(0x000f_ffe0_bfe8_0018, 37346),
    M!(0x0001_0000_8020_2001, 370),
    M!(0x0003_fffb_ff98_0180, 42182),
    M!(0x0001_fffd_ff90_00e0, 45385),
    M!(0x00ff_fefe_ebff_d800, 61659),
    M!(0x007f_fff7_ffc0_1400, 12790),
    M!(0x003f_ffbf_e4ff_e800, 16762),
    M!(0x001f_fff0_1fc0_3000, 0),
    M!(0x000f_ffe7_f8bf_e800, 38380),
    M!(0x0007_ffdf_df3f_f808, 11098),
    M!(0x0003_fff8_5fff_a804, 21803),
    M!(0x0001_fffd_75ff_a802, 39189),
    M!(0x00ff_ffd7_ffeb_ffd8, 58628),
    M!(0x007f_ff75_ff7f_bfd8, 44116),
    M!(0x003f_ff86_3fbf_7fd8, 78357),
    M!(0x001f_ffbf_dfd7_ffd8, 44481),
    M!(0x000f_fff8_1028_0028, 64134),
    M!(0x0007_ffd7_f7fe_ffd8, 41759),
    M!(0x0003_fffc_0c48_0048, 1394),
    M!(0x0001_ffff_afd7_ffd8, 40910),
    M!(0x00ff_ffe4_ffdf_a3ba, 66516),
    M!(0x007f_ffef_7ff3_d3da, 3897),
    M!(0x003f_ffbf_dfef_f7fa, 3930),
    M!(0x001f_ffef_f7fb_fc22, 72934),
    M!(0x0000_0204_0800_1001, 72662),
    M!(0x0007_fffe_ffff_77fd, 56325),
    M!(0x0003_ffff_bf7d_feec, 66501),
    M!(0x0001_ffff_9dff_a333, 14826),
];

// Compute the attack's index using the 'magic bitboards' approach
fn index_bishop(s: Square, occupied: Bitboard) -> usize {
    unsafe {
        (u64::wrapping_mul(
            (occupied & BISHOP_MAGICS.masks[s.0 as usize]).0,
            BISHOP_MAGICS.magics[s.0 as usize],
        ) >> (64 - 9)) as usize
    }
}

fn index_rook(s: Square, occupied: Bitboard) -> usize {
    unsafe {
        (u64::wrapping_mul(
            (occupied & ROOK_MAGICS.masks[s.0 as usize]).0,
            ROOK_MAGICS.magics[s.0 as usize],
        ) >> (64 - 12)) as usize
    }
}

fn attacks_bb_bishop(s: Square, occupied: Bitboard) -> Bitboard {
    unsafe { BISHOP_MAGICS.attacks[s.0 as usize][index_bishop(s, occupied)] }
}

fn attacks_bb_rook(s: Square, occupied: Bitboard) -> Bitboard {
    unsafe { ROOK_MAGICS.attacks[s.0 as usize][index_rook(s, occupied)] }
}

impl std::convert::From<Square> for Bitboard {
    fn from(s: Square) -> Self {
        unsafe { SQUARE_BB[s.0 as usize] }
    }
}

impl Square {
    pub fn bb(self) -> Bitboard {
        Bitboard::from(self)
    }

    pub fn file_bb(self) -> Bitboard {
        file_bb(self.file())
    }

    pub fn rank_bb(self) -> Bitboard {
        unsafe { RANK_BB[self.rank() as usize] }
    }
}

impl std::ops::BitOr<Bitboard> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Bitboard(self.0 | rhs.0)
    }
}

impl std::ops::BitOr<Square> for Bitboard {
    type Output = Bitboard;
    fn bitor(self, rhs: Square) -> Self {
        self | Bitboard::from(rhs)
    }
}

impl std::ops::BitAnd<Bitboard> for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Bitboard(self.0 & rhs.0)
    }
}

impl std::ops::BitAnd<Square> for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: Square) -> Self {
        self & Bitboard::from(rhs)
    }
}

impl std::ops::BitXor<Bitboard> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXor<Square> for Bitboard {
    type Output = Bitboard;
    fn bitxor(self, rhs: Square) -> Self {
        self ^ Bitboard::from(rhs)
    }
}

impl std::ops::Not for Bitboard {
    type Output = Bitboard;
    fn not(self) -> Self {
        Bitboard(!self.0)
    }
}

impl std::ops::Neg for Bitboard {
    type Output = Bitboard;
    fn neg(self) -> Self {
        Bitboard(self.0.wrapping_neg())
    }
}

impl std::ops::Shl<i32> for Bitboard {
    type Output = Bitboard;
    fn shl(self, rhs: i32) -> Self {
        Bitboard(self.0 << rhs)
    }
}

impl std::ops::Shr<i32> for Bitboard {
    type Output = Bitboard;
    fn shr(self, rhs: i32) -> Self {
        Bitboard(self.0 >> rhs)
    }
}

impl<RHS> std::ops::BitOrAssign<RHS> for Bitboard
where
    Bitboard: std::ops::BitOr<RHS, Output = Bitboard>,
{
    fn bitor_assign(&mut self, rhs: RHS) {
        *self = *self | rhs;
    }
}

impl<RHS> std::ops::BitAndAssign<RHS> for Bitboard
where
    Bitboard: std::ops::BitAnd<RHS, Output = Bitboard>,
{
    fn bitand_assign(&mut self, rhs: RHS) {
        *self = *self & rhs;
    }
}

impl<RHS> std::ops::BitXorAssign<RHS> for Bitboard
where
    Bitboard: std::ops::BitXor<RHS, Output = Bitboard>,
{
    fn bitxor_assign(&mut self, rhs: RHS) {
        *self = *self ^ rhs;
    }
}

impl std::cmp::PartialEq<u64> for Bitboard {
    fn eq(&self, rhs: &u64) -> bool {
        debug_assert!(*rhs == 0);
        self.0 == *rhs
    }
}

impl std::fmt::Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for s in *self {
            write!(f, "{} ", uci::square(s)).unwrap();
        }
        write!(f, "")
    }
}

pub fn more_than_one(b: Bitboard) -> bool {
    (b.0 & u64::wrapping_sub(b.0, 1)) != 0
}

pub fn lsb(b: Bitboard) -> Square {
    debug_assert!(b != 0);
    Square(u64::trailing_zeros(b.0))
}

pub fn msb(b: Bitboard) -> Square {
    debug_assert!(b != 0);
    Square(63 ^ u64::leading_zeros(b.0))
}

pub fn pop_lsb(b: &mut Bitboard) -> Square {
    let s = lsb(*b);
    b.0 &= u64::wrapping_sub(b.0, 1);
    s
}

pub fn frontmost_sq(c: Color, b: Bitboard) -> Square {
    if c == WHITE {
        msb(b)
    } else {
        lsb(b)
    }
}

pub fn backmost_sq(c: Color, b: Bitboard) -> Square {
    if c == WHITE {
        lsb(b)
    } else {
        msb(b)
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = BitboardIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        BitboardIntoIter(self)
    }
}

impl<'a> IntoIterator for &'a Bitboard {
    type Item = Square;
    type IntoIter = BitboardIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BitboardIter {
            bitboard: self,
            remaining: *self,
        }
    }
}

pub struct BitboardIntoIter(Bitboard);

impl Iterator for BitboardIntoIter {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(pop_lsb(&mut self.0))
        } else {
            None
        }
    }
}

pub struct BitboardIter<'a> {
    bitboard: &'a Bitboard,
    remaining: Bitboard,
}

impl<'a> Iterator for BitboardIter<'a> {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
        if self.remaining.0 != 0 {
            Some(pop_lsb(&mut self.remaining))
        } else {
            None
        }
    }
}

// file_bb() return a bitboard representing all the squares on the given file.

pub fn file_bb(f: File) -> Bitboard {
    unsafe { FILE_BB[f as usize] }
}

// bitboard!(A1, A2, ...) creates a bitboard with squares A1, A2, ...

macro_rules! bitboard {
    () => { Bitboard(0) };
    ($sq:ident) => { bitboard!() | Square::$sq };
    ($sq:ident, $($sqs:ident),+) => { bitboard!($($sqs),*) | Square::$sq };
}

// shift() moves a bitboard one step along direction D. Mainly for pawns.

impl Bitboard {
    pub fn shift(self, d: Direction) -> Bitboard {
        match d {
            NORTH => self << 8,
            SOUTH => self >> 8,
            NORTH_EAST => (self & !FILEH_BB) << 9,
            SOUTH_EAST => (self & !FILEH_BB) >> 7,
            NORTH_WEST => (self & !FILEA_BB) << 7,
            SOUTH_WEST => (self & !FILEA_BB) >> 9,
            _ => Bitboard(0),
        }
    }
}

// adjacent_files_bb() returns a bitboard representing all the squares on
// the adjacent files of the given one.

pub fn adjacent_files_bb(f: File) -> Bitboard {
    unsafe { ADJACENT_FILES_BB[f as usize] }
}

// between_bb() returns a bitboard representing all the squares between the
// two given ones. For instance, between_bb(Square::C4, Square::F7) returns
// a bitboard with the bits for squares d5 and e6 set. If s1 and s2 are not
// on the same rank, file or diagonal, an empty bitboard is returned.

pub fn between_bb(s1: Square, s2: Square) -> Bitboard {
    unsafe { BETWEEN_BB[s1.0 as usize][s2.0 as usize] }
}

// forward_ranks_bb() returns a bitboard representing all the squares on all
// the ranks in front of the given one, from the point of view of the given
// color. For instance, forward_ranks_bb(BLACK, Square::D3) returns the 16
// squares on ranks 1 and 2.

pub fn forward_ranks_bb(c: Color, s: Square) -> Bitboard {
    unsafe { FORWARD_RANKS_BB[c.0 as usize][s.rank() as usize] }
}

// forward_file_bb() returns a bitboard representing all the squares along
// the line in front of the given one, from the point of view of the given
// color.

pub fn forward_file_bb(c: Color, s: Square) -> Bitboard {
    unsafe { FORWARD_FILE_BB[c.0 as usize][s.0 as usize] }
}

// pawn_attack_span() returns a bitboard representing all the squares that
// can be attacked by a pawn of the given color when it moves along its file,
// starting from the given square.

pub fn pawn_attack_span(c: Color, s: Square) -> Bitboard {
    unsafe { PAWN_ATTACK_SPAN[c.0 as usize][s.0 as usize] }
}

// passed_pawn_mask() returns a bitboard mask which can be used to test if a
// pawn of the given color and on the given square is a passed pawn.

pub fn passed_pawn_mask(c: Color, s: Square) -> Bitboard {
    unsafe { PASSED_PAWN_MASK[c.0 as usize][s.0 as usize] }
}

pub fn line_bb(s1: Square, s2: Square) -> Bitboard {
    unsafe { LINE_BB[s1.0 as usize][s2.0 as usize] }
}

// aligned() returns true if the squares s1, s2 and s3 are aligned either on
// a straight or on a diagonal line.

pub fn aligned(s1: Square, s2: Square, s3: Square) -> bool {
    line_bb(s1, s2) & s3 != 0
}

pub fn pseudo_attacks(pt: PieceType, s: Square) -> Bitboard {
    unsafe { PSEUDO_ATTACKS[pt.0 as usize][s.0 as usize] }
}

pub fn pawn_attacks(c: Color, s: Square) -> Bitboard {
    unsafe { PAWN_ATTACKS[c.0 as usize][s.0 as usize] }
}

pub fn distance_ring_bb(s: Square, d: i32) -> Bitboard {
    unsafe { DISTANCE_RING_BB[s.0 as usize][d as usize] }
}

pub trait Distance {
    fn distance(x: Self, y: Self) -> u32;
}

impl Distance for u32 {
    fn distance(x: Self, y: Self) -> u32 {
        if x > y {
            x - y
        } else {
            y - x
        }
    }
}

fn get_index(x: usize, y: usize) -> usize {
    x * 64 + y
}

impl Distance for Square {
    fn distance(x: Self, y: Self) -> u32 {
        unsafe { SQUARE_DISTANCE[get_index(x.0 as usize, y.0 as usize)] }
    }
}

// init() initializes various bitboard tables. It is called at startup.
#[allow(clippy::too_many_lines)]
pub fn init() {
    for s in ALL_SQUARES {
        unsafe {
            SQUARE_BB[s.0 as usize] = Bitboard(1u64) << (s.0 as i32);
        }
    }

    for f in 0..8 {
        unsafe {
            FILE_BB[f as usize] = FILEA_BB << f;
        }
    }

    for r in 0..8 {
        unsafe {
            RANK_BB[r as usize] = RANK1_BB << (8 * r);
        }
    }

    for f in 0..8 {
        let left = if f > FILE_A {
            file_bb(f - 1)
        } else {
            Bitboard(0)
        };
        let right = if f < FILE_H {
            file_bb(f + 1)
        } else {
            Bitboard(0)
        };

        unsafe {
            ADJACENT_FILES_BB[f as usize] = left | right;
        }
    }

    for r in 0..7 {
        unsafe {
            let black_forward_rank =
                FORWARD_RANKS_BB[BLACK.0 as usize][r as usize] | RANK_BB[r as usize];
            FORWARD_RANKS_BB[BLACK.0 as usize][(r + 1) as usize] = black_forward_rank;

            let white_forward_rank = !black_forward_rank;
            FORWARD_RANKS_BB[WHITE.0 as usize][r as usize] = white_forward_rank;
        }
    }

    for &color in &[WHITE, BLACK] {
        for square in &ALL_SQUARES {
            unsafe {
                let forward_rank = FORWARD_RANKS_BB[color.0 as usize][square.rank() as usize];
                let file_bb = FILE_BB[square.file() as usize];

                let forward_file = forward_rank & file_bb;
                FORWARD_FILE_BB[color.0 as usize][square.0 as usize] = forward_file;

                let adjacent_files = ADJACENT_FILES_BB[square.file() as usize];
                let pawn_attack_span = forward_rank & adjacent_files;
                PAWN_ATTACK_SPAN[color.0 as usize][square.0 as usize] = pawn_attack_span;

                let passed_pawn_mask = forward_file | pawn_attack_span;
                PASSED_PAWN_MASK[color.0 as usize][square.0 as usize] = passed_pawn_mask;
            }
        }
    }

    // set square distance
    for s1 in &ALL_SQUARES {
        for s2 in &ALL_SQUARES {
            if s1 != s2 {
                let dist = std::cmp::max(
                    File::distance(s1.file(), s2.file()),
                    Rank::distance(s1.rank(), s2.rank()),
                );

                unsafe {
                    let index = get_index(s1.0 as usize, s2.0 as usize);
                    SQUARE_DISTANCE[index] = dist;
                    DISTANCE_RING_BB[s1.0 as usize][dist as usize - 1] |= s2;
                }
            }
        }
    }

    for &color in &[WHITE, BLACK] {
        for &piece_type in &[PAWN, KNIGHT, KING] {
            for square in &ALL_SQUARES {
                let steps: &[i32] = match piece_type {
                    PAWN => &[7, 9],
                    KNIGHT => &[6, 10, 15, 17],
                    _ => &[1, 7, 8, 9],
                };

                for &step in steps {
                    let direction = if color == WHITE {
                        Direction(step)
                    } else {
                        Direction(-step)
                    };

                    let to_square = square + direction;

                    if to_square.is_ok() && Square::distance(square, to_square) < 3 {
                        unsafe {
                            if piece_type == PAWN {
                                PAWN_ATTACKS[color.0 as usize][square.0 as usize] |= to_square;
                            } else {
                                PSEUDO_ATTACKS[piece_type.0 as usize][square.0 as usize] |=
                                    to_square;
                            }
                        }
                    }
                }
            }
        }
    }

    let rook_dirs = [NORTH, EAST, SOUTH, WEST];
    let bishop_dirs = [NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST];

    unsafe {
        init_magics(&mut ROOK_MAGICS, &ROOK_INIT, rook_dirs, index_rook);
        init_magics(&mut BISHOP_MAGICS, &BISHOP_INIT, bishop_dirs, index_bishop);
    }

    for s1 in &ALL_SQUARES {
        let bishop_attacks = attacks_bb(BISHOP, s1, Bitboard(0));
        let rook_attacks = attacks_bb(ROOK, s1, Bitboard(0));

        unsafe {
            PSEUDO_ATTACKS[BISHOP.0 as usize][s1.0 as usize] = bishop_attacks;
            PSEUDO_ATTACKS[ROOK.0 as usize][s1.0 as usize] = rook_attacks;
            PSEUDO_ATTACKS[QUEEN.0 as usize][s1.0 as usize] = bishop_attacks | rook_attacks;
        }

        for &piece_type in &[BISHOP, ROOK] {
            let s1_attacks = attacks_bb(piece_type, s1, Bitboard(0));

            for s2 in &ALL_SQUARES {
                unsafe {
                    if s1_attacks & s2 == 0 {
                        continue;
                    }

                    let s2_attacks = attacks_bb(piece_type, s2, Bitboard(0));

                    LINE_BB[s1.0 as usize][s2.0 as usize] = (s1_attacks & s2_attacks) | s1 | s2;
                    BETWEEN_BB[s1.0 as usize][s2.0 as usize] =
                        attacks_bb(piece_type, s1, s2.bb()) & attacks_bb(piece_type, s2, s1.bb());
                }
            }
        }
    }
}

fn sliding_attack(directions: [Direction; 4], sq: Square, occupied: Bitboard) -> Bitboard {
    let mut attack = Bitboard(0);
    for d in &directions {
        let mut s = sq + *d;
        while s.is_ok() && Square::distance(s, s - *d) == 1 {
            attack |= s;
            if occupied & s != 0 {
                break;
            }
            s += *d;
        }
    }
    attack
}

fn init_magics(
    m: &mut Magics,
    magic_init: &[MagicInit; 64],
    dirs: [Direction; 4],
    index: fn(Square, Bitboard) -> usize,
) {
    for s in ALL_SQUARES {
        // Board edges are not considered in the relevant occupancies
        let edges = ((RANK1_BB | RANK8_BB) & !s.rank_bb()) | ((FILEA_BB | FILEH_BB) & !s.file_bb());

        let mask = sliding_attack(dirs, s, Bitboard(0)) & !edges;

        m.masks[s.0 as usize] = mask;
        m.magics[s.0 as usize] = magic_init[s.0 as usize].magic;

        let base = magic_init[s.0 as usize].index as usize;
        let mut size = 0;

        // Use Carry-Ripler trick to enumerate all subsets of masks[s] and
        // fill the attacks table.
        let mut b = Bitboard(0);
        loop {
            let idx = index(s, b);
            size = std::cmp::max(size, idx + 1);
            unsafe {
                ATTACKS_TABLE[base + idx] = sliding_attack(dirs, s, b);
            }
            b = Bitboard(u64::wrapping_sub(b.0, mask.0)) & mask;
            if b == 0 {
                break;
            }
        }

        m.attacks[s.0 as usize] = unsafe { &ATTACKS_TABLE[base..base + size] };
    }
}

// attacks_bb() returns a bitboard representing all the squares attacked by
// a piece of type Pt (bishop or rook) placed on 's'.

pub fn attacks_bb(pt: PieceType, s: Square, occupied: Bitboard) -> Bitboard {
    match pt {
        BISHOP => attacks_bb_bishop(s, occupied),
        ROOK => attacks_bb_rook(s, occupied),
        QUEEN => attacks_bb_bishop(s, occupied) | attacks_bb_rook(s, occupied),
        _ => pseudo_attacks(pt, s),
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_example() {
        assert_eq!(2 + 2, 4);
    }
}

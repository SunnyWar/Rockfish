// SPDX-License-Identifier: GPL-3.0-or-later

use crate::bitboard::{pawn_attacks, pseudo_attacks, Distance};
use crate::types::{direction::Direction, Color, Square, BLACK, FILE_D, KING, RANK_2, RANK_7, WHITE};

// There are 24 possible pawn squares: the first 4 files and ranks from 2 to 7
const MAX_INDEX: usize = 2 * 24 * 64 * 64;

// Each u32 stores results of 32 positions, one per bit
use std::sync::Once;

static mut KPK_BITBASE: Option<&[u32; MAX_INDEX / 32]> = None;
static KPK_BITBASE_INIT: Once = Once::new();

// A KPK bitbase index is an integer in [0, IndexMax] range
//
// Information is mapped in a way that minimizes the number of iterations:
//
// bit  0- 5: white king square (from A1 to H8)
// bit  6-11: black king square (from A1 to H8)
// bit    12: side to move (WHITE or BLACK)
// bit 13-14: white pawn file (from FILE_A to FILE_D)
// bit 15-17: white pawn RANK_7 - rank
//            (from RANK_7 - RANK_7 to RANK_7 - RANK_2)
fn index(us: Color, bksq: Square, wksq: Square, psq: Square) -> usize {
    (wksq.0 | (bksq.0 << 6) | (us.0 << 12) | (psq.file() << 13) | ((RANK_7 - psq.rank()) << 15))
        as usize
}

const INVALID: u8 = 0;
const UNKNOWN: u8 = 1;
const DRAW: u8 = 2;
const WIN: u8 = 4;

struct KPKPosition {
    us: Color,
    ksq: [Square; 2],
    psq: Square,
    result: u8,
}

impl KPKPosition {
    fn new(idx: u32) -> KPKPosition {
        let ksq = [Square(idx & 0x3f), Square((idx >> 6) & 0x3f)];
        let us = Color((idx >> 12) & 0x01);
        let psq = Square::make((idx >> 13) & 0x03, RANK_7 - ((idx >> 15) & 0x07));

        let white_king = ksq[WHITE.0 as usize];
        let black_king = ksq[BLACK.0 as usize];
        let white_pawn_attacks = pawn_attacks(WHITE, psq);
        let black_pawn_attacks = pawn_attacks(BLACK, psq);

        let result = match (
            Square::distance(white_king, black_king) <= 1, // Check if the kings are adjacent
            white_king == psq, // Check if the white king is on the pawn's square
            black_king == psq, // Check if the black king is on the pawn's square
            us,
            white_pawn_attacks & black_king != 0, // Check if the white pawn is attacking the black king
            psq.rank() == RANK_7,                 // Check if the pawn can be promoted
            white_king != psq + Direction::NORTH, // Ensure the white king is not blocking the pawn's promotion
            Square::distance(black_king, psq + Direction::NORTH) > 1, // Check if the black king is at least 2 squares away from the pawn's promotion square
            pseudo_attacks(KING, white_king) & (psq + Direction::NORTH) != 0, // Check if the white king can defend the promotion square
            pseudo_attacks(KING, black_king)
                & !(pseudo_attacks(KING, white_king) | black_pawn_attacks)
                == 0, // Check if the black king is stalemated or if white is attacking
            pseudo_attacks(KING, black_king) & psq & !pseudo_attacks(KING, white_king) != 0, // Check if the black king can capture the pawn
        ) {
            // If the kings are adjacent, or a king is on the pawn's square, or the white pawn is attacking the black king
            (true, _, _, _, _, _, _, _, _, _, _)
            | (_, true, _, _, _, _, _, _, _, _, _)
            | (_, _, true, _, _, _, _, _, _, _, _)
            | (_, _, _, WHITE, true, _, _, _, _, _, _) => INVALID, // Result is invalid

            // Immediate win if a pawn can be promoted without getting captured
            (_, _, _, WHITE, _, true, true, true, true, _, _) => WIN, // Result is win

            // Immediate draw if it is a stalemate or a king captures undefended pawn
            (_, _, _, BLACK, _, _, _, _, _, true, _) | (_, _, _, BLACK, _, _, _, _, _, _, true) => {
                DRAW
            } // Result is draw

            // Position will be classified later
            _ => UNKNOWN, // Result is unknown
        };

        KPKPosition {
            us,
            ksq,
            psq,
            result,
        }
    }

    fn classify(&self, db: &[KPKPosition]) -> u8 {
        // White to move: if one move leads to a position classified as WIN,
        // the result of the current position is WIN; if all moves lead to
        // positions classified as DRAW, the current position is classified
        // as DRAW; otherwise, the current position is classified as UNKNOWN.
        //
        // Black to move: if one move leads to a position classified as DRAW,
        // the result of the current position is DRAW; if all moves lead to
        // positions classified as WIN, the position is classified as WIN;
        // otherwise, the current current position is classified as UNKNOWN.

        let us = self.us;
        let psq = self.psq;

        let them = if us == WHITE { BLACK } else { WHITE };
        let good = if us == WHITE { WIN } else { DRAW };
        let bad = if us == WHITE { DRAW } else { WIN };

        let mut r = INVALID;

        for s in pseudo_attacks(KING, self.ksq[us.0 as usize]) {
            r |= match us {
                WHITE => db[index(them, self.ksq[them.0 as usize], s, psq)].result,
                _ => db[index(them, s, self.ksq[them.0 as usize], psq)].result,
            };
        }

        if us == WHITE {
            match psq.rank() {
                rank if rank < RANK_7 => {
                    r |= db[index(
                        them,
                        self.ksq[them.0 as usize],
                        self.ksq[us.0 as usize],
                        psq + Direction::NORTH,
                    )]
                    .result;
                }
                RANK_2 => {
                    if psq + Direction::NORTH != self.ksq[us.0 as usize]
                        && psq + Direction::NORTH != self.ksq[them.0 as usize]
                    {
                        r |= db[index(
                            them,
                            self.ksq[them.0 as usize],
                            self.ksq[us.0 as usize],
                            psq + 2 * Direction::NORTH,
                        )]
                        .result;
                    }
                }
                _ => {}
            }
        }

        match (r & good != 0, r & UNKNOWN != 0) {
            (true, _) => good,
            (false, true) => UNKNOWN,
            _ => bad,
        }
    }
}

pub fn init() {
    let mut bitbase_storage: Box<[u32; MAX_INDEX / 32]> = Box::new([0; MAX_INDEX / 32]);
    let mut db: Vec<KPKPosition> = Vec::with_capacity(MAX_INDEX);

    // Initialize db with known win/draw positions
    for idx in 0..MAX_INDEX {
        db.push(KPKPosition::new(idx as u32));
    }

    let mut repeat = true;

    // Iterate through the positions until none of the unknown positions can
    // be changed to either wins or draws (15 cycles needed).
    while repeat {
        repeat = false;
        for idx in 0..MAX_INDEX {
            if db[idx].result == UNKNOWN {
                let result = db[idx].classify(&db);
                if result != UNKNOWN {
                    db[idx].result = result;
                    repeat = true;
                }
            }
        }
    }

    // Map 32 results into one KPK_BITBASE[] entry
    for idx in 0..MAX_INDEX {
        if db[idx].result == WIN {
            bitbase_storage[idx / 32] |= 1u32 << (idx & 0x1f);
        }
    }

    KPK_BITBASE_INIT.call_once(|| unsafe {
        KPK_BITBASE = Some(Box::leak(bitbase_storage));
    });
}

pub fn probe(wksq: Square, wpsq: Square, bksq: Square, us: Color) -> bool {
    debug_assert!(wpsq.file() <= FILE_D);
    let idx = index(us, bksq, wksq, wpsq);
    let byte = idx / 32;
    let bit = idx % 32;
    unsafe {
        let bitbase = KPK_BITBASE.unwrap();
        bitbase[byte] & (1 << bit) != 0
    }
}

// SPDX-License-Identifier: GPL-3.0-or-later

use crate::bitboard::more_than_one;
use crate::endgame::{
    evaluate_kxk, scale_kbpsk, scale_kpkp, scale_kpsk, scale_kqkrps, EvalFn, ScaleFn, EVAL_FNS,
    SCALE_FNS,
};
use crate::position::Position;
use crate::types::{
    key::Key, scale_factor::ScaleFactor, Color, Phase, PieceType, Score, Value, ENDGAME_LIMIT,
    MIDGAME_LIMIT, PHASE_MIDGAME,
};

pub struct Entry {
    key: Key,
    scaling_function: [Option<ScaleFn>; 2],
    evaluation_function: Option<EvalFn>,
    eval_side: Color,
    value: i16,
    factor: [u8; 2],
    game_phase: Phase,
}

impl Entry {
    pub fn new() -> Entry {
        Entry {
            key: Key(0),
            scaling_function: [None; 2],
            evaluation_function: None,
            eval_side: Color::WHITE,
            value: 0,
            factor: [0; 2],
            game_phase: 0,
        }
    }

    pub fn imbalance(&self) -> Score {
        Score::make(i32::from(self.value), i32::from(self.value))
    }

    pub fn game_phase(&self) -> Phase {
        self.game_phase
    }

    pub fn specialized_eval_exists(&self) -> bool {
        self.evaluation_function.is_some()
    }

    pub fn evaluate(&self, pos: &Position) -> Value {
        self.evaluation_function.unwrap()(pos, self.eval_side)
    }

    pub fn scale_factor(&self, pos: &Position, c: Color) -> ScaleFactor {
        let sf = match self.scaling_function[c.0 as usize] {
            Some(f) => f(pos, c),
            None => ScaleFactor::NONE,
        };
        if sf != ScaleFactor::NONE {
            sf
        } else {
            ScaleFactor(i32::from(self.factor[c.0 as usize]))
        }
    }
}

// Polynomial material imbalance parameters

const QUADRATIC_OURS: [[i32; 8]; 6] = [
    //             OUR PIECES
    // pair pawn knight bishop rook queen
    [1667, 0, 0, 0, 0, 0, 0, 0],           // Bishop pair
    [40, 0, 0, 0, 0, 0, 0, 0],             // Pawn
    [32, 255, -3, 0, 0, 0, 0, 0],          // Knight     OUR PIECES
    [0, 104, 4, 0, 0, 0, 0, 0],            // Bishop
    [-26, -2, 47, 105, -149, 0, 0, 0],     // Rook
    [-189, 24, 117, 133, -134, -10, 0, 0], // Queen
];

const QUADRATIC_THEIRS: [[i32; 8]; 6] = [
    //           THEIR PIECES
    // pair pawn knight bishop rook queen
    [0, 0, 0, 0, 0, 0, 0, 0],          // Bishop pair
    [36, 0, 0, 0, 0, 0, 0, 0],         // Pawn
    [9, 63, 0, 0, 0, 0, 0, 0],         // Knight    THEIR PIECES
    [59, 65, 42, 0, 0, 0, 0, 0],       // Bishop
    [46, 39, 24, -24, 0, 0, 0, 0],     // Rook
    [97, 100, -42, 137, 268, 0, 0, 0], // Queen
];

// Helper used to detect a given material distribution
fn is_kxk(pos: &Position, us: Color) -> bool {
    !more_than_one(pos.pieces_c(!us)) && pos.non_pawn_material_c(us) >= Value::RookValueMg
}

fn is_kbpsks(pos: &Position, us: Color) -> bool {
    pos.non_pawn_material_c(us) == Value::BishopValueMg
        && pos.count(us, PieceType::BISHOP) == 1
        && pos.count(us, PieceType::PAWN) >= 1
}

fn is_kqkrps(pos: &Position, us: Color) -> bool {
    pos.count(us, PieceType::PAWN) == 0
        && pos.non_pawn_material_c(us) == Value::QueenValueMg
        && pos.count(us, PieceType::QUEEN) == 1
        && pos.count(!us, PieceType::ROOK) == 1
        && pos.count(!us, PieceType::PAWN) >= 1
}

// imbalance() calculates the imbalance by comparing the piece count of
// each piece type for both colors.
fn imbalance(pc: &[[i32; 6]; 2], us: Color) -> i32 {
    let them = match us {
        Color::WHITE => Color::BLACK,
        _ => Color::WHITE,
    };

    let mut bonus = 0;

    // Second-degree polynomial material imbalance, by Tord Romstad
    for pt1 in 0..6 {
        if pc[us.0 as usize][pt1] == 0 {
            continue;
        }

        let mut v = 0;

        for pt2 in 0..=pt1 {
            v += QUADRATIC_OURS[pt1][pt2] * pc[us.0 as usize][pt2]
                + QUADRATIC_THEIRS[pt1][pt2] * pc[them.0 as usize][pt2];
        }

        bonus += pc[us.0 as usize][pt1] * v;
    }

    bonus
}

// probe() looks up the current position's material configuration in the
// material hash table. It returns a pointer to the Entry if the position
// is found. Otherwise a new Entry is computed and stored there, so we
// don't have to recompute all when the same material configuration occurs
// again.
#[allow(clippy::too_many_lines)]
pub fn probe(pos: &Position) -> &'static mut Entry {
    let key = pos.material_key();
    let e = pos.material_table[(key.0 & 8191) as usize].get();
    let e: &'static mut Entry = unsafe { &mut *e };

    if e.key == key {
        return e;
    }

    e.key = key;
    e.evaluation_function = None;
    e.scaling_function = [None; 2];
    e.factor[Color::WHITE.0 as usize] = ScaleFactor::NORMAL.0 as u8;
    e.factor[Color::BLACK.0 as usize] = ScaleFactor::NORMAL.0 as u8;
    e.value = 0;

    // Map total non-pawn material into [PHASE_ENDGAME, PHASE_MIDGAME]
    let npm_w = pos.non_pawn_material_c(Color::WHITE);
    let npm_b = pos.non_pawn_material_c(Color::BLACK);
    let npm = std::cmp::max(ENDGAME_LIMIT, std::cmp::min(npm_w + npm_b, MIDGAME_LIMIT));
    e.game_phase =
        (((npm - ENDGAME_LIMIT) * PHASE_MIDGAME) / (MIDGAME_LIMIT - ENDGAME_LIMIT)) as i32;

    // Let's look if we have a specialized evaluation function for this
    // particular material configuration. First we look for a fixed
    // configuartion one, then for a generic one.
    for entry in unsafe { EVAL_FNS.iter() } {
        for c in 0..2 {
            if entry.key[c] == key {
                e.evaluation_function = Some(entry.func);
                e.eval_side = Color(c as u32);
                return e;
            }
        }
    }

    for &c in &[Color::WHITE, Color::BLACK] {
        if is_kxk(pos, c) {
            e.evaluation_function = Some(evaluate_kxk);
            e.eval_side = c;
            return e;
        }
    }

    // OK, we didn't find any special evaluation function for the current
    // material configuration. Is there a suitable specialized scaling
    // function?
    for entry in unsafe { SCALE_FNS.iter() } {
        for c in 0..2 {
            if entry.key[c] == key {
                e.scaling_function[c] = Some(entry.func);
                return e;
            }
        }
    }

    // We didn't find any specialized scaling function, so fall back on
    // generic ones that refer to more than one material distributiion.
    // Note that in this case we don't return after setting the function.
    for &c in &[Color::WHITE, Color::BLACK] {
        e.scaling_function[c.0 as usize] = match (is_kbpsks(pos, c), is_kqkrps(pos, c)) {
            (true, _) => Some(scale_kbpsk),
            (_, true) => Some(scale_kqkrps),
            _ => None,
        };
    }

    if npm_w + npm_b == Value::ZERO && pos.pieces_p(PieceType::PAWN) != 0 {
        match (
            pos.count(Color::WHITE, PieceType::PAWN),
            pos.count(Color::BLACK, PieceType::PAWN),
        ) {
            (white, 0) if white >= 2 => {
                debug_assert!(white >= 2);
                e.scaling_function[Color::WHITE.0 as usize] = Some(scale_kpsk);
            }
            (0, black) if black >= 2 => {
                debug_assert!(black >= 2);
                e.scaling_function[Color::BLACK.0 as usize] = Some(scale_kpsk);
            }
            (1, 1) => {
                e.scaling_function[Color::WHITE.0 as usize] = Some(scale_kpkp);
                e.scaling_function[Color::BLACK.0 as usize] = Some(scale_kpkp);
            }
            _ => {}
        }
    }

    // Zero or just one pawn makes it difficult to win, even with a small
    // material advantage. This catches some trivial draws like KK, KBK
    // and KNK and gives a drawish scale factor for cases such as KRKBP
    // and KmmKm (except for KBBKN).
    if pos.count(Color::WHITE, PieceType::PAWN) == 0 && npm_w - npm_b <= Value::BishopValueMg {
        e.factor[Color::WHITE.0 as usize] = match npm_w {
            x if x < Value::RookValueMg => ScaleFactor::DRAW.0 as u8,
            _ if npm_b <= Value::BishopValueMg => 4,
            _ => 14,
        };
    }

    if pos.count(Color::BLACK, PieceType::PAWN) == 0 && npm_b - npm_w <= Value::BishopValueMg {
        e.factor[Color::BLACK.0 as usize] = match npm_b {
            x if x < Value::RookValueMg => ScaleFactor::DRAW.0 as u8,
            _ if npm_w <= Value::BishopValueMg => 4,
            _ => 14,
        };
    }

    for &(color, npm_diff, scale_factor) in &[
        (Color::WHITE, npm_w - npm_b, Color::WHITE.0),
        (Color::BLACK, npm_b - npm_w, Color::BLACK.0),
    ] {
        if pos.count(color, PieceType::PAWN) == 1 && npm_diff <= Value::BishopValueMg {
            e.factor[scale_factor as usize] = ScaleFactor::ONEPAWN.0 as u8;
        }
    }

    // Evaluate the material imbalance. We use PIECE_TYPE_NONE as a place
    // holder for the bishop pair "extended piece", which allows us to be
    // more flexible in defining bishop pair bonuses.
    let pc = [
        [
            i32::from(pos.count(Color::WHITE, PieceType::BISHOP) > 1),
            pos.count(Color::WHITE, PieceType::PAWN),
            pos.count(Color::WHITE, PieceType::KNIGHT),
            pos.count(Color::WHITE, PieceType::BISHOP),
            pos.count(Color::WHITE, PieceType::ROOK),
            pos.count(Color::WHITE, PieceType::QUEEN),
        ],
        [
            i32::from(pos.count(Color::BLACK, PieceType::BISHOP) > 1),
            pos.count(Color::BLACK, PieceType::PAWN),
            pos.count(Color::BLACK, PieceType::KNIGHT),
            pos.count(Color::BLACK, PieceType::BISHOP),
            pos.count(Color::BLACK, PieceType::ROOK),
            pos.count(Color::BLACK, PieceType::QUEEN),
        ],
    ];

    e.value = ((imbalance(&pc, Color::WHITE) - imbalance(&pc, Color::BLACK)) / 16) as i16;

    e
}

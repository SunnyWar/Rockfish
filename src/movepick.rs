// SPDX-License-Identifier: GPL-3.0-or-later

use crate::movegen::{generate, Captures, Evasions, ExtMove, QuietChecks, Quiets};
use crate::position::Position;
use crate::search;
use crate::types::{
    depth::Depth, piece_value, Color, Move, Piece, PieceType, Square, Value, MAX_MOVES, MG,
};

use std::cell::Cell;

pub struct ButterflyHistory {
    v: [[Cell<i16>; 4096]; 2],
}

impl ButterflyHistory {
    pub fn get(&self, c: Color, m: Move) -> i32 {
        i32::from(self.v[c.0 as usize][m.from_to() as usize].get())
    }

    pub fn update(&self, c: Color, m: Move, bonus: i32) {
        let entry = &self.v[c.0 as usize][m.from_to() as usize];
        let mut val = entry.get();
        val += (bonus * 32 - i32::from(val) * bonus.abs() / 324) as i16;
        entry.set(val);
    }
}

pub struct PieceToHistory {
    v: [[Cell<i16>; 64]; 16],
}

impl PieceToHistory {
    pub fn get(&self, pc: Piece, s: Square) -> i32 {
        i32::from(self.v[pc.0 as usize][s.0 as usize].get())
    }

    pub fn update(&self, pc: Piece, s: Square, bonus: i32) {
        let entry = &self.v[pc.0 as usize][s.0 as usize];
        let mut val = entry.get();
        val += (bonus * 32 - i32::from(val) * bonus.abs() / 936) as i16;
        entry.set(val);
    }
}

pub struct CapturePieceToHistory {
    v: [[[Cell<i16>; 8]; 64]; 16],
}

impl CapturePieceToHistory {
    pub fn get(&self, pc: Piece, to: Square, cap: PieceType) -> i32 {
        i32::from(self.v[pc.0 as usize][to.0 as usize][cap.0 as usize].get())
    }

    pub fn update(&self, pc: Piece, to: Square, cap: PieceType, bonus: i32) {
        let entry = &self.v[pc.0 as usize][to.0 as usize][cap.0 as usize];
        let mut val = entry.get();
        val += (bonus * 2 - i32::from(val) * bonus.abs() / 324) as i16;
        entry.set(val);
    }
}

pub struct CounterMoveHistory {
    v: [[Cell<Move>; 64]; 16],
}

impl CounterMoveHistory {
    pub fn get(&self, pc: Piece, s: Square) -> Move {
        self.v[pc.0 as usize][s.0 as usize].get()
    }

    pub fn set(&self, pc: Piece, s: Square, m: Move) {
        self.v[pc.0 as usize][s.0 as usize].set(m);
    }
}

pub struct ContinuationHistory {
    v: [[PieceToHistory; 64]; 16],
}

impl ContinuationHistory {
    pub fn get(&self, pc: Piece, s: Square) -> &'static PieceToHistory {
        let p: *const PieceToHistory = &self.v[pc.0 as usize][s.0 as usize];
        unsafe { &*p }
    }

    pub fn init(&self) {
        let p = self.get(Piece(0), Square(0));
        for pc in 0..16 {
            for s in 0..64 {
                p.v[pc][s].set(search::CM_THRESHOLD as i16 - 1);
            }
        }
    }
}

// MovePicker structs are used to pick one pseudo-legal move at a time from
// the current position. The most important method is next_move(), which
// returns a new pseudo-legal move each time it is called, until there are
// no moves left, when MOVE_NONE is returned. In order to improve the
// efficiency of the alpha beta algorithm, MovePicker attempts to return the
// moves which are most likely to get a cut off first.
pub struct MovePicker {
    cur: usize,
    end_moves: usize,
    end_bad_captures: usize,
    stage: GameStage,
    depth: Depth,
    tt_move: Move,
    countermove: Move,
    killers: [Move; 2],
    cmh: [&'static PieceToHistory; 3],
    list: [ExtMove; MAX_MOVES],
}

pub struct MovePickerQ {
    cur: usize,
    end_moves: usize,
    stage: GameStage,
    depth: Depth,
    tt_move: Move,
    recapture_square: Square,
    list: [ExtMove; MAX_MOVES],
}

pub struct MovePickerPC {
    cur: usize,
    end_moves: usize,
    stage: GameStage,
    tt_move: Move,
    threshold: Value,
    list: [ExtMove; MAX_MOVES],
}

#[derive(Debug, Clone, Copy)]
enum GameStage {
    MainSearch = 0,
    CapturesInit = 1,
    GoodCaptures = 2,
    Killers = 3,
    CounterMove = 4,
    QuietInit = 5,
    Quiet = 6,
    BadCaptures = 7,
    Evasion = 8,
    EvasionsInit = 9,
    AllEvasions = 10,
    ProbCut = 11,
    ProbCutInit = 12,
    ProbCutCaptures = 13,
    QSearch = 14,
    QCapturesInit = 15,
    QCaptures = 16,
    QChecks = 17,
    Invalid = 18,
}

impl GameStage {
    pub fn next(self) -> Self {
        let next_value = self as u8 + 1;
        GameStage::from(next_value)
    }

    pub fn from(value: u8) -> Self {
        match value {
            0 => GameStage::MainSearch,
            1 => GameStage::CapturesInit,
            2 => GameStage::GoodCaptures,
            3 => GameStage::Killers,
            4 => GameStage::CounterMove,
            5 => GameStage::QuietInit,
            6 => GameStage::Quiet,
            7 => GameStage::BadCaptures,
            8 => GameStage::Evasion,
            9 => GameStage::EvasionsInit,
            10 => GameStage::AllEvasions,
            11 => GameStage::ProbCut,
            12 => GameStage::ProbCutInit,
            13 => GameStage::ProbCutCaptures,
            14 => GameStage::QSearch,
            15 => GameStage::QCapturesInit,
            16 => GameStage::QCaptures,
            17 => GameStage::QChecks,
            _ => GameStage::Invalid,
        }
    }
}

// partial_insertion_sort() sorts moves in descending order up to and
// including a given limit.
fn partial_insertion_sort(list: &mut [ExtMove], limit: i32) {
    let mut sorted_end = 0;

    for p in 1..list.len() {
        if list[p].value >= limit {
            let tmp = list[p];
            sorted_end += 1;
            list[p] = list[sorted_end];

            let mut pos = sorted_end;
            while pos > 0 && list[pos - 1].value < tmp.value {
                list[pos] = list[pos - 1];
                pos -= 1;
            }

            list[pos] = tmp;
        }
    }
}

// pick_best() finds the best move in the list and moves it to the front.
// Calling pick_best() is faster than sorting all the moves in advance if
// there are few moves, e.g. the possible captures.
fn pick_best(list: &mut [ExtMove]) -> Move {
    if let Some((q, _)) = list.iter().enumerate().max_by_key(|&(_, x)| x.value) {
        list.swap(0, q);
    }
    list[0].m
}

// score_*() assigns a numerical value to each move in a list, to be used
// for sorting.

// Captures are ordered by Most Valuable Victim (MVV), preferring captures
// with a good history.
fn score_captures(pos: &Position, list: &mut [ExtMove]) {
    for m in list.iter_mut() {
        let x = pos.piece_on(m.m.to());
        m.value = piece_value(MG, x).0
            + pos
                .capture_history
                .get(pos.moved_piece(m.m), m.m.to(), x.piece_type());
    }
}

// Quiets are ordered using the histories.
fn score_quiets(pos: &Position, mp: &mut MovePicker) {
    let list = &mut mp.list[mp.cur..mp.end_moves];
    for m in list.iter_mut() {
        let mp_mm = pos.moved_piece(m.m);
        m.value = pos.main_history.get(pos.side_to_move(), m.m)
            + mp.cmh[0].get(mp_mm, m.m.to())
            + mp.cmh[1].get(mp_mm, m.m.to())
            + mp.cmh[2].get(mp_mm, m.m.to());
    }
}

fn score_evasions(pos: &Position, list: &mut [ExtMove]) {
    for m in list.iter_mut() {
        m.value = if pos.capture(m.m) {
            piece_value(MG, pos.piece_on(m.m.to())).0 - pos.moved_piece(m.m).piece_type().0 as i32
        } else {
            pos.main_history.get(pos.side_to_move(), m.m) - (1 << 28)
        }
    }
}

// Implementations of the MovePicker classes. As arguments we pass information
// to help it return the (presumably) good moves first, to decide which moves
// to return (in the quiescence search, for instance, we only want to search
// captures, promotions and some checks) and how important good move ordering
// is at the current node.
impl MovePicker {
    pub fn new(pos: &Position, ttm: Move, d: Depth, ss: &[search::Stack]) -> MovePicker {
        let mut stage = if pos.checkers() != 0 {
            GameStage::Evasion
        } else {
            GameStage::MainSearch
        };
        let tt_move = if ttm != Move::NONE && pos.pseudo_legal(ttm) {
            ttm
        } else {
            Move::NONE
        };
        if tt_move == Move::NONE {
            stage = stage.next();
        }
        let prev_sq = ss[4].current_move.to();
        MovePicker {
            cur: 0,
            end_moves: 0,
            end_bad_captures: 0,
            stage,
            tt_move: ttm,
            countermove: pos.counter_moves.get(pos.piece_on(prev_sq), prev_sq),
            killers: [ss[5].killers[0], ss[5].killers[1]],
            depth: d,
            cmh: [ss[4].cont_history, ss[3].cont_history, ss[1].cont_history],
            list: [ExtMove {
                m: Move::NONE,
                value: 0,
            }; MAX_MOVES],
        }
    }

    pub fn next_move(&mut self, pos: &Position, skip_quiets: bool) -> Move {
        loop {
            match self.stage {
                GameStage::MainSearch | GameStage::Evasion => {
                    self.stage = self.stage.next();
                    return self.tt_move;
                }

                GameStage::CapturesInit => {
                    self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                    score_captures(pos, &mut self.list[..self.end_moves]);
                    self.stage = self.stage.next();
                }

                GameStage::GoodCaptures => {
                    while self.cur < self.end_moves {
                        let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                        self.cur += 1;
                        if m != self.tt_move {
                            if pos.see_ge(m, Value(-55 * self.list[self.cur - 1].value / 1024)) {
                                return m;
                            }

                            // Losing capture. Move it to the beginning of the
                            // array.
                            self.list[self.end_bad_captures].m = m;
                            self.end_bad_captures += 1;
                        }
                    }
                    self.stage = self.stage.next();
                    let m = self.killers[0];
                    if m != Move::NONE
                        && m != self.tt_move
                        && pos.pseudo_legal(m)
                        && !pos.capture(m)
                    {
                        return m;
                    }
                }

                GameStage::Killers => {
                    self.stage = self.stage.next();
                    let m = self.killers[1];
                    if m != Move::NONE
                        && m != self.tt_move
                        && pos.pseudo_legal(m)
                        && !pos.capture(m)
                    {
                        return m;
                    }
                }

                GameStage::CounterMove => {
                    self.stage = self.stage.next();
                    let m = self.countermove;
                    if m != Move::NONE
                        && m != self.tt_move
                        && m != self.killers[0]
                        && m != self.killers[1]
                        && pos.pseudo_legal(m)
                        && !pos.capture(m)
                    {
                        return m;
                    }
                }

                GameStage::QuietInit => {
                    self.cur = self.end_bad_captures;
                    self.end_moves = generate::<Quiets>(pos, &mut self.list, self.cur);
                    score_quiets(pos, self);
                    partial_insertion_sort(
                        &mut self.list[self.cur..self.end_moves],
                        -4000 * self.depth.value(),
                    );
                    self.stage = self.stage.next();
                }

                GameStage::Quiet => {
                    if !skip_quiets {
                        while self.cur < self.end_moves {
                            let m = self.list[self.cur].m;
                            self.cur += 1;
                            if m != self.tt_move
                                && m != self.killers[0]
                                && m != self.killers[1]
                                && m != self.countermove
                            {
                                return m;
                            }
                        }
                    }
                    self.stage = self.stage.next();
                    self.cur = 0; // Point to beginning of bad captures
                }

                GameStage::BadCaptures => {
                    if self.cur < self.end_bad_captures {
                        let m = self.list[self.cur].m;
                        self.cur += 1;
                        return m;
                    }
                    break;
                }

                GameStage::EvasionsInit => {
                    self.cur = 0;
                    self.end_moves = generate::<Evasions>(pos, &mut self.list, 0);
                    score_evasions(pos, &mut self.list[..self.end_moves]);
                    self.stage = self.stage.next();
                }

                GameStage::AllEvasions => {
                    while self.cur < self.end_moves {
                        let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                        self.cur += 1;
                        if m != self.tt_move {
                            return m;
                        }
                    }
                    break;
                }

                _ => {
                    panic!("movepick")
                }
            }
        }

        Move::NONE
    }
}

impl MovePickerQ {
    pub fn new(pos: &Position, ttm: Move, d: Depth, s: Square) -> MovePickerQ {
        let mut stage = if pos.checkers() != 0 {
            GameStage::Evasion
        } else {
            GameStage::QSearch
        };
        let tt_move = if ttm != Move::NONE
            && pos.pseudo_legal(ttm)
            && (d > Depth::QS_RECAPTURES || ttm.to() == s)
        {
            ttm
        } else {
            stage = stage.next();
            Move::NONE
        };

        MovePickerQ {
            cur: 0,
            end_moves: 0,
            stage,
            depth: d,
            tt_move,
            recapture_square: s,
            list: [ExtMove {
                m: Move::NONE,
                value: 0,
            }; MAX_MOVES],
        }
    }

    pub fn next_move(&mut self, pos: &Position) -> Move {
        loop {
            match self.stage {
                GameStage::Evasion | GameStage::QSearch => {
                    self.stage = self.stage.next();
                    return self.tt_move;
                }

                GameStage::EvasionsInit => {
                    self.cur = 0;
                    self.end_moves = generate::<Evasions>(pos, &mut self.list, 0);
                    score_evasions(pos, &mut self.list[..self.end_moves]);
                    self.stage = self.stage.next();
                }

                GameStage::AllEvasions => {
                    while self.cur < self.end_moves {
                        let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                        self.cur += 1;
                        if m != self.tt_move {
                            return m;
                        }
                    }
                    break;
                }

                GameStage::QCapturesInit => {
                    self.cur = 0;
                    self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                    score_captures(pos, &mut self.list[..self.end_moves]);
                    self.stage = self.stage.next();
                }

                GameStage::QCaptures => {
                    while self.cur < self.end_moves {
                        let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                        self.cur += 1;
                        if m != self.tt_move
                            && (self.depth > Depth::QS_RECAPTURES
                                || m.to() == self.recapture_square)
                        {
                            return m;
                        }
                    }
                    if self.depth <= Depth::QS_NO_CHECKS {
                        break;
                    }
                    self.cur = 0;
                    self.end_moves = generate::<QuietChecks>(pos, &mut self.list, 0);
                    self.stage = self.stage.next();
                }

                GameStage::QChecks => {
                    while self.cur < self.end_moves {
                        let m = self.list[self.cur].m;
                        self.cur += 1;
                        if m != self.tt_move {
                            return m;
                        }
                    }
                    break;
                }

                _ => {
                    panic!("movepick_q")
                }
            }
        }

        Move::NONE
    }
}

impl MovePickerPC {
    pub fn new(pos: &Position, ttm: Move, threshold: Value) -> MovePickerPC {
        let tt_move;
        let stage;

        if ttm != Move::NONE
            && pos.pseudo_legal(ttm)
            && pos.capture(ttm)
            && pos.see_ge(ttm, threshold)
        {
            tt_move = ttm;
            stage = GameStage::ProbCut;
        } else {
            tt_move = Move::NONE;
            stage = GameStage::ProbCut.next();
        }

        MovePickerPC {
            cur: 0,
            end_moves: 0,
            stage,
            tt_move,
            threshold,
            list: [ExtMove {
                m: Move::NONE,
                value: 0,
            }; MAX_MOVES],
        }
    }

    pub fn next_move(&mut self, pos: &Position) -> Move {
        loop {
            match self.stage {
                GameStage::ProbCut => {
                    self.stage = self.stage.next();
                    return self.tt_move;
                }

                GameStage::ProbCutInit => {
                    self.cur = 0;
                    self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                    score_captures(pos, &mut self.list[..self.end_moves]);
                    self.stage = self.stage.next();
                }

                GameStage::ProbCutCaptures => {
                    while self.cur < self.end_moves {
                        let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                        self.cur += 1;
                        if m != self.tt_move && pos.see_ge(m, self.threshold) {
                            return m;
                        }
                    }
                    break;
                }

                _ => {
                    panic!("movepick_pc")
                }
            }
        }

        Move::NONE
    }
}

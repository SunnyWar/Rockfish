// SPDX-License-Identifier: GPL-3.0-or-later

use crate::material;
use crate::movegen::{Legal, MoveList};
use crate::pawns;
use crate::position::Position;
use crate::search::{mainthread_search, thread_search, LimitsType, RootMove, RootMoves, LIMITS};
use crate::tb;
use crate::types::{depth::Depth, Move, Value};
use crate::ucioption;

use std::cell::Cell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{channel, Sender};
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::thread;

pub struct PosData {
    pub fen: String,
    pub moves: Vec<Move>,
}

pub struct SearchResult {
    pub depth: Depth,
    pub score: Value,
    pub pv: Vec<Move>,
}

pub struct ThreadState {
    pub exit: bool,
    pub searching: bool,
    pub clear: bool,
}

pub struct CommonState {
    pub root_moves: Arc<RootMoves>,
    pub pos_data: Arc<RwLock<PosData>>,
    pub result: Arc<Mutex<SearchResult>>,
}

pub struct ThreadCtrl {
    pub idx: usize,
    pub state: Mutex<ThreadState>,
    pub common: Mutex<CommonState>,
    pub cv: Condvar,
    // nodes and tb_hits are Cell<u64> for lack of atomic u64 types
    pub nodes: Cell<u64>,
    pub tb_hits: Cell<u64>,
}

impl ThreadCtrl {
    pub fn new(idx: usize) -> ThreadCtrl {
        ThreadCtrl {
            idx,
            state: Mutex::new(ThreadState {
                exit: false,
                searching: true,
                clear: false,
            }),
            common: Mutex::new(CommonState {
                root_moves: Arc::new(Vec::new()),
                pos_data: Arc::new(RwLock::new(PosData {
                    fen: String::new(),
                    moves: Vec::new(),
                })),
                result: Arc::new(Mutex::new(SearchResult {
                    depth: Depth::ZERO,
                    score: -Value::INFINITE,
                    pv: Vec::new(),
                })),
            }),
            cv: Condvar::new(),
            nodes: Cell::new(0),
            tb_hits: Cell::new(0),
        }
    }
}

// Remove next line when nodes and tb_hits can be made atomic u64
unsafe impl Sync for ThreadCtrl {}

type Handlers = Vec<thread::JoinHandle<()>>;
type Threads = Vec<Arc<ThreadCtrl>>;

static mut HANDLERS: *mut Handlers = 0 as *mut Handlers;
static mut THREADS: *mut Threads = 0 as *mut Threads;

static STOP: AtomicBool = AtomicBool::new(false);
static PONDER: AtomicBool = AtomicBool::new(false);
static STOP_ON_PONDERHIT: AtomicBool = AtomicBool::new(false);

pub fn stop() -> bool {
    STOP.load(Ordering::Relaxed)
}

pub fn ponder() -> bool {
    PONDER.load(Ordering::Relaxed)
}

pub fn stop_on_ponderhit() -> bool {
    STOP_ON_PONDERHIT.load(Ordering::Relaxed)
}

pub fn set_stop(b: bool) {
    STOP.store(b, Ordering::SeqCst);
}

pub fn set_ponder(b: bool) {
    PONDER.store(b, Ordering::SeqCst);
}

pub fn set_stop_on_ponderhit(b: bool) {
    STOP_ON_PONDERHIT.store(b, Ordering::SeqCst);
}

pub fn init(requested: u16) {
    let handlers: Box<Handlers> = Box::default();
    let threads: Box<Threads> = Box::default();
    unsafe {
        HANDLERS = Box::into_raw(handlers);
        THREADS = Box::into_raw(threads);
    }

    set(requested);
}

pub fn free() {
    set(0);
    unsafe {
        std::mem::drop(Box::from_raw(HANDLERS));
        std::mem::drop(Box::from_raw(THREADS));
    }
}

pub fn set(requested: u16) {
    let mut handlers = unsafe { Box::from_raw(HANDLERS) };
    let mut threads = unsafe { Box::from_raw(THREADS) };
    while handlers.len() < requested as usize {
        let idx = handlers.len();
        let (tx, rx) = channel();
        // 16 MB stacks are now too small in debug mode, so use 32 MB stacks
        let builder = thread::Builder::new().stack_size(32 * 1024 * 1024);
        let handler = builder.spawn(move || run_thread(idx, tx)).unwrap();
        let th = rx.recv().unwrap();
        handlers.push(handler);
        threads.push(th);
    }
    while handlers.len() > requested as usize {
        let handler = handlers.pop().unwrap();
        let th = threads.pop().unwrap();
        wake_up(&th, true, false);
        let _ = handler.join();
    }
    std::mem::forget(handlers);
    std::mem::forget(threads);
}

fn run_thread(idx: usize, tx: Sender<Arc<ThreadCtrl>>) {
    let mut pos = Box::new(Position::new());
    pos.pawns_table.reserve_exact(16384);
    for _ in 0..16384 {
        pos.pawns_table
            .push(std::cell::UnsafeCell::new(pawns::Entry::new()));
    }
    pos.material_table.reserve_exact(8192);
    for _ in 0..8192 {
        pos.material_table
            .push(std::cell::UnsafeCell::new(material::Entry::new()));
    }
    pos.is_main = idx == 0;
    pos.thread_idx = idx as i16;
    let th = Arc::new(ThreadCtrl::new(idx));
    tx.send(th.clone()).unwrap();
    pos.thread_ctrl = Some(th.clone());
    pos.previous_time_reduction = 1.;
    pos.cont_history.init();

    loop {
        let mut state = th.state.lock().unwrap();
        state.searching = false;
        th.cv.notify_one();
        while !state.searching {
            state = th.cv.wait(state).unwrap();
        }
        if state.exit {
            break;
        }
        if state.clear {
            // Clear this thread as part of ucinewgame
            if th.idx == 0 {
                pos.previous_score = Value::INFINITE;
                pos.previous_time_reduction = 1.;
            }
            pos.counter_moves = unsafe { std::mem::zeroed() };
            pos.main_history = unsafe { std::mem::zeroed() };
            pos.capture_history = unsafe { std::mem::zeroed() };
            pos.cont_history = unsafe { std::mem::zeroed() };
            pos.cont_history.init();
            state.clear = false;
            continue;
        }
        {
            let common = th.common.lock().unwrap();
            let pos_data = common.pos_data.read().unwrap();
            pos.init_states();
            pos.set(&pos_data.fen, ucioption::get_bool("UCI_Chess960"));
            for &m in &pos_data.moves {
                let gives_check = pos.gives_check(m);
                pos.do_move(m, gives_check);
            }
            let fen = pos.fen();
            pos.set(&fen, ucioption::get_bool("UCI_Chess960"));
            pos.root_moves = (*common.root_moves).clone();
        } // Locks are dropped here
        pos.nodes = 0;
        pos.tb_hits = 0;
        if th.idx == 0 {
            mainthread_search(&mut pos, &th);
        } else {
            thread_search(&mut pos, &th);
            let lock = th.common.lock().unwrap();
            let result = &mut lock.result.lock().unwrap();
            if pos.root_moves[0].score > result.score
                && (pos.completed_depth >= result.depth
                    || pos.root_moves[0].score >= Value::MATE_IN_MAX_PLY)
            {
                result.depth = pos.completed_depth;
                result.score = pos.root_moves[0].score;
                result.pv = pos.root_moves[0].pv.clone();
            }
        }
    }
}

fn wake_up(th: &ThreadCtrl, exit: bool, clear: bool) {
    let mut state = th.state.lock().unwrap();
    state.searching = true;
    state.exit = exit;
    state.clear = clear;
    th.cv.notify_one();
}

pub fn wake_up_slaves() {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    for th in threads.iter() {
        if th.idx != 0 {
            wake_up(th, false, false);
        }
    }

    std::mem::forget(threads);
}

pub fn clear_search() {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    for th in threads.iter() {
        wake_up(th, false, true);
    }

    std::mem::forget(threads);
}

pub fn wait_for_main() {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    for th in threads.iter() {
        if th.idx == 0 {
            let mut state = th.state.lock().unwrap();
            while state.searching {
                state = th.cv.wait(state).unwrap();
            }
        }
    }

    std::mem::forget(threads);
}

pub fn wait_for_slaves() {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    for th in threads.iter() {
        if th.idx != 0 {
            let mut state = th.state.lock().unwrap();
            while state.searching {
                state = th.cv.wait(state).unwrap();
            }
        }
    }

    std::mem::forget(threads);
}

pub fn wait_for_all() {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    for th in threads.iter() {
        let mut state = th.state.lock().unwrap();
        while state.searching {
            state = th.cv.wait(state).unwrap();
        }
    }

    std::mem::forget(threads);
}

pub fn start_thinking(
    pos: &mut Position,
    pos_data: &Arc<RwLock<PosData>>,
    limits: &LimitsType,
    searchmoves: Vec<Move>,
    ponder_mode: bool,
) {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    wait_for_main();

    set_stop_on_ponderhit(false);
    set_stop(false);
    set_ponder(ponder_mode);

    unsafe {
        LIMITS = (*limits).clone();
    }

    let mut root_moves = RootMoves::new();
    for m in MoveList::new::<Legal>(pos) {
        if searchmoves.is_empty() || searchmoves.iter().any(|&x| x == m) {
            root_moves.push(RootMove::new(m));
        }
    }

    tb::read_options();
    tb::rank_root_moves(pos, &mut root_moves);

    let root_moves = Arc::new(root_moves);
    let result = Arc::new(Mutex::new(SearchResult {
        depth: Depth::ZERO,
        score: -Value::INFINITE,
        pv: Vec::new(),
    }));

    for th in threads.iter() {
        th.nodes.set(0);
        th.tb_hits.set(0);
        let mut common = th.common.lock().unwrap();
        common.root_moves = root_moves.clone();
        common.pos_data = pos_data.clone();
        common.result = result.clone();
    }

    wake_up(&threads[0], false, false);

    std::mem::forget(threads);
}

pub fn nodes_searched() -> u64 {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    let mut nodes = 0;

    for th in threads.iter() {
        nodes += th.nodes.get();
    }

    std::mem::forget(threads);

    nodes
}

pub fn tb_hits() -> u64 {
    let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };

    let mut tb_hits = 0;

    for th in threads.iter() {
        tb_hits += th.tb_hits.get();
    }

    std::mem::forget(threads);

    tb_hits
}

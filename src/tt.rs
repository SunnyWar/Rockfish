// SPDX-License-Identifier: GPL-3.0-or-later

use crate::types::{bound::Bound, depth::Depth, key::Key, Move, Value};

// TTEntry struct is the 10 bytes transposition-table entry, defined as below:
//
// key        16 bit
// move       16 bit
// value      16 bit
// eval value 16 bit
// generation  6 bit
// bound type  2 bit
// depth       8 bit

pub struct TTEntry {
    key16: u16,
    move16: u16,
    value16: i16,
    eval16: i16,
    gen_bound8: u8,
    depth8: i8,
}

impl TTEntry {
    pub fn mov(&self) -> Move {
        Move(u32::from(self.move16))
    }

    pub fn value(&self) -> Value {
        Value(i32::from(self.value16))
    }

    pub fn eval(&self) -> Value {
        Value(i32::from(self.eval16))
    }

    pub fn depth(&self) -> Depth {
        Depth(i32::from(self.depth8))
    }

    pub fn bound(&self) -> Bound {
        Bound(u32::from(self.gen_bound8 & 3))
    }

    #[allow(clippy::too_many_arguments)]
    pub fn save(
        &mut self,
        key: Key,
        value: Value,
        bound: Bound,
        depth: Depth,
        mov: Move,
        eval: Value,
        generation: u8,
    ) {
        let key16 = (key.0 >> 48) as u16;

        // Preserve any existing move for the same position
        if mov != Move::NONE || key16 != self.key16 {
            self.move16 = mov.0 as u16;
        }

        // Don't overwrite more valuable entries
        if key16 != self.key16 || (depth.value()) as i8 > self.depth8 - 4 || bound == Bound::EXACT {
            self.key16 = key16;
            self.value16 = value.0 as i16;
            self.eval16 = eval.0 as i16;
            self.gen_bound8 = generation | (bound.0 as u8);
            self.depth8 = (depth.value()) as i8;
        }
    }
}

// The transposition table consists of a power of 2 number of clusters.
// Each cluster consists of ClusterSize number of TTEntry. Each non-empty
// entry contains information about exactly one position. The size of a
// cluster should divide the size of a cache line size, to ensure that
// clusters never cross cache lines. This ensures best cache performance,
// as the cacheline is prefetched, as soon as possible.

const CLUSTER_SIZE: usize = 3;

struct Cluster {
    entry: [TTEntry; CLUSTER_SIZE],
    _padding: [u8; 2], // Align to a divisor of the cache line size
}

static mut CLUSTER_COUNT: usize = 0;
static mut TABLE: *mut Cluster = 0 as *mut Cluster;
static mut TABLE_CAP: usize = 0;
static mut GENERATION8: u8 = 0;

pub fn new_search() {
    unsafe {
        GENERATION8 += 4; // Lower two bits are used by bound
    }
}

pub fn generation() -> u8 {
    unsafe { GENERATION8 }
}

// The lowest order bits of the key are used to get the index of the cluster
fn cluster(key: Key) -> &'static mut Cluster {
    unsafe {
        let p: *mut Cluster =
            TABLE.offset(((u64::from(key.0 as u32) * (CLUSTER_COUNT as u64)) >> 32) as isize);
        let c: &'static mut Cluster = &mut *p;
        c
    }
}

// tt::resize() sets the size of the transposition table, measured in
// megabytes. The transposition table consists of a power of 2 number of
// clusters and each cluster consists of CLUSTER_SIZE number of TTEntry.
pub fn resize(mb_size: usize) {
    let new_cluster_count = mb_size * 1024 * 1024 / std::mem::size_of::<Cluster>();

    unsafe {
        if new_cluster_count == CLUSTER_COUNT {
            return;
        }

        free();

        CLUSTER_COUNT = new_cluster_count;

        let mut v: Vec<Cluster> = Vec::with_capacity(new_cluster_count);
        TABLE = v.as_mut_ptr();
        TABLE_CAP = v.capacity();
        // forget in order to prevent deallocation by dropping
        std::mem::forget(v);
    }
}

// tt::free() deallocates the transposition table.
pub fn free() {
    unsafe {
        if !TABLE.is_null() {
            let _ = Vec::from_raw_parts(TABLE, 0, TABLE_CAP);
            // deallocate by dropping
        }
    }
}

// tt::clear() clears the entire transposition table. It is called whenever
// the table is resized or when the user asks the program to clear the table
// (via the UCI interface).
pub fn clear() {
    let tt_slice = unsafe { std::slice::from_raw_parts_mut(TABLE, CLUSTER_COUNT) };

    for cluster in tt_slice.iter_mut() {
        for tte in &mut cluster.entry {
            tte.key16 = 0;
            tte.move16 = 0;
            tte.value16 = 0;
            tte.eval16 = 0;
            tte.gen_bound8 = 0;
            tte.depth8 = 0;
            tte.key16 = 0;
        }
    }
}

// tt::probe() looks up the current position in the transposition table. It
// returns true and a pointer to the TTentry if the position is found.
// Otherwise, it returns false and a pointer to an empty or least valuable
// TTEntry to be replaced later. The replace value of an entry is calculated
// as its depth minus 8 times its relative age. TTEntry t1 is considered more
// valuable than TTEntry t2 if its replace value is greater than that of t2.
pub fn probe(key: Key) -> (&'static mut TTEntry, bool) {
    let cl = cluster(key);
    // Use the high 16 bits of the hash key as key inside the cluster
    let key16 = (key.0 >> 48) as u16;

    for i in 0..CLUSTER_SIZE {
        let clkey = cl.entry[i].key16;
        if clkey == 0 || clkey == key16 {
            if cl.entry[i].gen_bound8 & 0xfc != generation() && clkey != 0 {
                cl.entry[i].gen_bound8 = generation() | (cl.entry[i].bound().0 as u8);
            }
            let found = clkey != 0;
            return (&mut (cl.entry[i]), found);
        }
    }

    // Find an entry to be replaced according to the replacement strategy
    let mut replacement_index = 0;
    let current_generation = i32::from(generation());
    const OFFSET: i32 = 259;

    for i in 1..CLUSTER_SIZE {
        let r_entry = &cl.entry[replacement_index];
        let i_entry = &cl.entry[i];

        let r_entry_age = (OFFSET + current_generation - i32::from(r_entry.gen_bound8)) & 0xfc;
        let i_entry_age = (OFFSET + current_generation - i32::from(i_entry.gen_bound8)) & 0xfc;

        let r_score = i32::from(r_entry.depth8) - r_entry_age * 2;
        let i_score = i32::from(i_entry.depth8) - i_entry_age * 2;

        if r_score > i_score {
            replacement_index = i;
        }
    }

    (&mut (cl.entry[replacement_index]), false)
}

// tt::hashfull() returns an approximation of the hashtable occupation during
// a search. The hash is x permill full, as per UCI protocol.
pub fn hashfull() -> i32 {
    let tt_slice = unsafe { std::slice::from_raw_parts(TABLE, 1000 / CLUSTER_SIZE) };
    let mut count = 0;
    let current_generation = generation();

    for cluster in tt_slice {
        for entry in &cluster.entry {
            if entry.gen_bound8 & 0xfc == current_generation {
                count += 1;
            }
        }
    }

    count
}

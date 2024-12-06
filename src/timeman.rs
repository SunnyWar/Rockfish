// SPDX-License-Identifier: GPL-3.0-or-later

use crate::search;
use crate::types::Color;
use crate::ucioption;

static mut START_TIME: Option<std::time::Instant> = None;
static mut OPTIMUM_TIME: i64 = 0;
static mut MAXIMUM_TIME: i64 = 0;

pub fn optimum() -> i64 {
    unsafe { OPTIMUM_TIME }
}

pub fn maximum() -> i64 {
    unsafe { MAXIMUM_TIME }
}

pub fn elapsed() -> i64 {
    let duration = unsafe { START_TIME.unwrap().elapsed() };
    (duration.as_secs() * 1000 + u64::from(duration.subsec_millis())) as i64
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum TimeType {
    OptimumTime,
    MaxTime,
}
use self::TimeType::{MaxTime, OptimumTime};

// Plan time management at most this many moves ahread
const MOVE_HORIZON: i32 = 50;
// When in trouble, we can step over reserved time with this ratio
const MAX_RATIO: f64 = 7.3;
// But we must not steal time from remaining moves over this ratio
const STEAL_RATIO: f64 = 0.34;

// importance() is a skew-logistic function based on naive statistical
// analysis of "how many games are still undecided after n half moves".
// The game is considered "undecided" as long as neither side has >275cp
// advantage. Data was extracted from the CCRL game database with some
// simply filtering criteria.

fn importance(ply: i32) -> f64 {
    const XSCALE: f64 = 6.85;
    const XSHIFT: f64 = 64.5;
    const SKEW: f64 = 0.171;

    let scaled_ply = (f64::from(ply) - XSHIFT) / XSCALE;
    let exp_term = scaled_ply.exp();
    let importance_value = (1.0 + exp_term).powf(-SKEW);

    importance_value + f64::MIN_POSITIVE
}

fn remaining(my_time: i64, movestogo: i32, ply: i32, slow_mover: i64, time_type: TimeType) -> i64 {
    // Determine max_ratio and steal_ratio based on time_type
    let (max_ratio, steal_ratio) = match time_type {
        TimeType::OptimumTime => (1.0, 0.0),
        _ => (MAX_RATIO, STEAL_RATIO),
    };

    // Calculate the importance of the current move
    let move_importance = (importance(ply) * slow_mover as f64) / 128.0;

    // Calculate the importance of other moves
    let other_moves_importance: f64 = (1..movestogo).map(|i| importance(ply + 2 * i)).sum();

    // Calculate the ratios
    let ratio1 =
        max_ratio * move_importance / (max_ratio * move_importance + other_moves_importance);
    let ratio2 = (move_importance + steal_ratio * other_moves_importance)
        / (move_importance + other_moves_importance);

    // Use the minimum of the calculated ratios
    let min_ratio = ratio1.min(ratio2);

    // Return the remaining time
    (my_time as f64 * min_ratio) as i64
}

// init() is called at the beginning of the search and calculates the allowed
// thinking time out of the time control and current game ply. We support
// four different kinds of time controls, passed in 'limits':
//
//  inc == 0 && movestogo == 0 means: x basetime  [sudden death!]
//  inc == 0 && movestogo != 0 means: x moves in y minutes
//  inc >  0 && movestogo == 0 means: x basetime + z increment
//  inc >  0 && movestogo != 0 means: x moves in y minutes + z increment

pub fn init(limits: &mut search::LimitsType, us: Color, ply: i32) {
    let min_think_time = i64::from(ucioption::get_u32("Minimum Thinking Time"));
    let move_overhead = i64::from(ucioption::get_u32("Move Overhead"));
    let slow_mover = i64::from(ucioption::get_u32("Slow Mover"));

    unsafe {
        START_TIME = limits.start_time;
        let time = std::cmp::max(limits.time[us.0 as usize], min_think_time);
        OPTIMUM_TIME = time;
        MAXIMUM_TIME = time;
    }

    let max_mtg = if limits.movestogo != 0 {
        std::cmp::min(limits.movestogo, MOVE_HORIZON)
    } else {
        MOVE_HORIZON
    };

    // We calculate optimum time usage for different hypothetical "moves to go"
    // values and choose the minimum of calculated search time values. Usually
    // the greates hyp_mtg givse the minimum values.
    for hyp_mtg in 1..=max_mtg {
        // Calculate thinking time for hypothetical "moves to go" value
        let inc = limits.inc[us.0 as usize];
        let time = limits.time[us.0 as usize];
        let overhead = 2 + i64::from(std::cmp::min(hyp_mtg, 40));

        let mut hyp_my_time = time + inc * i64::from(hyp_mtg - 1) - move_overhead * overhead;
        hyp_my_time = std::cmp::max(hyp_my_time, 0);

        let t1 = min_think_time + remaining(hyp_my_time, hyp_mtg, ply, slow_mover, OptimumTime);
        let t2 = min_think_time + remaining(hyp_my_time, hyp_mtg, ply, slow_mover, MaxTime);

        unsafe {
            OPTIMUM_TIME = std::cmp::min(t1, OPTIMUM_TIME);
            MAXIMUM_TIME = std::cmp::min(t2, MAXIMUM_TIME);
        }
    }

    if ucioption::get_bool("Ponder") {
        unsafe {
            OPTIMUM_TIME += OPTIMUM_TIME / 4;
        }
    }
}

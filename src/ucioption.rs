// SPDX-License-Identifier: GPL-3.0-or-later

use crate::tb;
use crate::threads;
use crate::tt;

type OnChange = Option<fn(&OptVal)>;

struct Opt {
    key: &'static str,
    val: OptVal,
    on_change: OnChange,
}

impl Opt {
    pub fn new(key: &'static str, val: OptVal, on_change: OnChange) -> Opt {
        Opt {
            key,
            val,
            on_change,
        }
    }
}

enum OptVal {
    StringOpt {
        def: &'static str,
        cur: String,
    },
    Spin {
        def: i32,
        cur: i32,
        min: i32,
        max: i32,
    },
    Check {
        def: bool,
        cur: bool,
    },
    Button,
    Combo {
        def: &'static str,
        cur: String,
    },
}

impl OptVal {
    pub fn string(def: &'static str) -> OptVal {
        OptVal::StringOpt {
            def,
            cur: String::from(def),
        }
    }

    pub fn spin(def: i32, min: i32, max: i32) -> OptVal {
        OptVal::Spin {
            def,
            cur: def,
            min,
            max,
        }
    }

    pub fn check(def: bool) -> OptVal {
        OptVal::Check { def, cur: def }
    }

    pub fn combo(def: &'static str) -> OptVal {
        OptVal::Combo {
            def,
            cur: String::from(&def[0..def.find(" var").unwrap()]).to_lowercase(),
        }
    }
}

fn on_clear_hash(_: &OptVal) {
    tt::clear();
}

fn on_hash_size(opt_val: &OptVal) {
    if let &OptVal::Spin { cur, .. } = opt_val {
        tt::resize(cur as usize);
    }
}

fn on_threads(opt_val: &OptVal) {
    if let &OptVal::Spin { cur, .. } = opt_val {
        threads::set(cur as u16);
    }
}

fn on_tb_path(opt_val: &OptVal) {
    if let OptVal::StringOpt { cur, .. } = opt_val {
        tb::init(String::from(cur.as_str()));
    }
}

static mut OPTIONS: *mut Vec<Opt> = 0 as *mut Vec<Opt>;

pub fn init() {
    let mut opts = Box::new(Vec::new());
    opts.push(Opt::new("Contempt", OptVal::spin(18, -100, 100), None));
    opts.push(Opt::new(
        "Analysis Contempt",
        OptVal::combo("Off var Off var White var Black"),
        None,
    ));
    opts.push(Opt::new(
        "Threads",
        OptVal::spin(1, 1, 512),
        Some(on_threads),
    ));
    opts.push(Opt::new(
        "Hash",
        OptVal::spin(16, 1, 128 * 1024),
        Some(on_hash_size),
    ));
    opts.push(Opt::new("Clear Hash", OptVal::Button, Some(on_clear_hash)));
    opts.push(Opt::new("Ponder", OptVal::check(false), None));
    opts.push(Opt::new("MultiPV", OptVal::spin(1, 1, 500), None));
    opts.push(Opt::new("Move Overhead", OptVal::spin(30, 0, 5000), None));
    opts.push(Opt::new(
        "Minimum Thinking Time",
        OptVal::spin(20, 0, 5000),
        None,
    ));
    opts.push(Opt::new("Slow Mover", OptVal::spin(84, 10, 1000), None));
    opts.push(Opt::new("UCI_AnalyseMode", OptVal::check(false), None));
    opts.push(Opt::new("UCI_Chess960", OptVal::check(false), None));
    opts.push(Opt::new(
        "SyzygyPath",
        OptVal::string("<empty>"),
        Some(on_tb_path),
    ));
    opts.push(Opt::new("SyzygyProbeDepth", OptVal::spin(1, 1, 100), None));
    opts.push(Opt::new("Syzygy50MoveRule", OptVal::check(true), None));
    opts.push(Opt::new("SyzygyProbeLimit", OptVal::spin(6, 0, 6), None));
    opts.push(Opt::new("SyzygyUseDTM", OptVal::check(true), None));
    unsafe {
        OPTIONS = Box::into_raw(opts);
    }
}

pub fn free() {
    let _opts = unsafe { Box::from_raw(OPTIONS) };
}

pub fn print() {
    let opts = unsafe { Box::from_raw(OPTIONS) };
    for opt in opts.iter() {
        print!(
            "\noption name {} type {}",
            opt.key,
            match opt.val {
                OptVal::StringOpt { def, .. } => format!("string default {def}"),
                OptVal::Spin { def, min, max, .. } =>
                    format!("spin default {def} min {min} max {max}"),
                OptVal::Check { def, .. } => format!("check default {def}"),
                OptVal::Button => "button".to_string(),
                OptVal::Combo { def, .. } => format!("combo default {def}"),
            }
        );
    }
    println!();
    std::mem::forget(opts);
}

pub fn set(key: &str, val: &str) {
    let mut opts = unsafe { Box::from_raw(OPTIONS) };
    if let Some(opt) = opts.iter_mut().find(|o| o.key == key) {
        match opt.val {
            OptVal::StringOpt { ref mut cur, .. } => *cur = String::from(val),
            OptVal::Spin { ref mut cur, .. } => *cur = val.parse().unwrap(),
            OptVal::Check { ref mut cur, .. } => *cur = val == "true",
            OptVal::Button => {}
            OptVal::Combo { ref mut cur, .. } => *cur = String::from(val).to_lowercase(),
        }
        if let Some(on_change) = opt.on_change {
            on_change(&opt.val);
        }
    } else {
        println!("No such option: {key}");
    }
    unsafe {
        OPTIONS = Box::into_raw(opts);
    }
}

pub fn get_i32(key: &str) -> i32 {
    let opts = unsafe { Box::from_raw(OPTIONS) };
    let val = {
        let opt = opts.iter().find(|o| o.key == key).unwrap();
        if let OptVal::Spin { cur, .. } = opt.val {
            cur
        } else {
            0
        }
    };
    std::mem::forget(opts);
    val
}

pub fn get_u32(key: &str) -> u32 {
    // Assuming OPTIONS is some global variable.
    // Ensure OPTIONS is safely accessible.
    let opts = unsafe { &*OPTIONS }; // Use reference without transferring ownership

    if let Some(opt) = opts.iter().find(|o| o.key == key) {
        if let OptVal::Spin { cur, .. } = opt.val {
            return cur as u32;
        }
    }

    0 // Default return value if no match found
}

pub fn get_u16(key: &str) -> u16 {
    let opts = unsafe { &*OPTIONS }; // Use reference without transferring ownership

    if let Some(opt) = opts.iter().find(|o| o.key == key) {
        if let OptVal::Spin { cur, .. } = opt.val {
            return cur as u16;
        }
    }

    0 // Default return value if no match found
}

pub fn get_bool(key: &str) -> bool {
    let opts = unsafe { Box::from_raw(OPTIONS) };
    let val = {
        let opt = opts.iter().find(|o| o.key == key).unwrap();
        if let OptVal::Check { cur, .. } = opt.val {
            cur
        } else {
            false
        }
    };
    std::mem::forget(opts);
    val
}

pub fn get_string(key: &str) -> String {
    let opts = unsafe { Box::from_raw(OPTIONS) };
    let val = {
        let opt = opts.iter().find(|o| o.key == key).unwrap();
        if let OptVal::StringOpt { ref cur, .. } = opt.val {
            String::from(cur.as_str())
        } else if let OptVal::Combo { ref cur, .. } = opt.val {
            String::from(cur.as_str())
        } else {
            String::new()
        }
    };
    std::mem::forget(opts);
    val
}

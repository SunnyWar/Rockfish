// SPDX-License-Identifier: GPL-3.0-or-later

#[derive(Clone, Copy)]
pub struct Prng(u64);

// xorshift64star Pseudo-Random Number Generator
// This class is based on original code written and dedicated
// to the public domain by Sebastiano Vigna (2014).
// It has the following characteristics:
//
//  -  Outputs 64-bit numbers
//  -  Passes Dieharder and SmallCrush test batteries
//  -  Does not require warm-up, no zeroland to escape
//  -  Internal state is a single 64-bit integer
//  -  Period is 2^64 - 1
//  -  Speed: 1.60 ns/call (Core i7 @3.40GHz)
//
// For further analysis see
//   <http://vigna.di.unimi.it/ftp/papers/xorshift.pdf>
impl Prng {
    pub fn new(seed: u64) -> Prng {
        Prng(seed)
    }

    pub fn rand64(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        u64::wrapping_mul(self.0, 2_685_821_657_736_338_717)
    }
}

pub fn engine_info(to_uci: bool) -> String {
    //    let months = &"Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec";

    format!(
        "Rockfish 10 dev{}",
        if to_uci {
            "\nid author SunnyWar, T. Romstad, M. Costalba, J. Kiiski, G. Linscott"
        } else {
            " by SunnyWar based on Rustfish by Syzygy based on Stockfish"
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_prng() {
        let seed = 123_456_789;
        let prng = Prng::new(seed);
        assert_eq!(prng.0, seed);
    }

    #[test]
    fn test_rand64() {
        let seed = 987_654_321;
        let mut prng = Prng::new(seed);

        // Perform some calls to rand64 and verify the output is not equal to the seed
        let rand1 = prng.rand64();
        let rand2 = prng.rand64();
        assert_eq!(rand1, 18211065302896784785);
        assert_eq!(rand2, 10667432569070492861);
        assert_ne!(rand1, rand2);
    }
}

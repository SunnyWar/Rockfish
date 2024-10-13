#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScaleFactor(pub i32);

impl ScaleFactor {
    pub const DRAW: ScaleFactor = ScaleFactor(0);
    pub const ONEPAWN: ScaleFactor = ScaleFactor(48);
    pub const NORMAL: ScaleFactor = ScaleFactor(64);
    pub const MAX: ScaleFactor = ScaleFactor(128);
    pub const NONE: ScaleFactor = ScaleFactor(255);
}

#[cfg(test)]
mod scale_factor_tests {
    use super::*;

    #[test]
    fn test_scalefactor_constants() {
        assert_eq!(ScaleFactor::DRAW, ScaleFactor(0));
        assert_eq!(ScaleFactor::ONEPAWN, ScaleFactor(48));
        assert_eq!(ScaleFactor::NORMAL, ScaleFactor(64));
        assert_eq!(ScaleFactor::MAX, ScaleFactor(128));
        assert_eq!(ScaleFactor::NONE, ScaleFactor(255));
    }

    #[test]
    fn test_scalefactor_clone() {
        let sf = ScaleFactor::NORMAL;
        let cloned = sf.clone();
        assert_eq!(sf, cloned);
    }

    #[test]
    fn test_scalefactor_copy() {
        let sf = ScaleFactor::MAX;
        let copied = sf;
        assert_eq!(sf, copied);
    }

    #[test]
    fn test_scalefactor_partial_eq() {
        let sf1 = ScaleFactor::ONEPAWN;
        let sf2 = ScaleFactor::ONEPAWN;
        let sf3 = ScaleFactor::DRAW;
        assert_eq!(sf1, sf2);
        assert_ne!(sf1, sf3);
    }
}

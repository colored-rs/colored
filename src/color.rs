use std::{borrow::Cow, str::FromStr};

use crate::control::{get_current_color_level, ColorLevel};

const ANSI_16_COLORS: [(u8, u8, u8, Color); 16] = [
    (0, 0, 0, Color::Black),
    (128, 0, 0, Color::Red),
    (0, 128, 0, Color::Green),
    (128, 128, 0, Color::Yellow),
    (0, 0, 128, Color::Blue),
    (128, 0, 128, Color::Magenta),
    (0, 128, 128, Color::Cyan),
    (192, 192, 192, Color::White),
    (128, 128, 128, Color::BrightBlack),
    (255, 0, 0, Color::BrightRed),
    (0, 255, 0, Color::BrightGreen),
    (255, 255, 0, Color::BrightYellow),
    (0, 0, 255, Color::BrightBlue),
    (255, 0, 255, Color::BrightMagenta),
    (0, 255, 255, Color::BrightCyan),
    (255, 255, 255, Color::BrightWhite),
];

const CUBE_VALUES: [u8; 6] = [0, 0x5f, 0x87, 0xaf, 0xd7, 0xff];

/// The 8 standard colors.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    Ansi256 { idx: u8 },
    TrueColor { r: u8, g: u8, b: u8 },
}

#[allow(missing_docs)]
impl Color {
    #[must_use]
    pub fn to_fg_str(&self) -> Cow<'static, str> {
        match *self {
            Self::Black => "30".into(),
            Self::Red => "31".into(),
            Self::Green => "32".into(),
            Self::Yellow => "33".into(),
            Self::Blue => "34".into(),
            Self::Magenta => "35".into(),
            Self::Cyan => "36".into(),
            Self::White => "37".into(),
            Self::BrightBlack => "90".into(),
            Self::BrightRed => "91".into(),
            Self::BrightGreen => "92".into(),
            Self::BrightYellow => "93".into(),
            Self::BrightBlue => "94".into(),
            Self::BrightMagenta => "95".into(),
            Self::BrightCyan => "96".into(),
            Self::BrightWhite => "97".into(),
            Self::Ansi256 { idx } => format!("38;5;{idx}").into(),
            Self::TrueColor { r, g, b } => match get_current_color_level() {
                ColorLevel::Ansi16 => self.truecolor_fallback_to_ansi16().to_fg_str(),
                ColorLevel::Ansi256 => self.truecolor_fallback_to_ansi256().to_fg_str(),
                ColorLevel::TrueColor | ColorLevel::None => format!("38;2;{r};{g};{b}").into(),
            },
        }
    }

    #[must_use]
    pub fn to_bg_str(&self) -> Cow<'static, str> {
        match *self {
            Self::Black => "40".into(),
            Self::Red => "41".into(),
            Self::Green => "42".into(),
            Self::Yellow => "43".into(),
            Self::Blue => "44".into(),
            Self::Magenta => "45".into(),
            Self::Cyan => "46".into(),
            Self::White => "47".into(),
            Self::BrightBlack => "100".into(),
            Self::BrightRed => "101".into(),
            Self::BrightGreen => "102".into(),
            Self::BrightYellow => "103".into(),
            Self::BrightBlue => "104".into(),
            Self::BrightMagenta => "105".into(),
            Self::BrightCyan => "106".into(),
            Self::BrightWhite => "107".into(),
            Self::Ansi256 { idx } => match get_current_color_level() {
                ColorLevel::Ansi16 => self.ansi256_fallback_to_ansi16().to_bg_str(),
                ColorLevel::Ansi256 | ColorLevel::TrueColor | ColorLevel::None => {
                    format!("48;5;{idx}").into()
                }
            },
            Self::TrueColor { r, g, b } => match get_current_color_level() {
                ColorLevel::Ansi16 => self.truecolor_fallback_to_ansi16().to_bg_str(),
                ColorLevel::Ansi256 => self.truecolor_fallback_to_ansi256().to_bg_str(),
                ColorLevel::TrueColor | ColorLevel::None => format!("48;2;{r};{g};{b}").into(),
            },
        }
    }

    /// Converts an ANSI 256-color to the closest ANSI 16-color palette color.
    #[must_use]
    fn ansi256_fallback_to_ansi16(self) -> Self {
        match self {
            Self::Ansi256 { idx } => {
                let (r, g, b) = ansi256_to_rgb(idx);
                let mut min_distance_sq = u32::MAX;
                let mut closest_color = self;

                for &(cr, cg, cb, color) in &ANSI_16_COLORS {
                    let dr = (i32::from(r) - i32::from(cr)).pow(2) as u32;
                    let dg = (i32::from(g) - i32::from(cg)).pow(2) as u32;
                    let db = (i32::from(b) - i32::from(cb)).pow(2) as u32;
                    let distance_sq = dr + dg + db;

                    if distance_sq < min_distance_sq {
                        min_distance_sq = distance_sq;
                        closest_color = color;
                    }
                }

                closest_color
            }
            _ => unreachable!("This function should only be called on an `Ansi256` variant."),
        }
    }

    /// Converts a `TrueColor` to the closest ANSI 16-color palette color.
    #[must_use]
    fn truecolor_fallback_to_ansi16(self) -> Self {
        match self {
            Self::TrueColor { r, g, b } => {
                let mut min_distance_sq = u32::MAX;
                let mut closest_color = self;

                for &(cr, cg, cb, color) in &ANSI_16_COLORS {
                    let dr = (i32::from(r) - i32::from(cr)).pow(2) as u32;
                    let dg = (i32::from(g) - i32::from(cg)).pow(2) as u32;
                    let db = (i32::from(b) - i32::from(cb)).pow(2) as u32;
                    let distance_sq = dr + dg + db;

                    if distance_sq < min_distance_sq {
                        min_distance_sq = distance_sq;
                        closest_color = color;
                    }
                }

                closest_color
            }
            _ => unreachable!("This function should only be called on a `TrueColor` variant."),
        }
    }

    /// Converts a `TrueColor` to the closest ANSI 256-color palette color.
    #[must_use]
    fn truecolor_fallback_to_ansi256(self) -> Self {
        match self {
            Self::TrueColor { r, g, b } => {
                let mut min_distance_sq = u32::MAX;
                let mut closest_idx = 0;

                for idx in 0u8..=255 {
                    let (cr, cg, cb) = ansi256_to_rgb(idx);
                    let dr = (i32::from(r) - i32::from(cr)).pow(2) as u32;
                    let dg = (i32::from(g) - i32::from(cg)).pow(2) as u32;
                    let db = (i32::from(b) - i32::from(cb)).pow(2) as u32;
                    let distance_sq = dr + dg + db;

                    if distance_sq < min_distance_sq {
                        min_distance_sq = distance_sq;
                        closest_idx = idx;
                    }
                }

                Self::Ansi256 { idx: closest_idx }
            }
            _ => unreachable!("This function should only be called on a `TrueColor` variant."),
        }
    }
}

impl From<&str> for Color {
    fn from(src: &str) -> Self {
        src.parse().unwrap_or(Self::White)
    }
}

impl From<String> for Color {
    fn from(src: String) -> Self {
        src.parse().unwrap_or(Self::White)
    }
}

impl FromStr for Color {
    type Err = ();

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        let src = src.to_lowercase();

        match src.as_ref() {
            "black" => Ok(Self::Black),
            "red" => Ok(Self::Red),
            "green" => Ok(Self::Green),
            "yellow" => Ok(Self::Yellow),
            "blue" => Ok(Self::Blue),
            "magenta" | "purple" => Ok(Self::Magenta),
            "cyan" => Ok(Self::Cyan),
            "white" => Ok(Self::White),
            "bright black" => Ok(Self::BrightBlack),
            "bright red" => Ok(Self::BrightRed),
            "bright green" => Ok(Self::BrightGreen),
            "bright yellow" => Ok(Self::BrightYellow),
            "bright blue" => Ok(Self::BrightBlue),
            "bright magenta" => Ok(Self::BrightMagenta),
            "bright cyan" => Ok(Self::BrightCyan),
            "bright white" => Ok(Self::BrightWhite),
            _ => Err(()),
        }
    }
}

fn ansi256_to_rgb(idx: u8) -> (u8, u8, u8) {
    if idx < 16 {
        let (r, g, b, _) = ANSI_16_COLORS[idx as usize];
        (r, g, b)
    } else if idx <= 231 {
        let idx = idx - 16;
        let r = idx / 36;
        let rem = idx % 36;
        let g = rem / 6;
        let b = rem % 6;
        let r_val = CUBE_VALUES[r as usize];
        let g_val = CUBE_VALUES[g as usize];
        let b_val = CUBE_VALUES[b as usize];
        (r_val, g_val, b_val)
    } else {
        let gray_level = idx - 232;
        let gray_value = 8 + gray_level * 10;
        (gray_value, gray_value, gray_value)
    }
}

#[cfg(test)]
mod tests {
    pub use super::*;

    mod from_str {
        pub use super::*;

        macro_rules! make_test {
            ( $( $name:ident: $src:expr => $dst:expr),* ) => {

                $(
                    #[test]
                    fn $name() {
                        let color : Color = $src.into();
                        assert_eq!($dst, color)
                    }
                )*
            }
        }

        make_test!(
            black: "black" => Color::Black,
            red: "red" => Color::Red,
            green: "green" => Color::Green,
            yellow: "yellow" => Color::Yellow,
            blue: "blue" => Color::Blue,
            magenta: "magenta" => Color::Magenta,
            purple: "purple" => Color::Magenta,
            cyan: "cyan" => Color::Cyan,
            white: "white" => Color::White,
            brightblack: "bright black" => Color::BrightBlack,
            brightred: "bright red" => Color::BrightRed,
            brightgreen: "bright green" => Color::BrightGreen,
            brightyellow: "bright yellow" => Color::BrightYellow,
            brightblue: "bright blue" => Color::BrightBlue,
            brightmagenta: "bright magenta" => Color::BrightMagenta,
            brightcyan: "bright cyan" => Color::BrightCyan,
            brightwhite: "bright white" => Color::BrightWhite,

            invalid: "invalid" => Color::White,
            capitalized: "BLUE" => Color::Blue,
            mixed_case: "bLuE" => Color::Blue
        );
    }

    mod from_string {
        pub use super::*;

        macro_rules! make_test {
            ( $( $name:ident: $src:expr => $dst:expr),* ) => {

                $(
                    #[test]
                    fn $name() {
                        let src = String::from($src);
                        let color : Color = src.into();
                        assert_eq!($dst, color)
                    }
                )*
            }
        }

        make_test!(
            black: "black" => Color::Black,
            red: "red" => Color::Red,
            green: "green" => Color::Green,
            yellow: "yellow" => Color::Yellow,
            blue: "blue" => Color::Blue,
            magenta: "magenta" => Color::Magenta,
            cyan: "cyan" => Color::Cyan,
            white: "white" => Color::White,
            brightblack: "bright black" => Color::BrightBlack,
            brightred: "bright red" => Color::BrightRed,
            brightgreen: "bright green" => Color::BrightGreen,
            brightyellow: "bright yellow" => Color::BrightYellow,
            brightblue: "bright blue" => Color::BrightBlue,
            brightmagenta: "bright magenta" => Color::BrightMagenta,
            brightcyan: "bright cyan" => Color::BrightCyan,
            brightwhite: "bright white" => Color::BrightWhite,

            invalid: "invalid" => Color::White,
            capitalized: "BLUE" => Color::Blue,
            mixed_case: "bLuE" => Color::Blue
        );
    }

    mod fromstr {
        pub use super::*;

        #[test]
        fn parse() {
            let color: Result<Color, _> = "blue".parse();
            assert_eq!(Ok(Color::Blue), color);
        }

        #[test]
        fn error() {
            let color: Result<Color, ()> = "bloublou".parse();
            assert_eq!(Err(()), color);
        }
    }

    // TODO
    // tests belown are disabled because they do not support bright colors

    // mod closest_euclidean {
    //     use super::*;

    //     macro_rules! make_euclidean_distance_test {
    //         ( $test:ident : ( $r:literal, $g: literal, $b:literal ), $expected:expr ) => {
    //             #[test]
    //             fn $test() {
    //                 let true_color = Color::TrueColor {
    //                     r: $r,
    //                     g: $g,
    //                     b: $b,
    //                 };
    //                 let actual = true_color.truecolor_fallback_to_ansi();
    //                 assert_eq!(actual, $expected);
    //             }
    //         };
    //     }

    //     make_euclidean_distance_test! { exact_black: (0, 0, 0), Color::Black }
    //     make_euclidean_distance_test! { exact_red: (205, 0, 0), Color::Red }
    //     make_euclidean_distance_test! { exact_green: (0, 205, 0), Color::Green }
    //     make_euclidean_distance_test! { exact_yellow: (205, 205, 0), Color::Yellow }
    //     make_euclidean_distance_test! { exact_blue: (0, 0, 238), Color::Blue }
    //     make_euclidean_distance_test! { exact_magenta: (205, 0, 205), Color::Magenta }
    //     make_euclidean_distance_test! { exact_cyan: (0, 205, 205), Color::Cyan }
    //     make_euclidean_distance_test! { exact_white: (229, 229, 229), Color::White }

    //     make_euclidean_distance_test! { almost_black: (10, 15, 10), Color::Black }
    //     make_euclidean_distance_test! { almost_red: (215, 10, 10), Color::Red }
    //     make_euclidean_distance_test! { almost_green: (10, 195, 10), Color::Green }
    //     make_euclidean_distance_test! { almost_yellow: (195, 215, 10), Color::Yellow }
    //     make_euclidean_distance_test! { almost_blue: (0, 0, 200), Color::Blue }
    //     make_euclidean_distance_test! { almost_magenta: (215, 0, 195), Color::Magenta }
    //     make_euclidean_distance_test! { almost_cyan: (10, 215, 215), Color::Cyan }
    //     make_euclidean_distance_test! { almost_white: (209, 209, 229), Color::White }
    // }
}

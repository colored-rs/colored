const CLEARV: u8 = 0b0000_0000;
const BOLD: u8 = 0b0000_0001;
const UNDERLINE: u8 = 0b0000_0010;
const REVERSED: u8 = 0b0000_0100;
const ITALIC: u8 = 0b0000_1000;
const BLINK: u8 = 0b0001_0000;
const HIDDEN: u8 = 0b0010_0000;
const DIMMED: u8 = 0b0100_0000;
const STRIKETHROUGH: u8 = 0b1000_0000;

static STYLES: [(u8, Styles); 8] = [
    (BOLD, Styles::Bold),
    (DIMMED, Styles::Dimmed),
    (UNDERLINE, Styles::Underline),
    (REVERSED, Styles::Reversed),
    (ITALIC, Styles::Italic),
    (BLINK, Styles::Blink),
    (HIDDEN, Styles::Hidden),
    (STRIKETHROUGH, Styles::Strikethrough),
];

pub static CLEAR: Style = Style(CLEARV);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Style(u8);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Styles {
    Clear,
    Bold,
    Dimmed,
    Underline,
    Reversed,
    Italic,
    Blink,
    Hidden,
    Strikethrough,
}

impl Styles {
    fn to_str<'a>(self) -> &'a str {
        match self {
            Styles::Clear => "", // unreachable, but we don't want to panic
            Styles::Bold => "1",
            Styles::Dimmed => "2",
            Styles::Italic => "3",
            Styles::Underline => "4",
            Styles::Blink => "5",
            Styles::Reversed => "7",
            Styles::Hidden => "8",
            Styles::Strikethrough => "9",
        }
    }

    fn to_u8(self) -> u8 {
        match self {
            Styles::Clear => CLEARV,
            Styles::Bold => BOLD,
            Styles::Dimmed => DIMMED,
            Styles::Italic => ITALIC,
            Styles::Underline => UNDERLINE,
            Styles::Blink => BLINK,
            Styles::Reversed => REVERSED,
            Styles::Hidden => HIDDEN,
            Styles::Strikethrough => STRIKETHROUGH,
        }
    }

    fn from_u8(u: u8) -> Option<Vec<Styles>> {
        if u == CLEARV {
            return None;
        }

        let res: Vec<Styles> = STYLES
            .into_iter()
            .filter(|&&(ref mask, _)| (0 != (u & mask)))
            .map(|&(_, value)| value)
            .collect();
        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }
}

impl Style {
    pub fn to_str(self) -> String {
        let styles = match Styles::from_u8(self.0) {
            None => return String::new(),
            Some(s) => s,
        };
        let mut res = String::new();
        let mut first = true;

        for style in styles.iter().map(|s| s.to_str()) {
            if first {
                res.push_str(style);
                first = false;
                continue;
            } else {
                res.push(';');
                res.push_str(style)
            }
        }
        res
    }

    pub fn new(from: Styles) -> Style {
        Style(from.to_u8())
    }

    pub fn from_both(one: Style, two: Styles) -> Style {
        Style(one.0 | two.to_u8())
    }
}

#[cfg(test)]
mod tests {

    mod u8_to_styles_invalid_is_none {
        use super::super::CLEARV;
        use super::super::{Style, Styles};

        #[test]
        fn empty_is_none() {
            assert_eq!(None, Styles::from_u8(CLEARV))
        }

    }

    mod u8_to_styles_isomorphism {
        use super::super::Styles;
        use super::super::{
            BLINK, BOLD, DIMMED, HIDDEN, ITALIC, REVERSED, STRIKETHROUGH, UNDERLINE,
        };

        macro_rules! value_isomorph {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    let u = Styles::from_u8($value);
                    assert!(
                        u.is_some(),
                        "{}: Styles::from_u8 -> None",
                        stringify!($value)
                    );
                    let u = u.unwrap();
                    assert!(
                        u.len() == 1,
                        "{}: Styles::from_u8 found {} styles (expected 1)",
                        stringify!($value),
                        u.len()
                    );
                    assert!(
                        u[0].to_u8() == $value,
                        "{}: to_u8() doesn't match its const value",
                        stringify!($value)
                    );
                }
            };
        }

        value_isomorph!(bold, BOLD);
        value_isomorph!(underline, UNDERLINE);
        value_isomorph!(reversed, REVERSED);
        value_isomorph!(italic, ITALIC);
        value_isomorph!(blink, BLINK);
        value_isomorph!(hidden, HIDDEN);
        value_isomorph!(dimmed, DIMMED);
        value_isomorph!(strikethrough, STRIKETHROUGH);
    }

    mod styles_combine_complex {
        use super::super::Styles::*;
        use super::super::{Style, Styles};
        use super::super::{
            BLINK, BOLD, DIMMED, HIDDEN, ITALIC, REVERSED, STRIKETHROUGH, UNDERLINE,
        };

        fn style_from_multiples(styles: &[Styles]) -> Style {
            let mut res = Style::new(styles[0]);
            for s in &styles[1..] {
                res = Style::from_both(res, *s)
            }
            res
        }

        macro_rules! test_aggreg {
            ($styles:expr, $expect:expr) => {{
                let v = style_from_multiples($styles);
                let r = Styles::from_u8(v.0).expect("should find styles");
                assert_eq!(&$expect as &[Styles], &r[..])
            }};
        }

        #[test]
        fn aggreg1() {
            let styles: &[Styles] = &[Bold, Bold, Bold];
            test_aggreg!(styles, [Bold])
        }

        #[test]
        fn aggreg2() {
            let styles: &[Styles] = &[Italic, Italic, Bold, Bold];
            test_aggreg!(styles, [Bold, Italic])
        }

        #[test]
        fn aggreg3() {
            let styles: &[Styles] = &[Bold, Italic, Bold];
            test_aggreg!(styles, [Bold, Italic])
        }

        macro_rules! test_combine {
            ($styles:expr) => {{
                let v = style_from_multiples($styles);
                let r = Styles::from_u8(v.0).expect("should find styles");
                assert_eq!($styles, &r[..])
            }};
        }

        #[test]
        fn two1() {
            let s: &[Styles] = &[Bold, Underline];
            test_combine!(s)
        }

        #[test]
        fn two2() {
            let s: &[Styles] = &[Underline, Italic];
            test_combine!(s)
        }

        #[test]
        fn two3() {
            let s: &[Styles] = &[Bold, Italic];
            test_combine!(s)
        }

        #[test]
        fn three1() {
            let s: &[Styles] = &[Bold, Underline, Italic];
            test_combine!(s)
        }

        #[test]
        fn three2() {
            let s: &[Styles] = &[Dimmed, Underline, Italic];
            test_combine!(s)
        }

        #[test]
        fn four() {
            let s: &[Styles] = &[Dimmed, Underline, Italic, Hidden];
            test_combine!(s)
        }

        #[test]
        fn five() {
            let s: &[Styles] = &[Dimmed, Underline, Italic, Blink, Hidden];
            test_combine!(s)
        }

        #[test]
        fn six() {
            let s: &[Styles] = &[Bold, Dimmed, Underline, Italic, Blink, Hidden];
            test_combine!(s)
        }

        #[test]
        fn all() {
            let s: &[Styles] = &[
                Bold,
                Dimmed,
                Underline,
                Reversed,
                Italic,
                Blink,
                Hidden,
                Strikethrough,
            ];
            test_combine!(s)
        }

    }
}

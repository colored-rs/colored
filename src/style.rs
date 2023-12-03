use core::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

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

/// A combinatorial style such as bold, italics, dimmed, etc.
///
/// # Implementation of Default
///
/// `Style::default()` returns a `Style` with no style switches
/// activated. Also consider using the [`CLEAR`] constant which
/// is 100% Equivalent.
///
/// # Implementation of logical bitwise operators
///
/// `Style` implements bitwise logical operations that operate on
/// the held style switches collectively. By far the most common
/// and useful is the bitwise 'or' operator `|` which combines two
/// styles, merging their combined styles into one. Example:
///
/// ```rust
/// # use colored::*;
/// let only_bold = Style::from(Styles::Bold);
/// let underline_and_italic =
///     Style::from(Styles::Underline) | Style::from(Styles::Italic);
/// let all_three = only_bold | underline_and_italic;
/// assert!(all_three.contains(Styles::Bold)
///     && all_three.contains(Styles::Underline)
///     && all_three.contains(Styles::Italic));
/// ```
///
/// This functionality also allows for easily turning off styles
/// of one `Styles` using another by combining the `&` and `!`
/// operators.
///
/// ```rust
/// # use colored::*;
/// let mut very_loud_style = Style::default();
/// for style in [
///     Styles::Bold,
///     Styles::Underline,
///     Styles::Italic,
///     Styles::Strikethrough,
///     Styles::Hidden,
/// ] {
///     very_loud_style.add(style);
/// }
/// // Oops! Some of those should not be in there!
/// // This Style now has all styles _except_ the two we don't want
/// // (hidden and strikethough).
/// let remove_mask =
///     !(Style::from(Styles::Hidden) | Style::from(Styles::Strikethrough));
/// very_loud_style &= remove_mask;
/// // `very_loud_style` no longer contains the undesired style
/// // switches...
/// assert!(!very_loud_style.contains(Styles::Hidden)
///     && !very_loud_style.contains(Styles::Strikethrough));
/// // ...but it retains everything else!
/// assert!(very_loud_style.contains(Styles::Bold));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Style(u8);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[allow(missing_docs)]
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
            .iter()
            .filter(|&(mask, _)| (0 != (u & mask)))
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
    /// Check if the current style has one of [`Styles`](Styles) switched on.
    ///
    /// ```rust
    /// # use colored::*;
    /// let colored = "".bold().italic();
    /// assert_eq!(colored.style().contains(Styles::Bold), true);
    /// assert_eq!(colored.style().contains(Styles::Italic), true);
    /// assert_eq!(colored.style().contains(Styles::Dimmed), false);
    /// ```
    pub fn contains(self, style: Styles) -> bool {
        let s = style.to_u8();
        self.0 & s == s
    }

    pub(crate) fn to_str(self) -> String {
        let styles = Styles::from_u8(self.0).unwrap_or_default();
        styles
            .iter()
            .map(|s| s.to_str())
            .collect::<Vec<&str>>()
            .join(";")
    }

    /// Adds the `two` style switch to this Style.
    ///
    /// ```rust
    /// # use colored::*;
    /// let cstr = "".red().bold();
    /// let mut template = StyleTemplate::default();
    /// template.copy_styling(&cstr);
    /// template.style.add(Styles::Italic);
    /// let mut cstr2 = "".blue();
    /// cstr2.copy_styling(&template);
    /// assert!(cstr2.styling().contains(Styles::Bold));
    /// assert!(cstr2.styling().contains(Styles::Italic));
    /// ```
    pub fn add(&mut self, two: Styles) {
        self.0 |= two.to_u8();
    }

    /// Turns off a style switch.
    ///
    /// ```rust
    /// use colored::*;
    /// let cstr = "".red().bold().italic();
    /// let mut template = StyleTemplate::default();
    /// template.copy_styling(&cstr);
    /// template.style.remove(Styles::Italic);
    /// let mut cstr2 = "".blue();
    /// cstr2.copy_styling(&template);
    /// assert!(cstr2.styling().contains(Styles::Bold));
    /// assert!(!cstr2.styling().contains(Styles::Italic));
    /// ```
    pub fn remove(&mut self, two: Styles) {
        self.0 &= !two.to_u8();
    }
}

macro_rules! auto_impl_ref_binop_trait {
    (impl $trait_name:ident, $method:ident for $t:ty) => {
        impl $trait_name<&$t> for $t {
            type Output = <$t as $trait_name<$t>>::Output;

            #[inline]
            fn $method(self, rhs: &$t) -> Self::Output {
                $trait_name::$method(self, *rhs)
            }
        }

        impl $trait_name<$t> for &$t {
            type Output = <$t as $trait_name<$t>>::Output;

            #[inline]
            fn $method(self, rhs: $t) -> Self::Output {
                $trait_name::$method(*self, rhs)
            }
        }

        impl $trait_name<&$t> for &$t {
            type Output = <$t as $trait_name<$t>>::Output;

            #[inline]
            fn $method(self, rhs: &$t) -> Self::Output {
                $trait_name::$method(*self, *rhs)
            }
        }
    };
}

impl BitAnd<Style> for Style {
    type Output = Style;

    fn bitand(self, rhs: Style) -> Self::Output {
        Style(self.0 & rhs.0)
    }
}

auto_impl_ref_binop_trait!(impl BitAnd, bitand for Style);

impl BitOr<Style> for Style {
    type Output = Style;

    fn bitor(self, rhs: Style) -> Self::Output {
        Style(self.0 | rhs.0)
    }
}

auto_impl_ref_binop_trait!(impl BitOr, bitor for Style);

impl BitXor<Style> for Style {
    type Output = Style;

    fn bitxor(self, rhs: Style) -> Self::Output {
        Style(self.0 ^ rhs.0)
    }
}

auto_impl_ref_binop_trait!(impl BitXor, bitxor for Style);

impl Not for Style {
    type Output = Style;

    fn not(self) -> Self::Output {
        Style(!self.0)
    }
}

impl Not for &Style {
    type Output = Style;

    fn not(self) -> Self::Output {
        Style(!self.0)
    }
}

macro_rules! impl_assign_op_trait {
    (
        $trait:ident, $method:ident for $t:ty, using $used_trait:ident::$used_method:ident
    ) => {
        impl $trait<$t> for $t {
            #[inline]
            fn $method(&mut self, other: $t) {
                *self = $used_trait::$used_method(&*self, other);
            }
        }

        impl $trait<&$t> for $t {
            #[inline]
            fn $method(&mut self, other: &$t) {
                *self = $used_trait::$used_method(&*self, other);
            }
        }
    };
}

impl_assign_op_trait!(BitAndAssign, bitand_assign for Style, using BitAnd::bitand);

impl_assign_op_trait!(BitOrAssign, bitor_assign for Style, using BitOr::bitor);

impl_assign_op_trait!(BitXorAssign, bitxor_assign for Style, using BitXor::bitxor);

impl Default for Style {
    fn default() -> Self {
        CLEAR
    }
}

impl From<Styles> for Style {
    fn from(value: Styles) -> Self {
        Style(value.to_u8())
    }
}

impl From<&Styles> for Style {
    fn from(value: &Styles) -> Self {
        Style(value.to_u8())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod u8_to_styles_invalid_is_none {
        use super::super::Styles;
        use super::super::CLEARV;

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

        fn style_from_multiples(styles: &[Styles]) -> Style {
            let mut res = Style(styles[0].to_u8());
            for s in &styles[1..] {
                res = Style(res.0 | s.to_u8());
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

    #[test]
    fn test_style_contains() {
        let mut style = Style(Styles::Bold.to_u8());
        style.add(Styles::Italic);

        assert_eq!(style.contains(Styles::Bold), true);
        assert_eq!(style.contains(Styles::Italic), true);
        assert_eq!(style.contains(Styles::Dimmed), false);
    }

    #[test]
    fn style_binops() {
        for (l, r) in [
            // BitAnd (&)
            (Style(BOLD) & Style(UNDERLINE), Style(CLEARV)),
            // Check impls for refs work
            (&Style(BOLD) & Style(BOLD), Style(BOLD)),
            (Style(CLEARV) & Style(CLEARV), Style(CLEARV)),
            // Full truth table for bits
            (&Style(0b0011) & &Style(0b0101), Style(0b0001)),
            // BitOr (|)
            (Style(BOLD) | Style(UNDERLINE), Style(BOLD | UNDERLINE)),
            (&Style(BOLD) | Style(BOLD), Style(BOLD)),
            (Style(CLEARV) | &Style(UNDERLINE), Style(UNDERLINE)),
            (&Style(0b0011) | &Style(0b0101), Style(0b0111)),
            // BitXor (^)
            (Style(BOLD) ^ Style(CLEARV), Style(BOLD)),
            (&Style(BOLD) ^ Style(UNDERLINE), Style(BOLD | UNDERLINE)),
            (Style(BOLD) ^ &Style(BOLD), Style(CLEARV)),
            (Style(0b0011) ^ Style(0b0101), Style(0b0110)),
        ] {
            assert_eq!(l, r);
        }
    }

    #[test]
    fn style_unops() {
        let not_bold = !Style(BOLD);
        assert!(!not_bold.contains(Styles::Bold));
        assert!(not_bold.contains(Styles::Strikethrough));
        assert_eq!(!Style(0b0011_0101), Style(0b1100_1010));
    }

    #[test]
    fn style_bitwise_assign_ops() {
        let origional_style = Style(0b0011);
        let op_style = Style(0b0101);

        let mut style = origional_style;
        style &= op_style;
        assert_eq!(style, Style(0b0001));

        style = origional_style;
        style |= op_style;
        assert_eq!(style, Style(0b0111));

        style = origional_style;
        style ^= op_style;
        assert_eq!(style, Style(0b0110));
    }
}

//!Coloring terminal so simple, you already know how to do it !
//!
//!    use colored::Colorize;
//!
//!    "this is blue".blue();
//!    "this is red".red();
//!    "this is red on blue".red().on_blue();
//!    "this is also red on blue".on_blue().red();
//!    "you can use truecolor values too!".truecolor(0, 255, 136);
//!    "background truecolor also works :)".on_truecolor(135, 28, 167);
//!    "you can also make bold comments".bold();
//!    println!("{} {} {}", "or use".cyan(), "any".italic().yellow(), "string type".cyan());
//!    "or change advice. This is red".yellow().blue().red();
//!    "or clear things up. This is default color and style".red().bold().clear();
//!    "purple and magenta are the same".purple().magenta();
//!    "bright colors are also allowed".bright_blue().on_bright_white();
//!    "you can specify color by string".color("blue").on_color("red");
//!    "and so are normal and clear".normal().clear();
//!    String::from("this also works!").green().bold();
//!    format!("{:30}", "format works as expected. This will be padded".blue());
//!    format!("{:.3}", "and this will be green but truncated to 3 chars".green());
//!
//!
//! See [the `Colorize` trait](./trait.Colorize.html) for all the methods.
//!
//! Note: The methods of [`Colorize`], when used on [`str`]'s, return
//! [`ColoredString`]'s. See [`ColoredString`] to learn more about them and
//! what you can do with them beyond continue to use [`Colorize`] to further
//! modify them.
#![warn(missing_docs)]

#[cfg(test)]
extern crate rspec;

mod color;
pub mod control;
mod error;
mod style;

pub use self::customcolors::CustomColor;

/// Custom colors support.
pub mod customcolors;

pub use color::*;

use core::fmt::{Display, Write};
use std::{
    error::Error,
    fmt::{self},
    ops::{Deref, DerefMut},
};

pub use style::{Style, Styles};

/// A string that may have color and/or style applied to it.
///
/// Commonly created via calling the methods of [`Colorize`] on a &str.
/// All methods of [`Colorize`] either create a new `ColoredString` from
/// the type called on or modify a callee `ColoredString`. See
/// [`Colorize`] for more.
///
/// The primary usage of `ColoredString`'s is as a way to take text,
/// apply colors and miscillaneous styling to it (such as bold or
/// underline), and then use it to create formatted strings that print
/// to the console with the special styling applied.
///
/// ## Usage
///
/// As stated, `ColoredString`'s, once created, can be printed to the
/// console with their colors and style or turned into a string
/// containing special console codes that has the same effect.
/// This is made easy via `ColoredString`'s implementations of
/// [`Display`](std::fmt::Display) and [`ToString`] for those purposes
/// respectively.
///
/// Printing a `ColoredString` with its style is as easy as:
///
/// ```
/// # use colored::*;
/// let cstring: ColoredString = "Bold and Red!".bold().red();
/// println!("{}", cstring);
/// ```
///
/// ## Manipulating the coloring/style of a `ColoredString`
///
/// Getting or changing the foreground color, background color, and or
/// style of a `ColoredString` is as easy as manually reading / modifying
/// the fields of `ColoredString`.
///
/// ```
/// # use colored::*;
/// let mut red_text = "Red".red();
/// // Changing color using re-assignment and [`Colorize`]:
/// red_text = red_text.blue();
/// // Manipulating fields of `ColoredString` in-place:
/// red_text.fgcolor = Some(Color::Blue);
///
/// let styled_text1 = "Bold".bold();
/// let styled_text2 = "Italic".italic();
/// let mut styled_text3 = ColoredString::from("Bold and Italic");
/// styled_text3.style = styled_text1.style | styled_text2.style;
/// ```
///
/// ## Modifying the text of a `ColoredString`
///
/// Modifying the text is as easy as modifying the `input` field of
/// `ColoredString`...
///
/// ```
/// # use colored::*;
/// let mut colored_text = "Magenta".magenta();
/// colored_text = colored_text.blue();
/// colored_text.input = "Blue".to_string();
/// // Note: The above is inefficient and `colored_text.input.replace_range(.., "Blue")` would
/// // be more proper. This is just for example.
///
/// assert_eq!(&*colored_text, "Blue");
/// ```
///
/// Notice how this process preserves the coloring and style.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct ColoredString {
    /// The plain text that will have color and style applied to it.
    pub input: String,
    /// The color of the text as it will be printed.
    pub fgcolor: Option<Color>,
    /// The background color (if any). None means that the text will be printed
    /// without a special background.
    pub bgcolor: Option<Color>,
    /// Any special styling to be applied to the text (see Styles for a list of
    /// available options).
    pub style: style::Style,
}

/// The trait that enables something to be given color.
///
/// You can use `colored` effectively simply by importing this trait
/// and then using its methods on `String` and `&str`.
#[allow(missing_docs)]
pub trait Colorize {
    // Font Colors
    fn black(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Black)
    }
    fn red(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Red)
    }
    fn green(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Green)
    }
    fn yellow(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Yellow)
    }
    fn blue(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Blue)
    }
    fn magenta(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Magenta)
    }
    fn purple(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Magenta)
    }
    fn cyan(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Cyan)
    }
    fn white(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::White)
    }
    fn bright_black(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightBlack)
    }
    fn bright_red(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightRed)
    }
    fn bright_green(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightGreen)
    }
    fn bright_yellow(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightYellow)
    }
    fn bright_blue(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightBlue)
    }
    fn bright_magenta(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightMagenta)
    }
    fn bright_purple(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightMagenta)
    }
    fn bright_cyan(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightCyan)
    }
    fn bright_white(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::BrightWhite)
    }
    fn truecolor(self, r: u8, g: u8, b: u8) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::TrueColor { r, g, b })
    }
    fn custom_color<T>(self, color: T) -> ColoredString
    where
        Self: Sized,
        T: Into<CustomColor>,
    {
        let color = color.into();

        self.color(Color::TrueColor {
            r: color.r,
            g: color.g,
            b: color.b,
        })
    }
    fn color<S: Into<Color>>(self, color: S) -> ColoredString;
    // Background Colors
    fn on_black(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Black)
    }
    fn on_red(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Red)
    }
    fn on_green(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Green)
    }
    fn on_yellow(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Yellow)
    }
    fn on_blue(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Blue)
    }
    fn on_magenta(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Magenta)
    }
    fn on_purple(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Magenta)
    }
    fn on_cyan(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::Cyan)
    }
    fn on_white(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::White)
    }
    fn on_bright_black(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightBlack)
    }
    fn on_bright_red(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightRed)
    }
    fn on_bright_green(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightGreen)
    }
    fn on_bright_yellow(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightYellow)
    }
    fn on_bright_blue(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightBlue)
    }
    fn on_bright_magenta(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightMagenta)
    }
    fn on_bright_purple(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightMagenta)
    }
    fn on_bright_cyan(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightCyan)
    }
    fn on_bright_white(self) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::BrightWhite)
    }
    fn on_truecolor(self, r: u8, g: u8, b: u8) -> ColoredString
    where
        Self: Sized,
    {
        self.on_color(Color::TrueColor { r, g, b })
    }
    fn on_custom_color<T>(self, color: T) -> ColoredString
    where
        Self: Sized,
        T: Into<CustomColor>,
    {
        let color = color.into();

        self.on_color(Color::TrueColor {
            r: color.r,
            g: color.g,
            b: color.b,
        })
    }
    fn on_color<S: Into<Color>>(self, color: S) -> ColoredString;
    // Styles
    fn clear(self) -> ColoredString;
    fn normal(self) -> ColoredString;
    fn bold(self) -> ColoredString;
    fn dimmed(self) -> ColoredString;
    fn italic(self) -> ColoredString;
    fn underline(self) -> ColoredString;
    fn blink(self) -> ColoredString;
    #[deprecated(since = "1.5.2", note = "Users should use reversed instead")]
    fn reverse(self) -> ColoredString;
    fn reversed(self) -> ColoredString;
    fn hidden(self) -> ColoredString;
    fn strikethrough(self) -> ColoredString;
}

impl ColoredString {
    /// Get the current background color applied.
    ///
    /// ```rust
    /// # use colored::*;
    /// let cstr = "".blue();
    /// assert_eq!(cstr.fgcolor(), Some(Color::Blue));
    /// let cstr = cstr.clear();
    /// assert_eq!(cstr.fgcolor(), None);
    /// ```
    #[deprecated(note = "Deprecated due to the exposing of the fgcolor struct field.")]
    #[must_use]
    pub fn fgcolor(&self) -> Option<Color> {
        self.fgcolor.as_ref().copied()
    }

    /// Get the current background color applied.
    ///
    /// ```rust
    /// # use colored::*;
    /// let cstr = "".on_blue();
    /// assert_eq!(cstr.bgcolor(), Some(Color::Blue));
    /// let cstr = cstr.clear();
    /// assert_eq!(cstr.bgcolor(), None);
    /// ```
    #[deprecated(note = "Deprecated due to the exposing of the bgcolor struct field.")]
    #[must_use]
    pub fn bgcolor(&self) -> Option<Color> {
        self.bgcolor.as_ref().copied()
    }

    /// Get the current [`Style`] which can be check if it contains a [`Styles`].
    ///
    /// ```rust
    /// # use colored::*;
    /// let colored = "".bold().italic();
    /// assert_eq!(colored.style().contains(Styles::Bold), true);
    /// assert_eq!(colored.style().contains(Styles::Italic), true);
    /// assert_eq!(colored.style().contains(Styles::Dimmed), false);
    /// ```
    #[deprecated(note = "Deprecated due to the exposing of the style struct field.")]
    #[must_use]
    pub fn style(&self) -> style::Style {
        self.style
    }

    /// Clears foreground coloring on this `ColoredString`, meaning that it
    /// will be printed with the default terminal text color.
    pub fn clear_fgcolor(&mut self) {
        self.fgcolor = None;
    }

    /// Gets rid of this `ColoredString`'s background.
    pub fn clear_bgcolor(&mut self) {
        self.bgcolor = None;
    }

    /// Clears any special styling and sets it back to the default (plain,
    /// maybe colored, text).
    pub fn clear_style(&mut self) {
        self.style = Style::default();
    }

    /// Checks if the colored string has no color or styling.
    ///
    /// ```rust
    /// # use colored::*;
    /// let cstr = "".red();
    /// assert_eq!(cstr.is_plain(), false);
    /// let cstr = cstr.clear();
    /// assert_eq!(cstr.is_plain(), true);
    /// ```
    #[must_use]
    pub fn is_plain(&self) -> bool {
        self.bgcolor.is_none() && self.fgcolor.is_none() && self.style == style::CLEAR
    }

    #[cfg(not(feature = "no-color"))]
    fn has_colors() -> bool {
        control::SHOULD_COLORIZE.should_colorize()
    }

    #[cfg(feature = "no-color")]
    fn has_colors() -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy)]
struct EscapeInnerResetSequencesHelper<'a> {
    input: &'a str,
    fgcolor: Option<Color>,
    bgcolor: Option<Color>,
    style: style::Style,
    is_plain: bool,
}

impl<'a> EscapeInnerResetSequencesHelper<'a> {
    fn new(input: &'a str, color: &ColoredString) -> Self {
        Self {
            input,
            fgcolor: color.fgcolor,
            bgcolor: color.bgcolor,
            style: color.style,
            is_plain: color.is_plain(),
        }
    }
}

impl Display for EscapeInnerResetSequencesHelper<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const RESET: &str = "\x1B[0m";

        if !ColoredString::has_colors() || self.is_plain {
            return f.write_str(self.input);
        }
        let mut matches = self
            .input
            .match_indices(RESET)
            .map(|(idx, _)| idx + RESET.len())
            .peekable();
        if matches.peek().is_none() {
            return f.write_str(self.input);
        }

        let mut start = 0;
        for offset in matches {
            // Write the text up to the end reset sequence
            f.write_str(&self.input[start..offset])?;
            // Remember where the next text starts
            start = offset;
            // Write style
            ComputeStyleHelper::from(self).fmt(f)?;
        }
        // Write rest
        f.write_str(&self.input[start..])?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct ComputeStyleHelper {
    fgcolor: Option<Color>,
    bgcolor: Option<Color>,
    style: style::Style,
    is_plain: bool,
}

impl From<&ColoredString> for ComputeStyleHelper {
    fn from(value: &ColoredString) -> Self {
        Self::new(value)
    }
}

impl From<&EscapeInnerResetSequencesHelper<'_>> for ComputeStyleHelper {
    fn from(value: &EscapeInnerResetSequencesHelper) -> Self {
        Self {
            fgcolor: value.fgcolor,
            bgcolor: value.bgcolor,
            style: value.style,
            is_plain: value.is_plain,
        }
    }
}

impl ComputeStyleHelper {
    fn new(color: &ColoredString) -> Self {
        Self {
            fgcolor: color.fgcolor,
            bgcolor: color.bgcolor,
            style: color.style,
            is_plain: color.is_plain(),
        }
    }
}

impl Display for ComputeStyleHelper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !ColoredString::has_colors() || self.is_plain {
            return Ok(());
        }
        f.write_str("\x1B[")?;
        let mut has_wrote = if self.style == style::CLEAR {
            false
        } else {
            self.style.private_fmt(f)?;
            true
        };

        if let Some(ref bgcolor) = self.bgcolor {
            if has_wrote {
                f.write_char(';')?;
            }
            bgcolor.to_bg_fmt(f)?;
            has_wrote = true;
        }

        if let Some(ref fgcolor) = self.fgcolor {
            if has_wrote {
                f.write_char(';')?;
            }
            fgcolor.to_fg_fmt(f)?;
        }

        f.write_char('m')?;
        Ok(())
    }
}

impl Deref for ColoredString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl DerefMut for ColoredString {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.input
    }
}

impl From<String> for ColoredString {
    fn from(s: String) -> Self {
        Self {
            input: s,
            ..Self::default()
        }
    }
}

impl<'a> From<&'a str> for ColoredString {
    fn from(s: &'a str) -> Self {
        Self {
            input: String::from(s),
            ..Self::default()
        }
    }
}

impl Colorize for ColoredString {
    fn color<S: Into<Color>>(mut self, color: S) -> ColoredString {
        self.fgcolor = Some(color.into());
        self
    }
    fn on_color<S: Into<Color>>(mut self, color: S) -> ColoredString {
        self.bgcolor = Some(color.into());
        self
    }

    fn clear(self) -> ColoredString {
        Self {
            input: self.input,
            ..Self::default()
        }
    }
    fn normal(self) -> ColoredString {
        self.clear()
    }
    fn bold(mut self) -> ColoredString {
        self.style.add(style::Styles::Bold);
        self
    }
    fn dimmed(mut self) -> ColoredString {
        self.style.add(style::Styles::Dimmed);
        self
    }
    fn italic(mut self) -> ColoredString {
        self.style.add(style::Styles::Italic);
        self
    }
    fn underline(mut self) -> ColoredString {
        self.style.add(style::Styles::Underline);
        self
    }
    fn blink(mut self) -> ColoredString {
        self.style.add(style::Styles::Blink);
        self
    }
    fn reverse(self) -> ColoredString {
        self.reversed()
    }
    fn reversed(mut self) -> ColoredString {
        self.style.add(style::Styles::Reversed);
        self
    }
    fn hidden(mut self) -> ColoredString {
        self.style.add(style::Styles::Hidden);
        self
    }
    fn strikethrough(mut self) -> ColoredString {
        self.style.add(style::Styles::Strikethrough);
        self
    }
}

impl Colorize for &str {
    fn color<S: Into<Color>>(self, color: S) -> ColoredString {
        ColoredString {
            fgcolor: Some(color.into()),
            input: String::from(self),
            ..ColoredString::default()
        }
    }

    fn on_color<S: Into<Color>>(self, color: S) -> ColoredString {
        ColoredString {
            bgcolor: Some(color.into()),
            input: String::from(self),
            ..ColoredString::default()
        }
    }

    fn clear(self) -> ColoredString {
        ColoredString {
            input: String::from(self),
            style: style::CLEAR,
            ..ColoredString::default()
        }
    }
    fn normal(self) -> ColoredString {
        self.clear()
    }
    fn bold(self) -> ColoredString {
        ColoredString::from(self).bold()
    }
    fn dimmed(self) -> ColoredString {
        ColoredString::from(self).dimmed()
    }
    fn italic(self) -> ColoredString {
        ColoredString::from(self).italic()
    }
    fn underline(self) -> ColoredString {
        ColoredString::from(self).underline()
    }
    fn blink(self) -> ColoredString {
        ColoredString::from(self).blink()
    }
    fn reverse(self) -> ColoredString {
        self.reversed()
    }
    fn reversed(self) -> ColoredString {
        ColoredString::from(self).reversed()
    }
    fn hidden(self) -> ColoredString {
        ColoredString::from(self).hidden()
    }
    fn strikethrough(self) -> ColoredString {
        ColoredString::from(self).strikethrough()
    }
}

impl fmt::Display for ColoredString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !Self::has_colors() || self.is_plain() {
            return <String as fmt::Display>::fmt(&self.input, f);
        }

        let mut input = &*self.input;
        let mut padding = 0;

        // Calculate padding and input
        if let Some(precision) = f.precision() {
            let mut iter = input.char_indices();

            // FIXME
            // feature(iter_advance_by) is not stable
            // CharIndeces::offset is MSRV 1.82.0
            // the std impl uses this methods in it is a lot nicer.
            let mut count = 0;
            let mut offset = 0;
            for _ in 0..precision {
                if let Some((i, c)) = iter.next() {
                    offset = i + c.len_utf8();
                    count += 1;
                }
            }

            input = &input[..offset];
            if let Some(width) = f.width() {
                padding = width.saturating_sub(count);
            }
        } else if let Some(width) = f.width() {
            padding = width.saturating_sub(self.chars().take(width).count());
        }

        // Do the actual formatting
        // XXX: see tests. Useful when nesting colored strings
        ComputeStyleHelper::from(self).fmt(f)?;
        EscapeInnerResetSequencesHelper::new(input, self).fmt(f)?;
        f.write_str("\x1B[0m")?;

        // Add padding
        for _ in 0..padding {
            f.write_char(f.fill())?;
        }

        Ok(())
    }
}

impl From<ColoredString> for Box<dyn Error> {
    fn from(cs: ColoredString) -> Self {
        Box::from(error::ColoredStringError(cs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{error::Error, fmt::Write};

    /// Test the formatting
    ///
    /// Formatting *must* respect padding
    ///
    /// The added normal str in the input is for comparison to rust-std
    mod formmating {
        use super::*;

        #[test]
        fn empty_padding() {
            let inputs: &[&dyn Display] = &[&"".custom_color((126, 194, 218)), &"".blue(), &""];

            for input in inputs {
                assert_eq!(
                    format!("{input:40}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    40
                );
            }
        }
        #[test]
        fn no_padding() {
            let inputs: &[&dyn Display] = &[&"".custom_color((126, 194, 218)), &"".blue(), &""];
            for input in inputs {
                assert_eq!(
                    format!("{input}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0
                );
            }
        }

        #[test]
        fn not_enough_for_padding() {
            let inputs: &[&dyn Display] =
                &[&"🦀🦀".custom_color((126, 194, 218)), &"CS".blue(), &"CS"];
            for input in inputs {
                assert_eq!(
                    format!("{input:0}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0
                );
            }
            for input in inputs {
                assert_eq!(
                    format!("{input:1}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0
                );
            }
            for input in inputs {
                assert_eq!(
                    format!("{input:2}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0
                );
            }
        }

        #[test]
        fn padding_with_input() {
            let inputs: &[&dyn Display] =
                &[&"🦀🦀".custom_color((126, 194, 218)), &"CS".blue(), &"CS"];
            for input in inputs {
                assert_eq!(
                    format!("{input:3}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    1
                );
                assert!(format!("{input:3}").contains(&input.to_string()));

                assert_eq!(
                    format!("{input:4}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    2
                );
                assert!(format!("{input:4}").contains(&input.to_string()));
            }
        }

        #[test]
        fn precision_less() {
            let inputs: &[&dyn Display] = &[
                &"🦀🦀".custom_color((126, 194, 218)),
                &"CC".blue(),
                &"CC",
                &"ColoredString".blue(),
            ];
            for input in inputs {
                assert_eq!(
                    format!("{input:.1}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    1,
                    "input: {input}"
                );
                assert!(!format!("{input:.1}").contains(&input.to_string()));
            }
        }

        #[test]
        fn precision_eq() {
            let inputs: &[&dyn Display] =
                &[&"🦀🦀".custom_color((126, 194, 218)), &"CC".blue(), &"CC"];
            for input in inputs {
                assert_eq!(
                    format!("{input:.2}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert!(format!("{input:.2}").contains(&input.to_string()));
            }
        }

        #[test]
        fn precision_more() {
            let inputs: &[&dyn Display] =
                &[&"🦀🦀".custom_color((126, 194, 218)), &"CC".blue(), &"CC"];
            for input in inputs {
                assert_eq!(
                    format!("{input:.100}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert!(format!("{input:.100}").contains(&input.to_string()));
            }
        }

        #[test]
        fn precision_padding() {
            let inputs: &[&dyn Display] =
                &[&"🦀🦀".custom_color((126, 194, 218)), &"CC".blue(), &"CC"];
            for input in inputs {
                // precision less, padding more
                assert_eq!(
                    format!("{input:40.1}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    1,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:40.1}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    39,
                    "input: {input}"
                );
                // precision less, padding less_or_equal
                assert_eq!(
                    format!("{input:1.1}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    1,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:1.1}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:0.1}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    1,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:0.1}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert!(format!("{input:.100}").contains(&input.to_string()));
                // precision eq, padding more
                assert_eq!(
                    format!("{input:40.2}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:40.2}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    38,
                    "input: {input}"
                );
                // precision eq, padding less_or_equal
                assert_eq!(
                    format!("{input:1.2}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:1.2}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert!(format!("{input:1.2}").contains(&input.to_string()));
                assert_eq!(
                    format!("{input:0.2}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:0.2}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert!(format!("{input:0.2}").contains(&input.to_string()));

                // precision more, padding more
                assert_eq!(
                    format!("{input:40.4}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:40.4}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    38,
                    "input: {input}"
                );
                // precision eq, padding less_or_equal
                assert_eq!(
                    format!("{input:1.4}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:1.4}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert!(format!("{input:1.4}").contains(&input.to_string()));
                assert_eq!(
                    format!("{input:0.4}")
                        .chars()
                        .filter(|c| *c == '🦀' || *c == 'C')
                        .count(),
                    2,
                    "input: {input}"
                );
                assert_eq!(
                    format!("{input:0.4}")
                        .chars()
                        .filter(|c| c.is_whitespace())
                        .count(),
                    0,
                    "input: {input}"
                );
                assert!(format!("{input:0.4}").contains(&input.to_string()));
            }
        }
    }

    #[test]
    fn it_works() -> Result<(), Box<dyn Error>> {
        let mut buf = String::new();
        let toto = "toto";
        writeln!(&mut buf, "{}", toto.red())?;
        writeln!(&mut buf, "{}", String::from(toto).red())?;
        writeln!(&mut buf, "{}", toto.blue())?;

        writeln!(&mut buf, "blue style ****")?;
        writeln!(&mut buf, "{}", toto.bold())?;
        writeln!(&mut buf, "{}", "yeah ! Red bold !".red().bold())?;
        writeln!(&mut buf, "{}", "yeah ! Yellow bold !".bold().yellow())?;
        writeln!(&mut buf, "{}", toto.bold().blue())?;
        writeln!(&mut buf, "{}", toto.blue().bold())?;
        writeln!(&mut buf, "{}", toto.blue().bold().underline())?;
        writeln!(&mut buf, "{}", toto.blue().italic())?;
        writeln!(&mut buf, "******")?;
        writeln!(&mut buf, "test clearing")?;
        writeln!(&mut buf, "{}", "red cleared".red().clear())?;
        writeln!(&mut buf, "{}", "bold cyan cleared".bold().cyan().clear())?;
        writeln!(&mut buf, "******")?;
        writeln!(&mut buf, "Bg tests")?;
        writeln!(&mut buf, "{}", toto.green().on_blue())?;
        writeln!(&mut buf, "{}", toto.on_magenta().yellow())?;
        writeln!(&mut buf, "{}", toto.purple().on_yellow())?;
        writeln!(&mut buf, "{}", toto.magenta().on_white())?;
        writeln!(&mut buf, "{}", toto.cyan().on_green())?;
        writeln!(&mut buf, "{}", toto.black().on_white())?;
        writeln!(&mut buf, "******")?;
        writeln!(&mut buf, "{}", toto.green())?;
        writeln!(&mut buf, "{}", toto.yellow())?;
        writeln!(&mut buf, "{}", toto.purple())?;
        writeln!(&mut buf, "{}", toto.magenta())?;
        writeln!(&mut buf, "{}", toto.cyan())?;
        writeln!(&mut buf, "{}", toto.white())?;
        writeln!(&mut buf, "{}", toto.white().red().blue().green())?;
        writeln!(&mut buf, "{}", toto.truecolor(255, 0, 0))?;
        writeln!(&mut buf, "{}", toto.truecolor(255, 255, 0))?;
        writeln!(&mut buf, "{}", toto.on_truecolor(0, 80, 80))?;
        writeln!(&mut buf, "{}", toto.custom_color((255, 255, 0)))?;
        writeln!(&mut buf, "{}", toto.on_custom_color((0, 80, 80)))?;
        #[cfg(feature = "no-color")]
        insta::assert_snapshot!("it_works_no_color", buf);
        #[cfg(not(feature = "no-color"))]
        insta::assert_snapshot!("it_works", buf);
        Ok(())
    }

    mod compute_style {
        use crate::{ColoredString, Colorize, ComputeStyleHelper};

        /// Into is not dyn compatible
        trait IntoCS {
            fn into(&self) -> ColoredString;
        }
        impl IntoCS for &str {
            fn into(&self) -> ColoredString {
                ColoredString::from(*self)
            }
        }
        impl IntoCS for ColoredString {
            fn into(&self) -> ColoredString {
                self.clone()
            }
        }

        /// Helper to use a Colorize function on str or `ColoredString`
        type ColorMapper = &'static dyn Fn(&dyn IntoCS) -> ColoredString;
        macro_rules! helper_color {
            ($f:ident, $r:expr) => {
                (&(|i| Colorize::$f(i.into())), $r)
            };
        }
        /// Helper to use a truecolor method of Colorize on str or `ColoredString`
        macro_rules! helper_truecolor {
            ($f:ident, $r:expr, $g:expr,$b:expr,$res:expr) => {
                (
                    &(|i| Colorize::$f(i.into(), $r, $g, $b)),
                    concat!(
                        $res,
                        stringify!($r),
                        ";",
                        stringify!($g),
                        ";",
                        stringify!($b)
                    ),
                )
            };
        }
        /// The input of a `ColoredString`
        const INPUTS: &[&str; 5] = &["🦀🦀", "CS", "CC", "ColoredString", "🦀ColoredString🦀CC"];
        /// All background function mappings to their expected result
        const BG_MAPPINGS: &[(ColorMapper, &str)] = &[
            helper_color!(on_black, "40"),
            helper_color!(on_red, "41"),
            helper_color!(on_green, "42"),
            helper_color!(on_yellow, "43"),
            helper_color!(on_blue, "44"),
            helper_color!(on_magenta, "45"),
            helper_color!(on_cyan, "46"),
            helper_color!(on_white, "47"),
            helper_color!(on_bright_black, "100"),
            helper_color!(on_bright_red, "101"),
            helper_color!(on_bright_green, "102"),
            helper_color!(on_bright_yellow, "103"),
            helper_color!(on_bright_blue, "104"),
            helper_color!(on_bright_magenta, "105"),
            helper_color!(on_bright_cyan, "106"),
            helper_color!(on_bright_white, "107"),
            helper_truecolor!(on_truecolor, 0, 0, 0, "48;2;"),
            helper_truecolor!(on_truecolor, 128, 0, 0, "48;2;"),
            helper_truecolor!(on_truecolor, 0, 128, 0, "48;2;"),
            helper_truecolor!(on_truecolor, 0, 0, 128, "48;2;"),
            helper_truecolor!(on_truecolor, 128, 127, 0, "48;2;"),
            helper_truecolor!(on_truecolor, 128, 0, 127, "48;2;"),
            helper_truecolor!(on_truecolor, 0, 128, 127, "48;2;"),
            helper_truecolor!(on_truecolor, 126, 128, 127, "48;2;"),
        ];
        /// All foreground function mappings to their expected result
        const FG_MAPPINGS: &[(ColorMapper, &str)] = &[
            helper_color!(black, "30"),
            helper_color!(red, "31"),
            helper_color!(green, "32"),
            helper_color!(yellow, "33"),
            helper_color!(blue, "34"),
            helper_color!(magenta, "35"),
            helper_color!(cyan, "36"),
            helper_color!(white, "37"),
            helper_color!(bright_black, "90"),
            helper_color!(bright_red, "91"),
            helper_color!(bright_green, "92"),
            helper_color!(bright_yellow, "93"),
            helper_color!(bright_blue, "94"),
            helper_color!(bright_magenta, "95"),
            helper_color!(bright_cyan, "96"),
            helper_color!(bright_white, "97"),
            helper_truecolor!(truecolor, 0, 0, 0, "38;2;"),
            helper_truecolor!(truecolor, 128, 0, 0, "38;2;"),
            helper_truecolor!(truecolor, 0, 128, 0, "38;2;"),
            helper_truecolor!(truecolor, 0, 0, 128, "38;2;"),
            helper_truecolor!(truecolor, 128, 127, 0, "38;2;"),
            helper_truecolor!(truecolor, 128, 0, 127, "38;2;"),
            helper_truecolor!(truecolor, 0, 128, 127, "38;2;"),
            helper_truecolor!(truecolor, 126, 128, 127, "38;2;"),
        ];

        const STYLE_MAPPINGS: &[(ColorMapper, &str)] = &[
            helper_color!(bold, "1"),
            helper_color!(dimmed, "2"),
            helper_color!(italic, "3"),
            helper_color!(underline, "4"),
            helper_color!(blink, "5"),
            helper_color!(reversed, "7"),
            helper_color!(hidden, "8"),
            helper_color!(strikethrough, "9"),
        ];

        #[test]
        /// Check for clear
        fn clear() {
            for input in INPUTS {
                assert_eq!(&format!("{}", input.clear()), input);
            }
        }

        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        /// Check for only a set foreground color
        fn simple_fg() {
            for input in INPUTS {
                for (fun, res) in FG_MAPPINGS {
                    assert_eq!(
                        format!("{}", ComputeStyleHelper::new(&fun(input))),
                        format!("\x1B[{res}m"),
                        "Input: '{input}'"
                    );
                }
            }
        }

        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        /// Check for only a set background color
        fn simple_bg() {
            for input in INPUTS {
                for (fun, res) in BG_MAPPINGS {
                    assert_eq!(
                        format!("{}", ComputeStyleHelper::new(&fun(input))),
                        format!("\x1B[{res}m"),
                        "Input: '{input}'"
                    );
                }
            }
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        /// Check for only a set background AND foreground color
        fn fg_and_bg() {
            for input in INPUTS {
                for (fg_fun, fg_res) in FG_MAPPINGS {
                    for (bg_fun, bg_res) in BG_MAPPINGS {
                        let tmp = bg_fun(input);
                        assert_eq!(
                            format!("{}", ComputeStyleHelper::new(&fg_fun(&tmp))),
                            format!("\x1B[{bg_res};{fg_res}m"),
                            "Input: '{input}'"
                        );
                    }
                }
            }
        }

        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        fn simple_style() {
            for input in INPUTS {
                for (fun, res) in STYLE_MAPPINGS {
                    assert_eq!(
                        format!("{}", ComputeStyleHelper::new(&fun(input))),
                        format!("\x1B[{res}m"),
                        "Input: '{input}'"
                    );
                }
            }
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        #[ignore = "Not yet implemented"]
        fn multiple_style() {
            todo!()
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        fn style_fg() {
            for input in INPUTS {
                for (fg_fun, fg_res) in FG_MAPPINGS {
                    for (style_fun, style_res) in STYLE_MAPPINGS {
                        let tmp = style_fun(input);
                        assert_eq!(
                            format!("{}", ComputeStyleHelper::new(&fg_fun(&tmp))),
                            format!("\x1B[{style_res};{fg_res}m"),
                            "Input: '{input}'"
                        );
                    }
                }
            }
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        fn style_bg() {
            for input in INPUTS {
                for (bg_fun, bg_res) in BG_MAPPINGS {
                    for (style_fun, style_res) in STYLE_MAPPINGS {
                        let tmp = style_fun(input);
                        assert_eq!(
                            format!("{}", ComputeStyleHelper::new(&bg_fun(&tmp))),
                            format!("\x1B[{style_res};{bg_res}m"),
                            "Input: '{input}'"
                        );
                    }
                }
            }
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        fn style_fg_bg() {
            for input in INPUTS {
                for (fg_fun, fg_res) in FG_MAPPINGS {
                    for (bg_fun, bg_res) in BG_MAPPINGS {
                        for (style_fun, style_res) in STYLE_MAPPINGS {
                            let tmp = style_fun(input);
                            let tmp = fg_fun(&tmp);
                            assert_eq!(
                                format!("{}", ComputeStyleHelper::new(&bg_fun(&tmp))),
                                format!("\x1B[{style_res};{bg_res};{fg_res}m"),
                                "Input: '{input}'"
                            );
                        }
                    }
                }
            }
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        #[ignore = "Not yet implemented"]
        fn multiple_style_fg() {
            todo!()
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        #[ignore = "Not yet implemented"]
        fn multiple_style_bg() {
            todo!()
        }
        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        #[ignore = "Not yet implemented"]
        fn multiple_style_fg_bg() {
            todo!()
        }
    }

    mod escape_rest {
        use crate::{ColoredString, Colorize, EscapeInnerResetSequencesHelper};

        #[test]
        fn do_nothing_on_empty_strings() {
            let style = ColoredString::default();
            let expected = String::new();

            let output = EscapeInnerResetSequencesHelper::new(&style, &style).to_string();

            assert_eq!(expected, output);
        }

        #[test]
        fn do_nothing_on_string_with_no_reset() {
            let style = ColoredString {
                input: String::from("hello world !"),
                ..ColoredString::default()
            };

            let expected = String::from("hello world !");
            let output = EscapeInnerResetSequencesHelper::new(&style, &style).to_string();

            assert_eq!(expected, output);
        }

        #[cfg_attr(feature = "no-color", ignore)]
        #[test]
        fn replace_inner_reset_sequence_with_current_style() {
            let input = format!("start {} end", String::from("hello world !").red());
            let style = input.blue();

            let output = EscapeInnerResetSequencesHelper::new(&style, &style).to_string();
            let blue = "\x1B[34m";
            let red = "\x1B[31m";
            let reset = "\x1B[0m";
            let expected = format!("start {red}hello world !{reset}{blue} end");
            assert_eq!(expected, output);
        }

        #[cfg_attr(feature = "no-color", ignore)]
        #[test]
        fn replace_multiple_inner_reset_sequences_with_current_style() {
            let italic_str = String::from("yo").italic();
            let input = format!("start 1:{italic_str} 2:{italic_str} 3:{italic_str} end");
            let style = input.blue();

            let output = EscapeInnerResetSequencesHelper::new(&style, &style).to_string();
            let blue = "\x1B[34m";
            let italic = "\x1B[3m";
            let reset = "\x1B[0m";
            let expected = format!(
                    "start 1:{italic}yo{reset}{blue} 2:{italic}yo{reset}{blue} 3:{italic}yo{reset}{blue} end"
                );

            assert_eq!(expected, output, "first: {expected}\nsecond: {output}");
        }
    }

    #[test]
    fn color_fn() {
        macro_rules! helper {
            ($f:ident) => {
                ("blue".$f(), "blue".color(stringify!($f)))
            };
        }
        macro_rules! helper_bright {
            ($f:ident, $e:expr) => {
                ("blue".$f(), "blue".color(concat!("bright ", $e)))
            };
        }
        let mappings = &[
            helper!(black),
            helper!(red),
            helper!(green),
            helper!(yellow),
            helper!(blue),
            helper!(magenta),
            helper!(cyan),
            helper!(white),
            helper_bright!(bright_black, "black"),
            helper_bright!(bright_red, "red"),
            helper_bright!(bright_green, "green"),
            helper_bright!(bright_yellow, "yellow"),
            helper_bright!(bright_blue, "blue"),
            helper_bright!(bright_magenta, "magenta"),
            helper_bright!(bright_cyan, "cyan"),
            helper_bright!(bright_white, "white"),
        ];

        for (l, r) in mappings {
            assert_eq!(l, r);
        }
    }

    #[test]
    fn exposing_tests() {
        #![allow(deprecated)]

        let cstring = "".red();
        assert_eq!(cstring.fgcolor(), Some(Color::Red));
        assert_eq!(cstring.bgcolor(), None);

        let cstring = cstring.clear();
        assert_eq!(cstring.fgcolor(), None);
        assert_eq!(cstring.bgcolor(), None);

        let cstring = cstring.blue().on_bright_yellow();
        assert_eq!(cstring.fgcolor(), Some(Color::Blue));
        assert_eq!(cstring.bgcolor(), Some(Color::BrightYellow));

        let cstring = cstring.bold().italic();
        assert_eq!(cstring.fgcolor(), Some(Color::Blue));
        assert_eq!(cstring.bgcolor(), Some(Color::BrightYellow));
        assert!(cstring.style().contains(Styles::Bold));
        assert!(cstring.style().contains(Styles::Italic));
        assert!(!cstring.style().contains(Styles::Dimmed));
    }
}

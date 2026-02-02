
use crate::color::Color;
use crate::style::Style;
use crate::{ColoredString, CustomColor, style};

#[cfg(feature = "past-formatter")]
pub trait ColoringFormatter {
    fn format(out: String, fg: Color, bg: Color, style: Style) -> String;
}

#[cfg(feature = "past-formatter")]
pub struct NoColor;

#[cfg(feature = "past-formatter")]
impl ColoringFormatter for NoColor {
    fn format(out: String, _fg: Color, _bg: Color, _style: Style) -> String {
        out
    }
}



/// A color and style configuration that can be applied to multiple strings.
///
/// Unlike `ColoredString` which stores both the text and styling,
/// `Colorist` only stores the styling configuration and can be reused
/// to apply the same styling to multiple strings.
///
/// # Examples
///
/// ```
/// # use colored::*;
/// let mut painter = Colorist::new();
/// painter.fgcolor = Some(Color::Red);
/// painter.style = Styles::Bold.into();
///
/// let red_bold_text1 = painter.paint("Hello");
/// let red_bold_text2 = painter.paint("World");
///
/// // You can also chain methods
/// let text = Colorist::new()
///     .color(Color::Blue)
///     .on_color(Color::Yellow)
///     .bold()
///     .paint("Chained!");
/// ```



#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Colorist {
    /// The color of the text as it will be printed.
    pub fgcolor: Option<Color>,

    /// The background color (if any). None means that the text will be printed
    /// without a special background.
    pub bgcolor: Option<Color>,

    /// Any special styling to be applied to the text (see Styles for a list of
    /// available options).
    pub style: style::Style,
}

impl Colorist {
    /// Creates a new `Colorist` with no color or styling.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a Colorist from a colored string.
    #[inline]
    pub fn from_colored_string(colored_string: &ColoredString) -> Self {
        Colorist {
            fgcolor: colored_string.fgcolor,
            bgcolor: colored_string.bgcolor,
            style: colored_string.style,
        }
    }

    /// Creates a new `Colorist` with the given foreground color.
    pub fn with_fgcolor(color: Color) -> Self {
        Self {
            fgcolor: Some(color),
            ..Self::default()
        }
    }

    /// Creates a new `Colorist` with the given background color.
    pub fn with_bgcolor(color: Color) -> Self {
        Self {
            bgcolor: Some(color),
            ..Self::default()
        }
    }

    /// Creates a new `Colorist` with the given style.
    pub fn with_style(style: style::Style) -> Self {
        Self {
            style,
            ..Self::default()
        }
    }

    /// Apply the current color and style configuration to a string.
    pub fn paint<T: Into<String>>(&self, text: T) -> ColoredString {
        ColoredString {
            input: text.into(),
            fgcolor: self.fgcolor,
            bgcolor: self.bgcolor,
            style: self.style,
        }
    }

    /// Apply the current color and style configuration to a string reference.
    /// This is more efficient than `paint` for string slices that don't need
    /// to be owned.
    pub fn paint_ref(&self, text: &str) -> ColoredString {
        ColoredString {
            input: text.to_string(),
            fgcolor: self.fgcolor,
            bgcolor: self.bgcolor,
            style: self.style,
        }
    }

    /// Set the foreground color.
    pub fn color<C: Into<Color>>(mut self, color: C) -> Self {
        self.fgcolor = Some(color.into());
        self
    }

    /// Set the background color.
    pub fn on_color<C: Into<Color>>(mut self, color: C) -> Self {
        self.bgcolor = Some(color.into());
        self
    }

    /// Clear all styling (color and style).
    pub fn clear(mut self) -> Self {
        self.fgcolor = None;
        self.bgcolor = None;
        self.style = style::CLEAR;
        self
    }

    /// Clear only the foreground color.
    pub fn clear_fgcolor(mut self) -> Self {
        self.fgcolor = None;
        self
    }

    /// Clear only the background color.
    pub fn clear_bgcolor(mut self) -> Self {
        self.bgcolor = None;
        self
    }

    /// Clear only the style.
    pub fn clear_style(mut self) -> Self {
        self.style = style::CLEAR;
        self
    }

    /// Check if the colorist has any color or styling applied.
    pub fn is_plain(&self) -> bool {
        self.fgcolor.is_none() && self.bgcolor.is_none() && self.style == style::CLEAR
    }
}

#[allow(missing_docs)]
impl Colorist {
    // Font Colors
    pub fn black(self) -> Self {
        self.color(Color::Black)
    }
    pub fn red(self) -> Self {
        self.color(Color::Red)
    }
    pub fn green(self) -> Self {
        self.color(Color::Green)
    }
    pub fn yellow(self) -> Self {
        self.color(Color::Yellow)
    }
    pub fn blue(self) -> Self {
        self.color(Color::Blue)
    }
    pub fn magenta(self) -> Self {
        self.color(Color::Magenta)
    }
    pub fn purple(self) -> Self {
        self.color(Color::Magenta)
    }
    pub fn cyan(self) -> Self {
        self.color(Color::Cyan)
    }
    pub fn white(self) -> Self {
        self.color(Color::White)
    }
    pub fn bright_black(self) -> Self {
        self.color(Color::BrightBlack)
    }
    pub fn bright_red(self) -> Self {
        self.color(Color::BrightRed)
    }
    pub fn bright_green(self) -> Self {
        self.color(Color::BrightGreen)
    }
    pub fn bright_yellow(self) -> Self {
        self.color(Color::BrightYellow)
    }
    pub fn bright_blue(self) -> Self {
        self.color(Color::BrightBlue)
    }
    pub fn bright_magenta(self) -> Self {
        self.color(Color::BrightMagenta)
    }
    pub fn bright_purple(self) -> Self {
        self.color(Color::BrightMagenta)
    }
    pub fn bright_cyan(self) -> Self {
        self.color(Color::BrightCyan)
    }
    pub fn bright_white(self) -> Self {
        self.color(Color::BrightWhite)
    }
    pub fn truecolor(self, r: u8, g: u8, b: u8) -> Self {
        self.color(Color::TrueColor { r, g, b })
    }
    pub fn custom_color<C: Into<CustomColor>>(self, color: C) -> Self {
        let color = color.into();
        self.color(Color::TrueColor {
            r: color.r,
            g: color.g,
            b: color.b,
        })
    }
    pub fn ansi_color<T: Into<u8>>(self, color: T) -> Self {
        self.color(Color::AnsiColor(color.into()))
    }

    // Background Colors
    pub fn on_black(self) -> Self {
        self.on_color(Color::Black)
    }
    pub fn on_red(self) -> Self {
        self.on_color(Color::Red)
    }
    pub fn on_green(self) -> Self {
        self.on_color(Color::Green)
    }
    pub fn on_yellow(self) -> Self {
        self.on_color(Color::Yellow)
    }
    pub fn on_blue(self) -> Self {
        self.on_color(Color::Blue)
    }
    pub fn on_magenta(self) -> Self {
        self.on_color(Color::Magenta)
    }
    pub fn on_purple(self) -> Self {
        self.on_color(Color::Magenta)
    }
    pub fn on_cyan(self) -> Self {
        self.on_color(Color::Cyan)
    }
    pub fn on_white(self) -> Self {
        self.on_color(Color::White)
    }
    pub fn on_bright_black(self) -> Self {
        self.on_color(Color::BrightBlack)
    }
    pub fn on_bright_red(self) -> Self {
        self.on_color(Color::BrightRed)
    }
    pub fn on_bright_green(self) -> Self {
        self.on_color(Color::BrightGreen)
    }
    pub fn on_bright_yellow(self) -> Self {
        self.on_color(Color::BrightYellow)
    }
    pub fn on_bright_blue(self) -> Self {
        self.on_color(Color::BrightBlue)
    }
    pub fn on_bright_magenta(self) -> Self {
        self.on_color(Color::BrightMagenta)
    }
    pub fn on_bright_purple(self) -> Self {
        self.on_color(Color::BrightMagenta)
    }
    pub fn on_bright_cyan(self) -> Self {
        self.on_color(Color::BrightCyan)
    }
    pub fn on_bright_white(self) -> Self {
        self.on_color(Color::BrightWhite)
    }
    pub fn on_truecolor(self, r: u8, g: u8, b: u8) -> Self {
        self.on_color(Color::TrueColor { r, g, b })
    }
    pub fn on_custom_color<C: Into<CustomColor>>(self, color: C) -> Self {
        let color = color.into();
        self.on_color(Color::TrueColor {
            r: color.r,
            g: color.g,
            b: color.b,
        })
    }
    pub fn on_ansi_color<T: Into<u8>>(self, color: T) -> Self {
        self.on_color(Color::AnsiColor(color.into()))
    }

    // Styles
    pub fn normal(self) -> Self {
        self.clear()
    }
    pub fn bold(mut self) -> Self {
        self.style.add(style::Styles::Bold);
        self
    }
    pub fn dimmed(mut self) -> Self {
        self.style.add(style::Styles::Dimmed);
        self
    }
    pub fn italic(mut self) -> Self {
        self.style.add(style::Styles::Italic);
        self
    }
    pub fn underline(mut self) -> Self {
        self.style.add(style::Styles::Underline);
        self
    }
    pub fn blink(mut self) -> Self {
        self.style.add(style::Styles::Blink);
        self
    }
    pub fn reversed(mut self) -> Self {
        self.style.add(style::Styles::Reversed);
        self
    }
    pub fn hidden(mut self) -> Self {
        self.style.add(style::Styles::Hidden);
        self
    }
    pub fn strikethrough(mut self) -> Self {
        self.style.add(style::Styles::Strikethrough);
        self
    }
}

impl From<ColoredString> for Colorist {
    #[inline]
    fn from(cs: ColoredString) -> Self {
        Self::from_colored_string(&cs)
    }
}

// 为 Colorist 实现 From<Color> 以便于从颜色创建 Colorist
impl From<Color> for Colorist {
    #[inline]
    fn from(color: Color) -> Self {
        Self {
            fgcolor: Some(color),
            bgcolor: None,
            style: style::CLEAR,
        }
    }
}

impl From<Style> for Colorist {
    #[inline]
    fn from(style: Style) -> Self {
        Self {
            fgcolor: None,
            bgcolor: None,
            style,
        }
    }
}
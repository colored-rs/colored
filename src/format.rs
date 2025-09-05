use core::fmt::{self, Write};

use crate::{style, Color, ColoredString};

/// Escape inner reset sequences, so we can represent [`ColoredString`] in a [`ColoredString`].
fn escape_inner_reset_sequences(
    input: &str,
    fgcolor: Option<Color>,
    bgcolor: Option<Color>,
    style: style::Style,
    is_plain: bool,
    writer: &mut dyn Write,
) -> std::fmt::Result {
    const RESET: &str = "\x1B[0m";

    if !ColoredString::has_colors() || is_plain {
        return writer.write_str(input);
    }

    // All reset markers in the input
    let mut matches = input
        .match_indices(RESET)
        .map(|(idx, _)| idx + RESET.len())
        .peekable();
    if matches.peek().is_none() {
        return writer.write_str(input);
    }

    let mut start = 0;
    for offset in matches {
        // Write the text up to the end reset sequence
        writer.write_str(&input[start..offset])?;
        // Remember where the next text starts
        start = offset;
        // Write style
        compute_style(fgcolor, bgcolor, style, is_plain, writer)?;
    }

    // Write rest
    writer.write_str(&input[start..])?;

    Ok(())
}

/// Compute the style, which needs to be inserted to color the [`ColoredString`].
fn compute_style(
    fgcolor: Option<Color>,
    bgcolor: Option<Color>,
    style: style::Style,
    is_plain: bool,
    writer: &mut dyn Write,
) -> std::fmt::Result {
    if !ColoredString::has_colors() || is_plain {
        return Ok(());
    }
    writer.write_str("\x1B[")?;
    let mut has_wrote = if style == style::CLEAR {
        false
    } else {
        style.private_write(writer)?;
        true
    };

    if let Some(ref bgcolor) = bgcolor {
        if has_wrote {
            writer.write_char(';')?;
        }
        bgcolor.to_bg_write(writer)?;
        has_wrote = true;
    }

    if let Some(ref fgcolor) = fgcolor {
        if has_wrote {
            writer.write_char(';')?;
        }
        fgcolor.to_fg_write(writer)?;
    }

    writer.write_char('m')?;
    Ok(())
}

impl fmt::Display for ColoredString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // No color to format
        if !Self::has_colors() || self.is_plain() {
            return <String as fmt::Display>::fmt(&self.input, f);
        }

        let (actual_input, padding) = calculate_precision_and_padding(self, f);

        // Do the actual formatting
        // XXX: see tests. Useful when nesting colored strings
        compute_style(self.fgcolor, self.bgcolor, self.style, self.is_plain(), f)?;
        escape_inner_reset_sequences(
            actual_input,
            self.fgcolor,
            self.bgcolor,
            self.style,
            self.is_plain(),
            f,
        )?;
        f.write_str("\x1B[0m")?;

        // Add padding
        write_padding(f, padding)?;

        Ok(())
    }
}

fn write_padding(f: &mut std::fmt::Formatter<'_>, padding: usize) -> Result<(), std::fmt::Error> {
    for _ in 0..padding {
        f.write_char(f.fill())?;
    }
    Ok(())
}

/// Calculates how long the input should be with the specified precision and how much padding is needed.
fn calculate_precision_and_padding<'a>(
    colored_string: &'a str,
    f: &std::fmt::Formatter<'_>,
) -> (&'a str, usize) {
    let mut input = colored_string;
    let mut padding = 0;

    // Calculate padding and input
    if let Some(precision) = f.precision() {
        let mut iter = input.char_indices();

        // FIXME
        // feature(iter_advance_by) is not stable
        // CharIndices::offset is MSRV 1.82.0
        // the std impl uses this methods in it is a lot nicer.
        let mut count = 0;
        let mut offset = 0;
        for _ in 0..precision {
            if let Some((i, c)) = iter.next() {
                offset = i + c.len_utf8();
                count += 1;
            }
        }

        // Short our input to the precision
        input = &input[..offset];

        // Calculate remaining padding in respect to characters and not bytes
        if let Some(width) = f.width() {
            padding = width.saturating_sub(count);
        }
    } else if let Some(width) = f.width() {
        // Calculate padding in respect to characters and not bytes
        padding = width.saturating_sub(colored_string.chars().take(width).count());
    }
    (input, padding)
}

/// Test the formatting
///
/// Formatting *must* respect padding
///
/// The added normal str in the input is for comparison to rust-std
#[cfg(test)]
mod tests {
    use super::*;
    mod formatting {
        use core::fmt::Display;

        use crate::Colorize;

        #[test]
        /// Insert padding on empty string
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
        /// Should do not any padding
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
        /// Should do not any padding because it is less or equal the chars
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
        /// Should do padding whie having input
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
        /// Precision should shorten the input
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
        /// Presision should not shorten the input
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
        /// Presision should not shorten the input
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
        /// Testing padding and precision together
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

    mod compute_style {
        use crate::{format::tests::compute_style, ColoredString, Colorize};

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

        fn compute_style_colored(colored: &ColoredString) -> String {
            let mut buf = String::new();
            compute_style(
                colored.fgcolor,
                colored.bgcolor,
                colored.style,
                colored.is_plain(),
                &mut buf,
            )
            .unwrap();
            buf
        }

        #[test]
        #[cfg_attr(feature = "no-color", ignore)]
        /// Check for only a set foreground color
        fn simple_fg() {
            for input in INPUTS {
                for (fun, res) in FG_MAPPINGS {
                    assert_eq!(
                        compute_style_colored(&fun(input)),
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
                        compute_style_colored(&fun(input)),
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
                            compute_style_colored(&fg_fun(&tmp)),
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
                        compute_style_colored(&fun(input)),
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
                            compute_style_colored(&fg_fun(&tmp)),
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
                            compute_style_colored(&bg_fun(&tmp)),
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
                                compute_style_colored(&bg_fun(&tmp)),
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
        use crate::{format::escape_inner_reset_sequences, ColoredString, Colorize};

        fn escape_inner_reset_sequences_colored(colored: &ColoredString) -> String {
            let mut buf = String::new();
            escape_inner_reset_sequences(
                &colored.input,
                colored.fgcolor,
                colored.bgcolor,
                colored.style,
                colored.is_plain(),
                &mut buf,
            )
            .unwrap();
            buf
        }

        #[test]
        fn do_nothing_on_empty_strings() {
            let style = ColoredString::default();
            let expected = String::new();

            let output = escape_inner_reset_sequences_colored(&style);

            assert_eq!(expected, output);
        }

        #[test]
        fn do_nothing_on_string_with_no_reset() {
            let style = ColoredString {
                input: String::from("hello world !"),
                ..ColoredString::default()
            };

            let expected = String::from("hello world !");
            let output = escape_inner_reset_sequences_colored(&style);

            assert_eq!(expected, output);
        }

        #[cfg_attr(feature = "no-color", ignore)]
        #[test]
        fn replace_inner_reset_sequence_with_current_style() {
            let input = format!("start {} end", String::from("hello world !").red());
            let style = input.blue();

            let output = escape_inner_reset_sequences_colored(&style);
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

            let output = escape_inner_reset_sequences_colored(&style);
            let blue = "\x1B[34m";
            let italic = "\x1B[3m";
            let reset = "\x1B[0m";
            let expected = format!(
                    "start 1:{italic}yo{reset}{blue} 2:{italic}yo{reset}{blue} 3:{italic}yo{reset}{blue} end"
                );

            assert_eq!(expected, output, "first: {expected}\nsecond: {output}");
        }
    }
}

#![cfg(not(feature = "no-color"))]
#![allow(unused_imports)]

extern crate colored;
extern crate nu_ansi_term;

use colored::*;
use nu_ansi_term::*;

macro_rules! test_simple_color {
    ($string:expr, $colored_name:ident, $nu_ansi_term_name:ident) => {
        #[test]
        fn $colored_name() {
            let s = format!("{} {}", $string, stringify!($colored_name));
            assert_eq!(
                s.$colored_name().to_string(),
                nu_ansi_term::Color::$nu_ansi_term_name.paint(s).to_string()
            )
        }
    };
}

mod compat_colors {
    use super::colored::*;
    use super::nu_ansi_term::*;

    test_simple_color!("test string", black, Black);
    test_simple_color!("test string", red, Red);
    test_simple_color!("test string", green, Green);
    test_simple_color!("test string", yellow, Yellow);
    test_simple_color!("test string", blue, Blue);
    test_simple_color!("test string", magenta, Purple);
    test_simple_color!("test string", cyan, Cyan);
    test_simple_color!("test string", white, White);
}

macro_rules! test_simple_style {
    ($string:expr, $colored_style:ident, $nu_ansi_term_style:ident) => {
        #[test]
        fn $colored_style() {
            let s = format!("{} {}", $string, stringify!($colored_style));
            assert_eq!(
                s.$colored_style().to_string(),
                nu_ansi_term::Style::new()
                    .$nu_ansi_term_style()
                    .paint(s)
                    .to_string()
            )
        }
    };
}

mod compat_styles {
    use super::colored;
    use super::colored::*;
    use super::nu_ansi_term;
    use super::nu_ansi_term::*;

    test_simple_style!("test string", bold, bold);
    test_simple_style!("test string", dimmed, dimmed);
    test_simple_style!("test string", italic, italic);
    test_simple_style!("test string", underline, underline);
    test_simple_style!("test string", blink, blink);
    test_simple_style!("test string", reversed, reverse);
    test_simple_style!("test string", hidden, hidden);
}

macro_rules! test_simple_bgcolor {
    ($string:expr, $colored_name:ident, $nu_ansi_term_name:ident) => {
        #[test]
        fn $colored_name() {
            let s = format!("{} {}", $string, stringify!($colored_name));
            assert_eq!(
                s.$colored_name().to_string(),
                nu_ansi_term::Style::default()
                    .on(nu_ansi_term::Color::$nu_ansi_term_name)
                    .paint(s)
                    .to_string()
            )
        }
    };
}

mod compat_bgcolors {
    use super::colored;
    use super::colored::*;
    use super::nu_ansi_term;
    use super::nu_ansi_term::*;

    test_simple_bgcolor!("test string", on_black, Black);
    test_simple_bgcolor!("test string", on_red, Red);
    test_simple_bgcolor!("test string", on_green, Green);
    test_simple_bgcolor!("test string", on_yellow, Yellow);
    test_simple_bgcolor!("test string", on_blue, Blue);
    test_simple_bgcolor!("test string", on_magenta, Purple);
    test_simple_bgcolor!("test string", on_cyan, Cyan);
    test_simple_bgcolor!("test string", on_white, White);
}

mod compat_complex {
    use super::colored;
    use super::colored::*;
    use super::nu_ansi_term;
    use super::nu_ansi_term::*;

    #[test]
    fn complex1() {
        let s = "test string";
        let ansi = nu_ansi_term::Color::Red
            .on(nu_ansi_term::Color::Black)
            .bold()
            .italic()
            .paint(s);
        assert_eq!(
            ansi.to_string(),
            s.red().bold().italic().on_black().to_string()
        );
    }

    #[test]
    fn complex2() {
        let s = "test string";
        let ansi = nu_ansi_term::Color::Green
            .on(nu_ansi_term::Color::Yellow)
            .underline()
            .paint(s);
        assert_eq!(
            ansi.to_string(),
            s.green().on_yellow().underline().to_string()
        );
    }
}

mod compat_overrides {
    use super::colored;
    use super::colored::*;
    use super::nu_ansi_term;
    use super::nu_ansi_term::*;

    #[test]
    fn overrides1() {
        let s = "test string";
        let ansi = nu_ansi_term::Color::Red
            .on(nu_ansi_term::Color::Black)
            .on(nu_ansi_term::Color::Blue)
            .paint(s);
        assert_eq!(ansi.to_string(), s.red().on_blue().to_string());
    }

    #[test]
    fn overrides2() {
        let s = "test string";
        let ansi = nu_ansi_term::Color::Green
            .on(nu_ansi_term::Color::Yellow)
            .paint(s);
        assert_eq!(
            ansi.to_string(),
            s.green().on_yellow().green().on_yellow().to_string()
        );
    }
}

/// A macro to conveniently print colorized, formatted output.
///
/// Follows the syntax of [`println`], but with a list of [`colored`][crate] methods to apply
/// to the formatted string. The methods listed before the format string will be applied to the
/// output.
///
/// Note: Allocates a new string per call.
///
/// # Examples
/// ```rust
/// println_color!(blue, bold, "This will be {} and {}!", "blue", "bold");
///
/// // is equivalent to:
///
/// println!("{}", format!("This will be {} and {}!", "blue", "bold").blue().bold());
/// ```
#[macro_export]
macro_rules! println_color {
    ($($color:ident),*, $fmt_str:literal, $($rest:tt)*) => {
        println!(
            "{}",
            format!($fmt_str, $($rest)*)
                $(
                    .$color()
                )*
        )
    };
    ($($color:ident),*, $fmt_str:literal) => {
        println!(
            "{}",
            format!($fmt_str)
                $(
                    .$color()
                )*
        )
    };
}
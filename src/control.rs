//! A couple of functions to set whether to colorize.
//!
//! You can set the color level to use for the terminal with [`set_should_colorize()`].
//!
//! And get the current color level with [`get_should_colorize()`].
//!
//! # Example
//! ```rust
//! use colored::control::{set_should_colorize, ShouldColorize};
//!
//! // Enable colorization with the detected color level
//! set_should_colorize(ShouldColorize::Yes);
//!
//! // Disable colorization
//! set_should_colorize(ShouldColorize::No);
//!
//! // Enable colorization but force 16 ANSI colors
//! set_should_colorize(ShouldColorize::YesWithAnsi16);
//!
//! // Automatically enable or disable colorization based on environment variables and terminal status
//! set_should_colorize(ShouldColorize::from_env());
//!
//! ```

use std::io::IsTerminal;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::LazyLock;
use std::{env, io};

/// The Color Level detected automatically.
pub static COLOR_LEVEL_DETECTED: LazyLock<ColorLevel> = LazyLock::new(ColorLevel::detect);

/// The flag to override the colorization,
/// which points to enum [`ShouldColorize`].
///
/// Default value is generated by [`ShouldColorize::from_env()`].
pub static SHOULD_COLORIZE: LazyLock<AtomicU8> =
    LazyLock::new(|| AtomicU8::new(ShouldColorize::from_env() as u8));

/// Sets a flag to the console to use a virtual terminal environment.
///
/// This is primarily used for Windows 10 environments which will not correctly colorize
/// the outputs based on ANSI escape codes.
///
/// The returned `Result` is _always_ `Ok(())`, the return type was kept to ensure backwards
/// compatibility.
///
/// # Notes
/// > Only available to `Windows` build targets.
///
/// # Example
/// ```rust
/// use colored::*;
/// control::set_virtual_terminal(false).unwrap();
/// println!("{}", "bright cyan".bright_cyan());    // will print '[96mbright cyan[0m' on windows 10
///
/// control::set_virtual_terminal(true).unwrap();
/// println!("{}", "bright cyan".bright_cyan());    // will print correctly
/// ```
/// # Errors
/// This function will return `Ok(())` if the operation was successful.
#[allow(clippy::result_unit_err)]
#[cfg(windows)]
pub fn set_virtual_terminal(use_virtual: bool) -> Result<(), ()> {
    use windows_sys::Win32::System::Console::{
        GetConsoleMode, GetStdHandle, SetConsoleMode, ENABLE_VIRTUAL_TERMINAL_PROCESSING,
        STD_OUTPUT_HANDLE,
    };

    #[allow(unsafe_code)] // needed here
    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        let mut original_mode = 0;
        GetConsoleMode(handle, &mut original_mode);

        let enabled = original_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING
            == ENABLE_VIRTUAL_TERMINAL_PROCESSING;

        match (use_virtual, enabled) {
            // not enabled, should be enabled
            (true, false) => {
                SetConsoleMode(handle, ENABLE_VIRTUAL_TERMINAL_PROCESSING | original_mode)
            }
            // already enabled, should be disabled
            (false, true) => {
                SetConsoleMode(handle, ENABLE_VIRTUAL_TERMINAL_PROCESSING ^ original_mode)
            }
            _ => 0,
        };
    }

    Ok(())
}

/// Sets the color level to use for the terminal.
///
/// Default value is generated by [`ShouldColorize::from_env()`].
pub fn set_should_colorize(should_colorize: ShouldColorize) {
    SHOULD_COLORIZE.store(should_colorize as u8, Ordering::Relaxed);
}

/// Gets the current color level to use for the terminal.
pub fn get_should_colorize() -> ShouldColorize {
    SHOULD_COLORIZE.load(Ordering::Relaxed).into()
}

pub(crate) fn get_current_color_level() -> ColorLevel {
    match get_should_colorize() {
        ShouldColorize::No => ColorLevel::None,
        ShouldColorize::Yes => *COLOR_LEVEL_DETECTED,
        level => level.into(),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The color level to use for the terminal.
/// Determines the color depth to use for the terminal.
pub enum ColorLevel {
    /// Use no colors.
    None,
    /// Use 16 ANSI colors.
    Ansi16,
    /// Use 256 ANSI colors.
    Ansi256,
    /// Use True Color.
    TrueColor,
}

impl ColorLevel {
    fn detect() -> Self {
        // Check for 24-bit color support via COLORTERM
        if env::var("COLORTERM").is_ok_and(|v| matches!(v.as_str(), "truecolor" | "24bit")) {
            return Self::TrueColor;
        }

        // Detect Windows Terminal in Windows and WSL
        if env::var_os("WT_SESSION").is_some() {
            return Self::TrueColor;
        }

        // Detect CI environments, which typically have limited color support
        if env::var_os("CI").is_some() {
            return Self::Ansi256;
        }

        // Windows version-specific checks
        #[cfg(target_os = "windows")]
        {
            use windows_version::OsVersion;
            let version = OsVersion::current();

            // Windows 10 build 14931+ supports TrueColor
            if version >= OsVersion::new(10, 0, 0, 14931) {
                return Self::TrueColor;
            }

            // Windows 10 build 10586+ supports 256 colors
            if version >= OsVersion::new(10, 0, 0, 10586) {
                return Self::Ansi256;
            }
        }

        // Check TERM for 256-color indication
        if let Some(term) = env::var_os("TERM").and_then(|term| term.into_string().ok()) {
            if term.ends_with("-256color") || term.ends_with("256") {
                return Self::Ansi256;
            }
        }

        // Fallback to basic ANSI colors
        Self::Ansi16
    }
}

/// Whether and how to colorize the output.
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum ShouldColorize {
    /// Do not colorize the output.
    No,
    /// Colorize the output with the automaticly detected color level.
    Yes,
    /// Force colorization with 16 ANSI colors.
    YesWithAnsi16,
    /// Force colorization with 256 ANSI colors.
    YesWithAnsi256,
    /// Force colorization with True Color.
    YesWithTrueColor,
}

impl ShouldColorize {
    /// Determines if colorization should be applied based on environment variables and terminal status.
    ///
    /// Priority order for environment variables:
    /// 1. `CLICOLOR_FORCE` (force enable colorization)
    /// 2. `NO_COLOR` (force disable colorization)
    /// 3. `CLICOLOR` (enable colorization if set, depending on tty)
    /// 4. If none of the above, use the terminal status (enabled if stdout is a tty)
    #[must_use]
    pub fn from_env() -> Self {
        if env::var("CLICOLOR_FORCE").is_ok_and(|v| v != "0") {
            return Self::Yes;
        }

        if env::var("NO_COLOR").is_ok() {
            return Self::No;
        }

        if env::var("CLICOLOR").is_ok_and(|v| v != "0") {
            return Self::Yes;
        }

        if io::stdout().is_terminal() {
            Self::Yes
        } else {
            Self::No
        }
    }
}

impl From<u8> for ShouldColorize {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::No,
            2 => Self::YesWithAnsi16,
            3 => Self::YesWithAnsi256,
            4 => Self::YesWithTrueColor,
            _ => Self::Yes, // unreachable, but default to Yes to avoid panics
        }
    }
}

impl From<ShouldColorize> for ColorLevel {
    fn from(value: ShouldColorize) -> Self {
        match value {
            ShouldColorize::YesWithAnsi16 => Self::Ansi16,
            ShouldColorize::YesWithAnsi256 => Self::Ansi256,
            ShouldColorize::YesWithTrueColor => Self::TrueColor,
            _ => Self::None,
        }
    }
}

#[cfg(test)]
mod test {
    // TODO
}

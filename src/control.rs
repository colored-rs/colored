//! A couple of functions to enable and disable coloring.

use std::default::Default;
use std::env;
use std::io::{self, IsTerminal};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::LazyLock;

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

/// A flag for whether coloring should occur.
pub struct ShouldColorize {
    clicolor: bool,
    clicolor_force: Option<bool>,
    // XXX we can't use Option<Atomic> because we can't use &mut references to ShouldColorize
    has_manual_override: AtomicBool,
    manual_override: AtomicBool,
}

/// Use this to force colored to ignore the environment and always/never colorize
/// See example/control.rs
pub fn set_override(override_colorize: bool) {
    SHOULD_COLORIZE.set_override(override_colorize);
}

/// Remove the manual override and let the environment decide if it's ok to colorize
/// See example/control.rs
pub fn unset_override() {
    SHOULD_COLORIZE.unset_override();
}

/// The persistent [`ShouldColorize`].
pub static SHOULD_COLORIZE: LazyLock<ShouldColorize> = LazyLock::new(ShouldColorize::from_env);

impl Default for ShouldColorize {
    fn default() -> Self {
        Self {
            clicolor: true,
            clicolor_force: None,
            has_manual_override: AtomicBool::new(false),
            manual_override: AtomicBool::new(false),
        }
    }
}

impl ShouldColorize {
    /// Reads environment variables and checks if output is a tty to determine
    /// whether colorization should be used or not.
    /// `CLICOLOR_FORCE` takes highest priority, followed by `NO_COLOR`,
    /// followed by `CLICOLOR` combined with tty check.
    #[must_use]
    pub fn from_env() -> Self {
        Self {
            clicolor: Self::normalize_env(env::var("CLICOLOR")).unwrap_or(true)
                && io::stdout().is_terminal(),
            clicolor_force: Self::resolve_clicolor_force(
                env::var("NO_COLOR"),
                env::var("CLICOLOR_FORCE"),
            ),
            ..Self::default()
        }
    }

    /// Returns if the current coloring is expected.
    pub fn should_colorize(&self) -> bool {
        if self.has_manual_override.load(Ordering::Relaxed) {
            return self.manual_override.load(Ordering::Relaxed);
        }

        if let Some(forced_value) = self.clicolor_force {
            return forced_value;
        }

        self.clicolor
    }

    /// Use this to force colored to ignore the environment and always/never colorize
    pub fn set_override(&self, override_colorize: bool) {
        self.has_manual_override.store(true, Ordering::Relaxed);
        self.manual_override
            .store(override_colorize, Ordering::Relaxed);
    }

    /// Remove the manual override and let the environment decide if it's ok to colorize
    pub fn unset_override(&self) {
        self.has_manual_override.store(false, Ordering::Relaxed);
    }

    /* private */

    fn normalize_env(env_res: Result<String, env::VarError>) -> Option<bool> {
        env_res.map_or(None, |string| Some(string != "0"))
    }

    fn resolve_clicolor_force(
        no_color: Result<String, env::VarError>,
        clicolor_force: Result<String, env::VarError>,
    ) -> Option<bool> {
        if Self::normalize_env(clicolor_force) == Some(true) {
            Some(true)
        } else if Self::normalize_env(no_color).is_some() {
            Some(false)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod specs {
    use super::*;
    use std::env;

    #[test]
    fn normalize_env_returns_none_for_errors() {
        assert_eq!(
            None,
            ShouldColorize::normalize_env(Err(env::VarError::NotPresent))
        );
        assert_eq!(
            None,
            ShouldColorize::normalize_env(Err(env::VarError::NotUnicode("".into())))
        );
    }

    #[test]
    fn normalize_env_returns_true_for_non_zero_values() {
        assert_eq!(
            Some(true),
            ShouldColorize::normalize_env(Ok(String::from("1")))
        );
    }

    #[test]
    fn normalize_env_returns_false_for_zero_values() {
        assert_eq!(
            Some(false),
            ShouldColorize::normalize_env(Ok(String::from("0")))
        );
    }

    #[test]
    fn resolve_clicolor_force_returns_none_when_no_override_is_enabled() {
        assert_eq!(
            None,
            ShouldColorize::resolve_clicolor_force(
                Err(env::VarError::NotPresent),
                Err(env::VarError::NotPresent)
            )
        );
        assert_eq!(
            None,
            ShouldColorize::resolve_clicolor_force(
                Err(env::VarError::NotPresent),
                Ok(String::from("0"))
            )
        );
    }

    #[test]
    fn resolve_clicolor_force_returns_false_when_no_color_is_set() {
        assert_eq!(
            Some(false),
            ShouldColorize::resolve_clicolor_force(
                Ok(String::from("0")),
                Err(env::VarError::NotPresent)
            )
        );
        assert_eq!(
            Some(false),
            ShouldColorize::resolve_clicolor_force(
                Ok(String::from("1")),
                Err(env::VarError::NotPresent)
            )
        );
        assert_eq!(
            Some(false),
            ShouldColorize::resolve_clicolor_force(Ok(String::from("1")), Ok(String::from("0")))
        );
    }

    #[test]
    fn resolve_clicolor_force_prioritizes_non_zero_clicolor_force() {
        assert_eq!(
            Some(true),
            ShouldColorize::resolve_clicolor_force(Ok(String::from("1")), Ok(String::from("1")))
        );
        assert_eq!(
            Some(false),
            ShouldColorize::resolve_clicolor_force(Ok(String::from("1")), Ok(String::from("0")))
        );
        assert_eq!(
            Some(true),
            ShouldColorize::resolve_clicolor_force(
                Err(env::VarError::NotPresent),
                Ok(String::from("1"))
            )
        );
    }

    #[test]
    fn constructors_exist() {
        ShouldColorize::default();
        let _ = ShouldColorize::from_env();
    }

    #[test]
    fn clicolor_false_disables_color() {
        let colorize_control = ShouldColorize {
            clicolor: false,
            ..ShouldColorize::default()
        };
        assert!(!colorize_control.should_colorize());
    }

    #[test]
    fn clicolor_true_enables_color() {
        let colorize_control = ShouldColorize {
            clicolor: true,
            ..ShouldColorize::default()
        };
        assert!(colorize_control.should_colorize());
    }

    #[test]
    fn unset_clicolor_enables_color() {
        assert!(ShouldColorize::default().should_colorize());
    }

    #[test]
    fn clicolor_force_true_overrides_clicolor() {
        let colorize_control = ShouldColorize {
            clicolor: false,
            clicolor_force: Some(true),
            ..ShouldColorize::default()
        };

        assert!(colorize_control.should_colorize());
    }

    #[test]
    fn clicolor_force_false_overrides_clicolor() {
        let colorize_control = ShouldColorize {
            clicolor: true,
            clicolor_force: Some(false),
            ..ShouldColorize::default()
        };

        assert!(!colorize_control.should_colorize());
    }

    #[test]
    fn manual_override_true_enables_color() {
        let colorize_control = ShouldColorize {
            clicolor: false,
            clicolor_force: None,
            has_manual_override: AtomicBool::new(true),
            manual_override: AtomicBool::new(true),
        };

        assert!(colorize_control.should_colorize());
    }

    #[test]
    fn manual_override_false_disables_color() {
        let colorize_control = ShouldColorize {
            clicolor: true,
            clicolor_force: Some(true),
            has_manual_override: AtomicBool::new(true),
            manual_override: AtomicBool::new(false),
        };

        assert!(!colorize_control.should_colorize());
    }

    #[test]
    fn set_override_sets_manual_override_state() {
        let colorize_control = ShouldColorize::default();
        colorize_control.set_override(true);

        assert!(colorize_control.has_manual_override.load(Ordering::Relaxed));
        assert!(colorize_control.manual_override.load(Ordering::Relaxed));

        colorize_control.set_override(false);

        assert!(colorize_control.has_manual_override.load(Ordering::Relaxed));
        assert!(!colorize_control.manual_override.load(Ordering::Relaxed));
    }

    #[test]
    fn unset_override_clears_manual_override_state() {
        let colorize_control = ShouldColorize::default();
        colorize_control.set_override(true);
        colorize_control.unset_override();
        assert!(!colorize_control.has_manual_override.load(Ordering::Relaxed));
    }
}

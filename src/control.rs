//! A couple of functions to enable and disable coloring.

use std::env;
use std::default::Default;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct ShouldColorize {
    clicolor: Option<bool>,
    clicolor_force: Option<bool>,
    // XXX we can't use Option<Atomic> because we can't use &mut references to ShouldColorize
    has_manual_override: AtomicBool,
    manual_override: AtomicBool,
}

/// Use this to force colored to ignore the environment and always/never colorize
/// See example/control.rs
pub fn set_override(override_colorize: bool) {
    SHOULD_COLORIZE.set_override(override_colorize)
}

/// Remove the manual override and let the environment decide if it's ok to colorize
/// See example/control.rs
pub fn unset_override() {
    SHOULD_COLORIZE.unset_override()
}


lazy_static! {
    pub static ref SHOULD_COLORIZE : ShouldColorize = ShouldColorize::from_env();
}

impl Default for ShouldColorize {
    fn default() -> ShouldColorize {
        ShouldColorize {
            clicolor: None,
            clicolor_force: None,
            has_manual_override: AtomicBool::new(false),
            manual_override: AtomicBool::new(false),
        }
    }
}

impl ShouldColorize {
    pub fn from_env() -> Self {
        use std::io;

        ShouldColorize {
            clicolor: ShouldColorize::normalize_env(env::var("CLICOLOR")),
            clicolor_force: ShouldColorize::normalize_env(env::var("CLICOLOR_FORCE")),
            ..ShouldColorize::default()
        }
    }

    pub fn should_colorize(&self) -> bool {

        if self.has_manual_override.load(Ordering::Relaxed) {
            return self.manual_override.load(Ordering::Relaxed);
        }

        if let Some(forced_value) = self.clicolor_force {
            return forced_value;
        }

        if let Some(value) = self.clicolor {
            return value;
        }

        true
    }

    pub fn set_override(&self, override_colorize: bool) {

        self.has_manual_override.store(true, Ordering::Relaxed);
        self.manual_override
            .store(override_colorize, Ordering::Relaxed);
    }

    pub fn unset_override(&self) {
        self.has_manual_override.store(false, Ordering::Relaxed);
    }

    /* private */

    fn normalize_env(env_res: Result<String, env::VarError>) -> Option<bool> {
        match env_res {
            Ok(string) => Some(string != "0"),
            Err(_) => None,
        }
    }
}

#[cfg(test)]
mod specs {
    use super::*;
    use rspec::context::*;
    use rspec;
    use std::env;

    #[test]
    fn clicolor_behavior() {

        use std::io;

        let stdout = &mut io::stdout();
        let mut formatter = rspec::formatter::Simple::new(stdout);
        let mut runner = describe("ShouldColorize", |ctx| {

            ctx.describe("::normalize_env", |ctx| {

                ctx.it("should return None if error", || {
                   assert_eq!(None, ShouldColorize::normalize_env(Err(env::VarError::NotPresent)));
                   assert_eq!(None, ShouldColorize::normalize_env(Err(env::VarError::NotUnicode("".into()))))
                });

                ctx.it("should return Some(true) if != 0", || {
                    Some(true) == ShouldColorize::normalize_env(Ok(String::from("1")))
                });

                ctx.it("should return Some(false) if == 0", || {
                    Some(false) == ShouldColorize::normalize_env(Ok(String::from("0")))
                });
            });

            ctx.describe("constructors", |ctx| {

                ctx.it("should have a default constructor",
                       || { ShouldColorize::default(); });

                ctx.it("should have an environment constructor",
                       || { ShouldColorize::from_env(); });
            });

            ctx.describe("when only changing clicolors", |ctx| {

                ctx.it("clicolor == false means no colors", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        ..ShouldColorize::default()
                    };
                    false == colorize_control.should_colorize()
                });

                ctx.it("clicolor == true means colors !", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        ..ShouldColorize::default()
                    };
                    true == colorize_control.should_colorize()
                });

                ctx.it("unset clicolors implies true",
                       || true == ShouldColorize::default().should_colorize());
            });

            ctx.describe("when using clicolor_force", |ctx| {

                ctx.it("clicolor_force should force to true no matter clicolor",
                       || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        clicolor_force: Some(true),
                        ..ShouldColorize::default()
                    };

                    true == colorize_control.should_colorize()
                });

                ctx.it("clicolor_force should force to false no matter clicolor",
                       || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        clicolor_force: Some(false),
                        ..ShouldColorize::default()
                    };

                    false == colorize_control.should_colorize()
                });
            });

            ctx.describe("using a manual override", |ctx| {

                ctx.it("shoud colorize if manual_override is true, but clicolor is false and clicolor_force also false", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        clicolor_force: None,
                        has_manual_override: AtomicBool::new(true),
                        manual_override: AtomicBool::new(true),
                        .. ShouldColorize::default()
                    };

                    true == colorize_control.should_colorize()
                });

                ctx.it("should not colorize if manual_override is false, but clicolor is true or clicolor_force is true", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        clicolor_force: Some(true),
                        has_manual_override: AtomicBool::new(true),
                        manual_override: AtomicBool::new(false),
                        .. ShouldColorize::default()
                    };

                    false == colorize_control.should_colorize()
                })
            });

            ctx.describe("::set_override", |ctx| {
                ctx.it("should exists", || {
                    let colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                });

                ctx.it("set the manual_override property", || {
                    let colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                    {
                        assert_eq!(true,
                                   colorize_control
                                       .has_manual_override
                                       .load(Ordering::Relaxed));
                        let val = colorize_control.manual_override.load(Ordering::Relaxed);
                        assert_eq!(true, val);
                    }
                    colorize_control.set_override(false);
                    {
                        assert_eq!(true,
                                   colorize_control
                                       .has_manual_override
                                       .load(Ordering::Relaxed));
                        let val = colorize_control.manual_override.load(Ordering::Relaxed);
                        assert_eq!(false, val);
                    }
                });
            });

            ctx.describe("::unset_override", |ctx| {
                ctx.it("should exists", || {
                    let colorize_control = ShouldColorize::default();
                    colorize_control.unset_override();
                });

                ctx.it("unset the manual_override property", || {
                    let colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                    colorize_control.unset_override();
                    assert_eq!(false,
                               colorize_control
                                   .has_manual_override
                                   .load(Ordering::Relaxed));
                });
            });
        });
        runner.add_event_handler(&mut formatter);
        runner.run().unwrap();
    }
}

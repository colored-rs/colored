
use std::env;
use std::default::Default;

pub struct ShouldColorize {
    clicolor: Option<bool>,
    clicolor_force: Option<bool>,
    manual_override: Option<bool>
}

impl Default for ShouldColorize {
    fn default() -> ShouldColorize {
        ShouldColorize {
            clicolor: None,
            clicolor_force: None,
            manual_override: None
        }
    }
}

impl ShouldColorize {

    pub fn from_env() -> Self {
        use std::io;

        ShouldColorize {
            clicolor: ShouldColorize::normalize_env(env::var("CLICOLOR")),
            clicolor_force: ShouldColorize::normalize_env(env::var("CLICOLOR_FORCE")),
            .. ShouldColorize::default()
        }
    }

    pub fn should_colorize(&self) -> bool {

        if let Some(manual_override) = self.manual_override {
            return manual_override;
        }

        if let Some(forced_value) = self.clicolor_force {
            return forced_value;
        }

        if let Some(value) = self.clicolor {
            return value;
        }

        return true;
    }

    pub fn set_override(&mut self, override_colorize: bool) {
        self.manual_override = Some(override_colorize);
    }

    pub fn unset_override(&mut self) {
        self.manual_override = None;
    }

    fn normalize_env(env_res: Result<String, env::VarError>) -> Option<bool> {
        match env_res {
            Ok(string) => Some(string != "0"),
            Err(_) => None
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

                ctx.it("should have a default constructor", || {
                    ShouldColorize::default();
                });

                ctx.it("should have an environment constructor", || {
                    ShouldColorize::from_env();
                });
            });

            ctx.describe("when only changing clicolors", |ctx| {

                ctx.it("clicolor == false means no colors", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        .. ShouldColorize::default()
                    };
                    false == colorize_control.should_colorize()
                });

                ctx.it("clicolor == true means colors !", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        .. ShouldColorize::default()
                    };
                    true == colorize_control.should_colorize()
                });

                ctx.it("unset clicolors implies true", || {
                    true == ShouldColorize::default().should_colorize()
                });
            });

            ctx.describe("when using clicolor_force", |ctx| {

                ctx.it("clicolor_force should force to true no matter clicolor", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        clicolor_force: Some(true),
                        .. ShouldColorize::default()
                    };

                    true == colorize_control.should_colorize()
                });

                ctx.it("clicolor_force should force to false no matter clicolor", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        clicolor_force: Some(false),
                        .. ShouldColorize::default()
                    };

                    false == colorize_control.should_colorize()
                });
            });
            
            ctx.describe("using a manual override", |ctx| {

                ctx.it("shoud colorize if manual_override is true, but clicolor is false and clicolor_force also false", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(false),
                        clicolor_force: None,
                        manual_override: Some(true),
                        .. ShouldColorize::default()
                    };

                    true == colorize_control.should_colorize()
                });

                ctx.it("should not colorize if manual_override is false, but clicolor is true or clicolor_force is true", || {
                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        clicolor_force: Some(true),
                        manual_override: Some(false),
                        .. ShouldColorize::default()
                    };

                    false == colorize_control.should_colorize()
                })
            });

            ctx.describe("::set_override", |ctx| {
                ctx.it("should exists", || {
                    let mut colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                });

                ctx.it("set the manual_override property", || {
                    let mut colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                    assert_eq!(Some(true), colorize_control.manual_override);
                    colorize_control.set_override(false);
                    assert_eq!(Some(false), colorize_control.manual_override);
                });
            });

            ctx.describe("::unset_override", |ctx| {
                ctx.it("should exists", || {
                    let mut colorize_control = ShouldColorize::default();
                    colorize_control.unset_override();
                });

                ctx.it("unset the manual_override property", || {
                    let mut colorize_control = ShouldColorize::default();
                    colorize_control.set_override(true);
                    colorize_control.unset_override();
                    assert_eq!(None, colorize_control.manual_override);
                });
            });
        });
        runner.add_event_handler(&mut formatter);
        runner.run().unwrap();
    }
}


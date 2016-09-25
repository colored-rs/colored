
use lazy_static::*;
use std::env;
use std::default::Default;

pub fn has_colors() -> bool {

    let clicolor : &str = &env::var("CLICOLOR").unwrap_or(String::new());

    match clicolor {
        "0" => false,
        _ => true
    }
}

pub struct ShouldColorize {
    clicolor: Option<bool>,
    clicolor_force: Option<bool>,
    stdout_is_a_tty: bool,
    manual_override: Option<bool>
}

impl Default for ShouldColorize {
    fn default() -> ShouldColorize {
        ShouldColorize {
            clicolor: None,
            clicolor_force: None,
            stdout_is_a_tty: true,
            manual_override: None
        }
    }
}

impl ShouldColorize {
    fn should_colorize(&self) -> bool {

        match (self.stdout_is_a_tty, self.clicolor_force, self.clicolor) {
            (_    , Some(true),        _)           => true,
            (false, _,                 _)           => false,
            (_,     Some(force_value), _)           => force_value,
            (_,     _,                 Some(value)) => value,
            _                                       => true
        }
    }
}

#[cfg(test)]
mod specs {
    use super::*;
    use rspec::context::{rdescribe};


    #[test]
    fn clicolor_behavior() {

        rdescribe("ShouldColorize::value", |ctx| {

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
            

            ctx.describe("changing stdout_is_a_tty", |ctx| {

                ctx.it("should not colorize when stdout_is_a_tty is false", || {

                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        stdout_is_a_tty: false,
                        .. ShouldColorize::default()
                    };

                    false == colorize_control.should_colorize()
                });

                ctx.it("it should colorize if clicolor_force is true and stdout_is_a_tty false", || {

                    let colorize_control = ShouldColorize {
                        clicolor: Some(true),
                        stdout_is_a_tty: false,
                        clicolor_force: Some(true),
                        .. ShouldColorize::default()
                    };

                    true == colorize_control.should_colorize()
                })
            });
        });
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    pub use std::env;
    pub use std::thread;
    pub use std::time;

    macro_rules! with_env (
        (empty, $body:block)                                => (with_env!(clicolor:"", clicolor_force:"", $body));
        (clicolor: $clicolor:expr, clicolor_force: $clicolor_force:expr, $body:block) => {{

            let old_clicolor = env::var_os("CLICOLOR").unwrap_or("".into());
            let old_clicolor_force = env::var_os("CLICOLOR_FORCE").unwrap_or("".into());

            env::set_var("CLICOLOR", $clicolor);
            assert!(Ok($clicolor.into()) == env::var("CLICOLOR"),
                    "setting the env var `clicolor` failed for some reason. Please re-run the test");

            env::set_var("CLICOLOR_FORCE", $clicolor_force);
            assert!(Ok($clicolor_force.into()) == env::var("CLICOLOR_FORCE"),
                    "setting the env variable `clicolor_force` failed for some reason. Please re-run the test");

            $body;

            env::set_var("CLICOLOR", old_clicolor.clone());
            assert!(Some(old_clicolor) == env::var_os("CLICOLOR"),
                    "setting back the env var `clicolor` failed for some reason. Please re-run the test");

            env::set_var("CLICOLOR_FORCE", old_clicolor_force.clone());
            assert!(Some(old_clicolor_force) == env::var_os("CLICOLOR_FORCE"),
                    "setting back the env var `clicolor_force` failed for some reason. Please re-run the test");
        }};
        (clicolor_force: $clicolor_force:expr, $body:block) => (with_env!(clicolor:"", clicolor_force: $clicolor_force, $body));
        (clicolor: $clicolor:expr, $body:block)             => (with_env!(clicolor: $clicolor, clicolor_force: "", $body));
    );

    #[test]
    fn it_expose_the_current_state_of_colors() {
        has_colors();
    }

    #[test]
    fn it_is_on_by_default() {
        with_env!(empty, {
            assert_eq!(true, has_colors());
        });
    }

    #[test]
    fn it_is_off_when_env_clicolor_is_zero() {
        with_env!(clicolor: "0", {
            assert_eq!(false, has_colors());
        });
    }

    #[test]
    fn it_is_on_when_env_clicolor_is_one_or_anything() {
        with_env!(clicolor: "1", {
            assert_eq!(true, has_colors());
        });
        with_env!(clicolor: "2", {
            assert_eq!(true, has_colors());
        });
        with_env!(clicolor: "a", {
            assert_eq!(true, has_colors());
        });
        with_env!(clicolor: "plop", {
            assert_eq!(true, has_colors());
        });
    }
}

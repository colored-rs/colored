extern crate colored;
use colored::*;

fn main() {
    // this will be yellow if your environment allow it
    println!("{}", "some warning".yellow());
    // now , this will be always yellow
    colored::control::set_override(true);
    println!("{}", "some warning".yellow());
    // now, this will be never yellow
    colored::control::set_override(false);
    println!("{}", "some warning".yellow());
    // let the environment decide again
    colored::control::unset_override();
}

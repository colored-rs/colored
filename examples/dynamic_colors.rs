extern crate colored;
use colored::*;

fn main() {
    // the easy way
    println!("{}", "blue string yo".color("blue"));

    // this will default to white
    println!("{}", "white string".color("zorglub"));

    // the safer way via a Result
    let color_res = "zorglub".parse(); // <- this returns a Result<Color, ()>
    println!("{}", "red string".color(color_res.unwrap_or(Color::Red)));
}

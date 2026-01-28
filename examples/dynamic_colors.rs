extern crate colored;
use colored::{Color, Colorize};

fn main() {
    // the easy way
    let _ = "blue string yo".color("blue");

    // this will default to white
    let _ = "white string".color("zorglub");

    // the safer way via a Result
    let color_res = "zorglub".parse(); // <- this returns a Result<Color, ()>
    let _ = "red string".color(color_res.unwrap_or(Color::Red));
}

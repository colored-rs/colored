extern crate colored;
use colored::Colorize;

fn main() {
    let red_text = "This is Red!".color("red").italic().underline();
    println!("{}", red_text);
    let blue_text = "This is Blue!".color("blue").italic().underline();
    println!("{}", blue_text);
    // This seems inefficient. If only there were a way to change the contents
    // of a ColoredString without having to make another ColoredString,
    // complete with heap allocation and having to re-construct the style?

    let mut text = "This is Red!".color("red").italic().underline();
    println!("{}", text);
    text.push_str(" ...but now it's blue!");
    text = text.blue();
    println!("{}", text);
    // And look at that, we preserved the style AND the allocation!
}

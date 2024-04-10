extern crate afetch_colored;

use afetch_colored::Colorize;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    Err("ERROR".red())?
}

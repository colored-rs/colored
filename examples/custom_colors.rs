use colored::*;
fn main() {
    let my_color = customcolors::CustomColor::new(0, 120, 120);
    println!("{}", "Greetings from Ukraine".custom_color(my_color));
}

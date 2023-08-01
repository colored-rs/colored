use colored::*;
fn main() {
    let no_colors = vec![];
    let one_color = vec![Color::Red];
    let two_colors = vec![Color::Blue, Color::Green];

    println!("{}", "Text defaults to white".gradient(&no_colors));
    println!("{}", "This text is red".gradient(&one_color));
    println!("{}", "Transition from blue to green".gradient(&two_colors));
    println!("{}", "A lot of the colors of the rainbow".rainbow());

    println!(
        "{}",
        "Transition from blue to green".on_gradient(&two_colors)
    );
    println!("{}", "A lot of the colors of the rainbow".on_rainbow());

    //Test edge cases
    println!("{}", "".gradient(&no_colors));
    println!("{}", "a".gradient(&no_colors));
    println!("{}", "b".gradient(&one_color));
    println!("{}", "c".gradient(&two_colors));
    println!("{}", "de".gradient(&two_colors));
    println!(
        "{}",
        "fg".gradient(&vec![Color::Green, Color::Violet, Color::Blue])
    );
}

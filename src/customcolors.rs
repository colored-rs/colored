/// Custom color structure, it will generate a true color in the result
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CustomColor {
    /// Red
    pub r: u8,
    /// Green
    pub g: u8,
    /// Blue
    pub b: u8,
}

/// This only makes custom color creation easier.
impl CustomColor {
    /// Create a new custom color
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn main() {
        let my_color = CustomColor::new(0, 120, 120);
        println!("{}", "Greetings from Ukraine".custom_color(my_color));
    }
}

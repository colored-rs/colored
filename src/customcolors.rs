#[doc = "Custom color structure, it will generate a true color in the result"]
pub struct CustomColor {
    /// Red
    pub r: u8,
    /// Green
    pub g: u8,
    /// Blue
    pub b: u8,
}

#[doc = "This only makes custom color creation easier.\n\n"]
impl CustomColor {
    /// Create a new custom color
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

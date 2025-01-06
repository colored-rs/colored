use rgb::Rgb;

/// Custom color structure, it will generate a true color in the result.
/// You should use the [Rgb] struct instead.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[deprecated]
pub struct CustomColor {
    /// Red
    pub r: u8,
    /// Green
    pub g: u8,
    /// Blue
    pub b: u8,
}

#[allow(deprecated)]
/// This only makes custom color creation easier.
impl CustomColor {
    /// Create a new custom color
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

#[allow(deprecated)]
impl From<CustomColor> for Rgb<u8> {
    fn from(value: CustomColor) -> Self {
        Rgb {
            r: value.r,
            g: value.g,
            b: value.b,
        }
    }
}

#[allow(deprecated)]
impl From<(u8, u8, u8)> for CustomColor {
    fn from((r, g, b): (u8, u8, u8)) -> Self {
        Self::new(r, g, b)
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[cfg_attr(feature = "no-color", ignore)]
    #[allow(deprecated)]
    #[test]
    fn test_custom_colour() {
        let my_color = CustomColor::new(0, 120, 120);
        insta::assert_snapshot!("Greetings from Ukraine".custom_color(my_color));
    }

    #[test]
    #[allow(deprecated)]
    fn from_tuple() {
        let tuple = (1u8, 255u8, 0u8);
        let cc = CustomColor::from(tuple);

        assert_eq!(cc.r, tuple.0);
        assert_eq!(cc.g, tuple.1);
        assert_eq!(cc.b, tuple.2);
    }
}

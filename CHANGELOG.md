# 1.6.0
- FEAT: introduced bright colors. `"hello".bright_blue().on_bright_red();`
- FEAT: introduced strikethrough styling. `"hello".strikethrough();`

# 1.5.3 (September 28, 2017)

- FEAT: derive Copy and Clone for `Color`
- FEAT: derive Clone for `ColoredString`

# 1.5.2 (July 6, 2017)

- FIX: method `Colorize::reversed` has been added. `Colorize::reverse` was a typo, that we will keep
    for compatibility

# 1.5.1 (May 9, 2017)

- Update lazy_static to 0.2.

# 1.5.0 (May 1, 2017)

- FEAT: support for `"hello".color("blue")` (dynamic colors)

# 1.3.2 (Nov 26, 2016)

- FIX: usage of nested ColoredString again, no more style broken mid-line

# 1.3.1 (Oct 14, 2016)

- FIX: usage of ColoredString in a nested way broke the styling mid-line

# 1.3.0 (Jul 31, 2016)

- Provide various options for disabling the coloring in an API-compatible way

# 1.2.0 (Mar 30, 2016)

- Support the different formatting options, like padding and alignment

# 1.1.0 (Mar 15, 2016)

- Respect the CLICOLOR/CLICOLOR\_FORCE behavior. See [this specs](http://bixense.com/clicolors/)

# 1.0.1 (Mar 14, 2016)

- Add a CHANGLOG
- Fix crate dependencies: move `ansi_term` in dev\_dependencies

# 1.0.0 (Mar 13, 2016)

- Initial release

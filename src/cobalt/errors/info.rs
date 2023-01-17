#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ErrorInfo {
    pub message: &'static str,
    pub help: &'static str
}
impl ErrorInfo {
    pub(self) const fn new(message: &'static str, help: &'static str) -> Option<Self> {
        Some(ErrorInfo {message, help})
    }
}
pub static ERR_REGISTRY: &[(u64, &[Option<ErrorInfo>])] = &[
    (20, &[
    /*20*/ ErrorInfo::new("empty character literal", "")
    ]),
    (101, &[
    /*101*/ ErrorInfo::new("invalid character in source file", ""   ),
    /*102*/ ErrorInfo::new("unterminated multiline comment", "Multiline comments must be properly terminated.
# Erroneous code
```
#=
This is a multiline comment with no end
```
# Help
- If you use comments with more than one `=`, make sure you close them properly
- Make sure you properly match nested comments"),
    /*103*/ ErrorInfo::new("function-like @ directives must have closed parameters", "Macros, annotations, and intrinisics in their function-like form must have matched parentheses
# Erroneous code
```
# this is the only way you can get this (unless you do something weird with macros)
@version(major
```
# Help
Check your parentheses"),
    /*104*/ ErrorInfo::new("expected a name after @", "Macros, annotations, and intrinisics must have a name.
# Erroneous code
```
@ # <<< an erroneous annotation
fn main(): i32 = @; # <<< an erroneous intrinsic
```
# Help
There cannot be a space between the `@` and the identifier, so this is also erroneous:
```
@ extern(C) fn puts(str: i8 const*): null;
#^ remove this space
```")]),
    (110, &[ErrorInfo::new("unknown version specification", r#"@version macro, when used with arguments, requires a version type.
Valid options are:
- `major`
- `minor`
- `patch`
- `array`- this expands to an array containing [major, minor, patch]
- no argument or an empty argument expands to a string containing "major.minor.patch"
# Erroneous code
```
let major = @version(majoe);
```
# Help
Check your spelling"#)]),
    (130, &[
    /*130*/ ErrorInfo::new("unterminated character literal", ""),
    /*131*/ ErrorInfo::new("unterminated string literal", ""),
    /*132*/ ErrorInfo::new("unexpected non-hex character in hex escape sequence", ""),
    /*133*/ ErrorInfo::new("invalid Unicode codepoint", ""),
    /*134*/ ErrorInfo::new("unexpected characters in character literal", "")]),
    (313, &[
    /*313*/ ErrorInfo::new("invalid arguments to call", "")]),
    (900, &[
    /*900*/ ErrorInfo::new("const function parameters aren't implemented yet", "")])
];
pub fn lookup(code: u64) -> Option<ErrorInfo> {
    ERR_REGISTRY.iter().rev().skip_while(|(start, _)| start > &code).next().and_then(|(start, arr)| arr.get((code - start) as usize).copied().unwrap_or(None))
}

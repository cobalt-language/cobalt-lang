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
    /*020*/ ErrorInfo::new("empty character literal", ""),
    /*021*/ ErrorInfo::new("externally linked variable has a non-runtime type", "")]),
    (30, &[
    /*030*/ ErrorInfo::new("@static on a global variable does nothing", ""),
    /*031*/ ErrorInfo::new("respecifying @static does nothing", ""),
    /*032*/ ErrorInfo::new("respecifying @extern does nothing", "")]),
    (90, &[
    /*090*/ ErrorInfo::new("value has been moved from and is in an indeterminate state", "")]),
    (101, &[
    /*101*/ ErrorInfo::new("invalid character in source file", ""),
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
    (310, &[
    /*310*/ ErrorInfo::new("operator is not defined for these types", ""),
    /*311*/ ErrorInfo::new("invalid implicit conversion", ""),
    /*312*/ ErrorInfo::new("invalid explicit conversion", ""),
    /*313*/ ErrorInfo::new("invalid arguments to call", ""),
    /*314*/ ErrorInfo::new("function parameter's default value must be constant", ""),
    /*315*/ ErrorInfo::new("no common type available", ""),
    /*316-319*/ None, None, None, None,
    /*320*/ ErrorInfo::new("value does not exist", ""),
    /*321*/ ErrorInfo::new("value is not a module", ""),
    /*322*/ ErrorInfo::new("value is not a variable", ""),
    /*323*/ ErrorInfo::new("redefinition of variable", ""),
    /*324*/ ErrorInfo::new("value cannot be determined at compile-time", "")]),
    (390, &[
    /*390*/ ErrorInfo::new("unknown literal suffix", ""),
    /*391*/ ErrorInfo::new("unknown intrinisc", "")]),
    (410, &[
    /*410*/ ErrorInfo::new("unknown annotation", ""),
    /*411*/ ErrorInfo::new("@static annotation cannot have arguments", ""),
    /*412*/ ErrorInfo::new("@link annotation requires an argument", ""),
    /*413*/ ErrorInfo::new("unknown linkage type for @link", ""),
    /*414*/ ErrorInfo::new("linkage type cannot be respecified on a variable or function definition", ""),
    /*415*/ ErrorInfo::new("@linkas annotation requires an argument", ""),
    /*416*/ ErrorInfo::new("@linkas cannot be respecified", ""),
    /*417*/ ErrorInfo::new("@extern cannot be specified for non-static local variables", ""),
    /*418*/ ErrorInfo::new("@link cannot be specified for non-static local variables", ""),
    /*419*/ ErrorInfo::new("@linkas cannot be specified for non-static local variables", ""),
    /*420*/ ErrorInfo::new("@cconv annotation cannot be respecified", ""),
    /*421*/ ErrorInfo::new("@cconv annotation requires an argument", ""),
    /*422*/ ErrorInfo::new("unknown calling convention for @cconv annotation", "")]),
    (900, &[
    /*900*/ ErrorInfo::new("const function parameters aren't implemented yet", "")])
];
pub fn lookup(code: u64) -> Option<ErrorInfo> {
    ERR_REGISTRY.iter().rev().skip_while(|(start, _)| start > &code).next().and_then(|(start, arr)| arr.get((code - start) as usize).copied().unwrap_or(None))
}

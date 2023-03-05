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
    /*102*/ ErrorInfo::new("unterminated multiline comment", include_str!("help/E0102.md")),
    /*103*/ ErrorInfo::new("function-like @ directives must have closed parameters", include_str!("help/E0103.md")),
    /*104*/ ErrorInfo::new("expected a name after @", include_str!("help/E0104.md"))]),
    (110, &[ErrorInfo::new("unknown version specification", include_str!("help/E0110.md"))]),
    (130, &[
    /*130*/ ErrorInfo::new("unterminated character literal", ""),
    /*131*/ ErrorInfo::new("unterminated string literal", ""),
    /*132*/ ErrorInfo::new("unexpected non-hex character in hex escape sequence", ""),
    /*133*/ ErrorInfo::new("invalid Unicode codepoint", ""),
    /*134*/ ErrorInfo::new("unexpected characters in character literal", "")]),
    (200, &[
    /*200*/ ErrorInfo::new("unexpected top-level token", ""),
    /*201*/ ErrorInfo::new("expected module body", ""),
    /*202*/ ErrorInfo::new("expected semicolon after module assignment", ""),
    /*203*/ ErrorInfo::new("expected a semicolon", "")]),
    (210, &[
    /*210*/ ErrorInfo::new("expected token in identifier", ""),
    /*211*/ ErrorInfo::new("identifier cannot contain consecutive periods", ""),
    /*212*/ ErrorInfo::new("identifier cannot contain consecutive names", ""),
    /*213*/ ErrorInfo::new("unexpected token in type", ""),
    /*214*/ ErrorInfo::new("identifier cannot end in a period", ""),
    /*215*/ ErrorInfo::new("subimport in group import cannot be global", ""),
    /*216*/ ErrorInfo::new("unterminated group import", "")]),
    (230, &[
    /*230*/ ErrorInfo::new("expected type specification or value after variable definition", ""),
    /*231*/ ErrorInfo::new("expected semicolon after variable definition", ""),
    /*232*/ None,
    /*233*/ ErrorInfo::new("variable definition must have a type specification and/or value", ""),
    /*234*/ ErrorInfo::new("expected parameters or assignment after function definition", ""),
    /*235*/ ErrorInfo::new("function declaration requires explicit parameters and return type", ""),
    /*236*/ None,
    /*237*/ None,
    /*238*/ ErrorInfo::new("unexpected end of parameter list", ""),
    /*239*/ ErrorInfo::new("function parameters cannot be global variables", ""),
    /*240*/ ErrorInfo::new("expected function return type", ""),
    /*241*/ ErrorInfo::new("expected ')' or ',' after function parameter", ""),
    /*242*/ ErrorInfo::new("function parameters must have explicit types", ""),
    /*243*/ ErrorInfo::new("function declaration requires an explicit return type", ""),
    /*244*/ ErrorInfo::new("expected function body or semicolon", ""),
    /*245*/ ErrorInfo::new("functions are defined with '='", ""),
    /*246*/ ErrorInfo::new("all parameters after the first default parameter must have defaults", ""),
    /*247-249*/ None, None, None,
    /*250*/ ErrorInfo::new("unmatched '('", ""),
    /*251*/ ErrorInfo::new("unmatched ')'", ""),
    /*252*/ ErrorInfo::new("unmatched '['", ""),
    /*253*/ ErrorInfo::new("unmatched ']'", ""),
    /*254*/ ErrorInfo::new("unmatched '{'", ""),
    /*255*/ ErrorInfo::new("unmatched '}'", ""),
    /*256-260*/ None, None, None, None, None,
    /*261*/ ErrorInfo::new("expected condition for 'if' expression", ""),
    /*262*/ ErrorInfo::new("expected condition for 'while' expression", ""),
    /*263*/ ErrorInfo::new("unexpected 'else'", ""),
    /*264*/ ErrorInfo::new("operator is not a postfix operator", ""),
    /*265*/ ErrorInfo::new("operator is not a prefix operator", ""),
    /*266-269*/ None, None, None, None,
    /*270*/ ErrorInfo::new("unexpected token after literal", ""),
    /*271*/ ErrorInfo::new("unexpected token after variable name", ""),
    /*272*/ ErrorInfo::new("unexpected token after intrinsic", ""),
    /*273*/ ErrorInfo::new("expected an identifier, literal, or intrinisic (should be unreachable)", ""),
    /*274*/ None,
    /*275*/ ErrorInfo::new("module definitions aren't allowed at local scope", ""),
    /*276*/ ErrorInfo::new("local function definitions can't have global names", ""),
    /*277*/ ErrorInfo::new("local variable definitions can't have global names", ""),
    /*278*/ None,
    /*279*/ None,
    /*280*/ ErrorInfo::new("expected a ';' before the next statement", ""),
    /*281*/ ErrorInfo::new("annotations must be used on a variable or function definition", ""),
    /*282*/ ErrorInfo::new("annotations can't be used on a module", ""),
    /*283*/ ErrorInfo::new("annotations can't be used on an import statement", "")]),
    (290, &[
    /*290*/ ErrorInfo::new("expected an expression", ""),
    /*291*/ ErrorInfo::new("expected a type", ""),
    /*292*/ ErrorInfo::new("error when parsing integral type", "")]),
    (310, &[
    /*310*/ ErrorInfo::new("operator is not defined for these types", ""),
    /*311*/ ErrorInfo::new("invalid implicit conversion", ""),
    /*312*/ ErrorInfo::new("invalid explicit conversion", ""),
    /*313*/ ErrorInfo::new("invalid arguments to call", ""),
    /*314*/ ErrorInfo::new("function parameter's default value must be constant", ""),
    /*315*/ ErrorInfo::new("no common type available", ""),
    /*316*/ ErrorInfo::new("bit casts must be done on statically sized types", ""),
    /*317*/ ErrorInfo::new("bit cast target must be same size as source", ""),
    /*318*/ ErrorInfo::new("subscript is not defined for these types", ""),
    /*319*/ ErrorInfo::new("array value types do not match", ""),
    /*320*/ ErrorInfo::new("value does not exist", ""),
    /*321*/ ErrorInfo::new("value is not a module", ""),
    /*322*/ ErrorInfo::new("value is not a variable", ""),
    /*323*/ ErrorInfo::new("redefinition of variable", ""),
    /*324*/ ErrorInfo::new("value cannot be determined at compile-time", ""),
    /*325*/ ErrorInfo::new("redefinition of values in module", ""),
    /*326*/ ErrorInfo::new("value is not a type", ""),
    /*327*/ ErrorInfo::new("variable must not have a const-only type", "")]),
    (390, &[
    /*390*/ ErrorInfo::new("unknown literal suffix", ""),
    /*391*/ ErrorInfo::new("unknown intrinisc", ""),
    /*392*/ ErrorInfo::new("array literal is too long", "")]),
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
    /*422*/ ErrorInfo::new("unknown calling convention for @cconv annotation", ""),
    /*423*/ ErrorInfo::new("respecification of @inline annotation", ""),
    /*424*/ ErrorInfo::new("unknown inlining specification", ""),
    /*425*/ ErrorInfo::new("unknwon argument for @C annotation", ""),
    /*426*/ ErrorInfo::new("@target annotation requires arguments", ""),
    /*427*/ ErrorInfo::new("error in target glob", ""),
    /*428-429*/ None, None,
    /*430*/ ErrorInfo::new("@asm intrinsic requires arguments", ""),
    /*431*/ ErrorInfo::new("@asm intrinsic requires a constraint, delimited by a semicolon, and a body", ""),
    /*432*/ ErrorInfo::new("invalid call to inline assembly", ""),
    /*433*/ ErrorInfo::new("invalid return specification for @asm intrinsic", "")]),
    (900, &[
    /*900*/ ErrorInfo::new("const function parameters aren't implemented yet", "")])
];
pub fn lookup(code: u64) -> Option<ErrorInfo> {
    let (start, slice) = ERR_REGISTRY.iter().rev().skip_while(|(x, _)| x > &code).next()?;
    slice.get((code - start) as usize).copied()?
}

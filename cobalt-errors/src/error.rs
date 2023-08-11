use crate::CobaltFile;
use miette::{Diagnostic, SourceSpan};
use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum CobaltError<'src> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    OtherFile(Box<SourcedCobaltError<'src>>),

    // Operators
    #[error(r#"binary operator "{op}" is not defined for types `{lhs}` and `{rhs}`"#)]
    BinOpNotDefined {
        lhs: Cow<'src, str>,
        rhs: Cow<'src, str>,
        op: &'src str,
        #[label("left type is `{lhs}`")]
        lloc: SourceSpan,
        #[label("right type is `{rhs}`")]
        rloc: SourceSpan,
        #[label]
        oloc: SourceSpan,
    },
    #[error(r#"prefix operator "{op}" is not defined for type `{val}`"#)]
    PreOpNotDefined {
        val: Cow<'src, str>,
        op: Cow<'src, str>,
        #[label("value type is `{val}`")]
        vloc: SourceSpan,
        #[label]
        oloc: SourceSpan,
    },
    #[error(r#"postfix operator "{op}" is not defined for type `{val}`"#)]
    PostOpNotDefined {
        val: Cow<'src, str>,
        op: Cow<'src, str>,
        #[label("value type is `{val}`")]
        vloc: SourceSpan,
        #[label]
        oloc: SourceSpan,
    },
    #[error("cannot subscript value of type `{val}` with `{sub}`")]
    SubscriptNotDefined {
        val: Cow<'src, str>,
        sub: Cow<'src, str>,
        #[label("value type is `{val}`")]
        vloc: SourceSpan,
        #[label("subscript type is `{sub}`")]
        sloc: SourceSpan,
    },
    #[error("value of type `{val}` has no attribute `{attr}`")]
    AttrNotDefined {
        val: Cow<'src, str>,
        attr: Cow<'src, str>,
        #[label("value type is `{val}`")]
        vloc: SourceSpan,
        #[label("attribute is `{attr}`")]
        aloc: SourceSpan,
    },
    #[error("value of type `{val}` cannot be {}plicitly converted to `{ty}`", if *.is_expl {"ex"} else {"im"})]
    InvalidConversion {
        is_expl: bool,
        val: Cow<'src, str>,
        ty: Cow<'src, str>,
        #[label("value type is `{val}`")]
        vloc: Option<SourceSpan>,
        #[label("target type is `{ty}`")]
        tloc: Option<SourceSpan>,
        #[label]
        oloc: SourceSpan,
    },
    #[error("cannot call value of type `{val}` with arguments ({})", .args.join(", "))]
    CannotCallWithArgs {
        val: Cow<'src, str>,
        #[label("function type is `{val}`")]
        loc: SourceSpan,
        args: Vec<Cow<'src, str>>,
        #[label("argument types are ({})", .args.join(", "))]
        aloc: Option<SourceSpan>,
        #[related]
        nargs: Vec<ArgError<'src>>,
    },
    #[error("invalid call to inline assembly")]
    InvalidInlineAsmCall {
        loc: SourceSpan,
        #[related]
        args: Vec<InvalidAsmArg<'src>>,
    },
    #[error("cannot access element {idx} of a tuple with length {len}")]
    TupleIdxOutOfBounds {
        idx: usize,
        len: usize,
        #[label("tuple length is {len}")]
        tloc: SourceSpan,
        #[label("index is {idx}")]
        iloc: SourceSpan,
    },
    #[error("cannot mutate an immutable value")]
    CantMutateImmut {
        #[label("value is of type `{ty}`")]
        vloc: SourceSpan,
        ty: Cow<'src, str>,
        #[label("operator is `{op}`")]
        oloc: Option<SourceSpan>,
        op: Cow<'src, str>,
        #[label("defined as immutable here")]
        floc: SourceSpan,
    },

    // Misc stuff
    #[error("error in glob pattern")]
    GlobPatternError {
        pos: usize,
        msg: Cow<'src, str>,
        #[label("error at byte {pos} of glob: {msg}")]
        loc: SourceSpan,
    },
    #[error("value cannot be determined at compile-time")]
    NotCompileTime {
        #[label]
        loc: SourceSpan,
    },
    #[error("constant functions aren't yet supported")]
    ConstFnsArentSupported {
        #[label]
        loc: SourceSpan,
    },
    #[error("{}", if let Some(p) = param {format!("cannot convert convert self_t ({self_t}) to {p} for self parameter")} else {"function must have a self parameter".to_string()})]
    InvalidSelfParam {
        self_t: Cow<'src, str>,
        param: Option<Cow<'src, str>>,
        #[label]
        loc: SourceSpan,
    },
    #[error(r#"unknown intrinsic "@{name}""#)]
    UnknownIntrinsic {
        name: Cow<'src, str>,
        #[label]
        loc: SourceSpan,
    },
    #[error("couldn't call `@{name}`")]
    InvalidIntrinsicCall {
        name: &'static str,
        #[label]
        loc: SourceSpan,
        #[related]
        errs: Vec<Self>,
    },
    #[error("@sizeof requires all arguments to be types")]
    ExpectedType {
        #[label]
        loc: SourceSpan,
        #[label("argument type is {ty}")]
        aloc: SourceSpan,
        ty: Cow<'src, str>,
    },
    #[error("bit cast target must be the same size as the source")]
    DifferentBitCastSizes {
        #[label]
        loc: SourceSpan,
        from_ty: Cow<'src, str>,
        from_sz: u32,
        #[label("source type is {from_ty}, which has a size of {from_sz} bytes")]
        from_loc: SourceSpan,
        to_ty: Cow<'src, str>,
        to_sz: u32,
        #[label("target type is {to_ty}, which has a size of {to_sz} bytes")]
        to_loc: SourceSpan,
    },
    #[error("bit cast source and target must both be statically sized")]
    UnsizedBitCast {
        #[label]
        loc: SourceSpan,
        from_ty: Cow<'src, str>,
        #[label("source type is {from_ty}")]
        from_loc: SourceSpan,
        to_ty: Cow<'src, str>,
        #[label("source type is {from_ty}")]
        to_loc: SourceSpan,
    },
    #[error(r#"unknown suffix "{suf}" for {lit} literal"#)]
    UnknownLiteralSuffix {
        suf: Cow<'src, str>,
        lit: &'static str, // integer, floating-point, character, or string
        #[label]
        loc: SourceSpan,
    },
    #[error("this array has {len} elements, but the maximum is 4294697295")]
    ArrayTooLong {
        len: usize,
        #[label]
        loc: SourceSpan,
    },
    #[error("elements in array aren't the same type")]
    ArrayElementsDontMatch {
        current: Cow<'src, str>,
        new: Cow<'src, str>,
        #[label("element type is {new}")]
        loc: SourceSpan,
        #[label("element type previously determined to be {current} here")]
        prev: SourceSpan,
    },
    #[error("expected UTF-8 string")]
    NonUtf8String {
        pos: usize,
        #[label("this string should be valid UTF-8, but there was an error at byte {pos}")]
        loc: SourceSpan,
    },
    #[error("parameter type can't be mutable")]
    #[help("try `mut param: T` instead")]
    ParamCantBeMut {
        #[label]
        loc: SourceSpan,
    },
    #[error("return type can't be mutable")]
    ReturnCantBeMut {
        #[label]
        loc: SourceSpan,
    },
    #[error("can't move out of a reference")]
    CantMoveFromReference {
        #[label("`{ty}` has a destructor, so it can't be moved out of references")]
        loc: SourceSpan,
        ty: Cow<'src, str>,
    },
    #[error("cannot move from variable twice")]
    DoubleMove {
        #[label("{name} {} moved here{}", if *.guaranteed {"is"} else {"may be"}, if .prev.is_none() {" in previous iteration"} else {""})]
        loc: SourceSpan,
        #[label("previously moved here")]
        prev: Option<SourceSpan>,
        name: Cow<'src, str>,
        guaranteed: bool,
    },
    #[error("invalid parameters for overloaded operator function")]
    #[help("for `{op}`, the parameters should be ({ex})")]
    InvalidOpParams {
        #[label("found ({})", .found.join(", "))]
        loc: SourceSpan,
        op: &'static str,
        ex: &'static str,
        found: Vec<Cow<'src, str>>,
    },

    // @asm issues
    #[error("invalid creation of inline assembly")]
    #[help("both arguments should be constant strings (i8 const* or i8[] const&)")]
    InvalidInlineAsm2 {
        #[label("first argument type is {type1} ({})", if *.const1 {"constant"} else {"runtime-only"})]
        loc1: SourceSpan,
        type1: Cow<'src, str>,
        const1: bool,
        #[label("second argument type is {type2} ({})", if *.const2 {"constant"} else {"runtime-only"})]
        loc2: SourceSpan,
        type2: Cow<'src, str>,
        const2: bool,
    },
    #[error("invalid creation of inline assembly")]
    #[help("arguments should be a type, then two constant strings (i8 const* or i8[] const&)")]
    InvalidInlineAsm3 {
        #[label("first argument type is {type1} ({})", if *.const1 {"constant"} else {"runtime-only"})]
        loc1: SourceSpan,
        type1: Cow<'src, str>,
        const1: bool,
        #[label("second argument type is {type2} ({})", if *.const2 {"constant"} else {"runtime-only"})]
        loc2: SourceSpan,
        type2: Cow<'src, str>,
        const2: bool,
        #[label("third argument type is {type3} ({})", if *.const3 {"constant"} else {"runtime-only"})]
        loc3: SourceSpan,
        type3: Cow<'src, str>,
        const3: bool,
    },
    #[error("invalid creation of inline assembly")]
    #[help("valid forms are (constraints, body) and (return, constraints, body)")]
    InvalidInlineAsm {
        nargs: usize,
        #[label("expected 2 or 3 arguments, got {nargs}")]
        loc: SourceSpan,
    },

    // @alloca
    #[error("type for @alloca must be runtime-available")]
    NonRuntimeAllocaType {
        ty: Cow<'src, str>,
        #[label("type is {ty}")]
        loc: SourceSpan,
    },
    #[error("all arguments to @alloca (except for an optional type) must be integral")]
    NonIntegralAllocaArg {
        ty: Cow<'src, str>,
        #[label("argument type is {ty}")]
        loc: SourceSpan,
    },
    #[error("@alloca needs at least one argument, but none were given")]
    AllocaNeedsArgs {
        #[label]
        loc: SourceSpan,
    },

    // Annotations
    #[error(r#"unknown annotation "@{name}" for {def} definition"#)]
    UnknownAnnotation {
        name: Cow<'src, str>,
        def: &'static str, // "variable", "constant", "type", or "function"
        #[label]
        loc: SourceSpan,
    },
    #[error("{} argument for @{name} annotation{}", if .expected.is_some() {"invalid"} else {"unexpected"}, .found.as_ref().map_or_else(Default::default, |f| format!(r#": "{f}""#)))]
    InvalidAnnArgument {
        name: &'static str,
        found: Option<Cow<'src, str>>,
        expected: Option<&'static str>,
        #[label("{}{}", .expected.as_ref().map_or_else(|| "no arguments should be given".to_string(), |ex| format!("expected {ex}")), .found.as_ref().map_or_else(Default::default, |f| format!(r#", found "{f}""#)))]
        loc: SourceSpan,
    },
    #[error("@{name} cannot be respecified")]
    RedefAnnArgument {
        name: &'static str,
        #[label]
        loc: SourceSpan,
        #[label("previously defined here")]
        prev: SourceSpan,
    },
    #[error("@{name} can only be specified for global variables")]
    MustBeGlobal {
        name: &'static str,
        #[label]
        loc: SourceSpan,
    },
    #[error("@{name} can only be specified for local variables")]
    MustBeLocal {
        name: &'static str,
        #[label]
        loc: SourceSpan,
    },

    // Variables
    #[error(r#"variable "{name}" cannot be found{}"#, if .container.is_empty() {"".into()} else {format!(r#" in {container} "{module}""#)})]
    VariableDoesNotExist {
        name: Cow<'src, str>,
        module: Cow<'src, str>,
        container: &'static str, // "module" or "type"
        #[label]
        loc: SourceSpan,
    },
    #[error(r#""{name}" has not been initialized, most likely because of a cyclical dependency"#)]
    UninitializedGlobal {
        name: Cow<'src, str>,
        #[label]
        loc: SourceSpan,
    },
    #[error("runtime variable cannot have a const-only type")]
    #[diagnostic(help("consider using `const` instead"))]
    TypeIsConstOnly { ty: Cow<'src, str>, loc: SourceSpan },
    #[error("{name} is not a module")]
    NotAModule {
        name: Cow<'src, str>,
        #[label]
        loc: SourceSpan,
    },
    #[error("{name} has already been defined")]
    RedefVariable {
        name: Cow<'src, str>,
        #[label]
        loc: SourceSpan,
        #[label("previously defined here")]
        prev: Option<SourceSpan>,
    },

    // warnings
    #[error("import statement does not refer to anything")]
    #[diagnostic(severity(warning))]
    UselessImport {
        #[label]
        loc: SourceSpan,
    },
}
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum ArgError<'src> {
    #[error("expected {expected} arguments, found {found}")]
    WrongNumArgs {
        found: usize,
        expected: usize,
        loc: SourceSpan,
    },
    #[error("value of type `{val}` cannot be implicitly converted to `{ty}` in {} argument", ordinal::Ordinal(.n + 1))]
    InvalidArg {
        val: Cow<'src, str>,
        ty: Cow<'src, str>,
        n: usize,
        #[label]
        loc: SourceSpan,
    },
    #[error("{} parameter is const, but the argument is not", ordinal::Ordinal(.n + 1))]
    ArgMustBeConst {
        n: usize,
        #[label]
        loc: SourceSpan,
    },
}
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
#[error("cannot pass argument of type {0} into inline assembly")]
pub struct InvalidAsmArg<'src>(
    pub Cow<'src, str>,
    #[label("{0} is not a valid assembly type")] pub SourceSpan,
);
impl<'src> CobaltError<'src> {
    pub fn with_file(self, file: CobaltFile) -> SourcedCobaltError<'src> {
        if let Self::OtherFile(err) = self {
            *err
        } else {
            SourcedCobaltError { err: self, file }
        }
    }
    pub fn is_err(&self) -> bool {
        self.severity()
            .map_or(true, |s| s == miette::Severity::Error)
    }
}
impl<'src> From<SourcedCobaltError<'src>> for CobaltError<'src> {
    #[inline]
    fn from(err: SourcedCobaltError<'src>) -> Self {
        Self::OtherFile(Box::new(err))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Diagnostic)]
pub struct SourcedCobaltError<'src> {
    err: CobaltError<'src>,
    #[source_code]
    file: CobaltFile,
}
impl std::fmt::Display for SourcedCobaltError<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}
impl std::error::Error for SourcedCobaltError<'_> {}

pub use OpType::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpType {
    Ltr, Rtl,
    Op(&'static str)
}
pub const COBALT_BIN_OPS: &[OpType] = &[
    Op("="), Op("+="), Op("-="), Op("*="), Op("/="), Op("%="), Op("&="), Op("|="), Op("^="), Op("<<="), Op(">>="),  Rtl,
    Op("|?"),                                                                                                       Ltr,
    Op("&?"),                                                                                                       Ltr,
    Op("|"),                                                                                                        Ltr,
    Op("^"),                                                                                                        Ltr,
    Op("&"),                                                                                                        Ltr,
    Op("=="), Op("!="),                                                                                             Ltr,
    Op("<"), Op(">"), Op("<="), Op(">="),                                                                           Ltr,
    Op("<<"), Op(">>"),                                                                                             Ltr,
    Op("+"), Op("-"),                                                                                               Ltr,
    Op("*"), Op("/"), Op("%"),                                                                                      Ltr
];
pub const COBALT_PRE_OPS: &[&'static str] = &["++", "--", "+", "-", "~", "*", "&", "!"];
pub const COBALT_POST_OPS: &[&'static str] = &["?", "!"];

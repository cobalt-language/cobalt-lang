#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ErrorInfo {
    pub message: &'static str,
    pub help: &'static str
}
impl ErrorInfo {

}
pub static ERR_REGISTRY: &[(u64, &[Option<ErrorInfo>])] = &[

];
pub fn lookup(code: u64) -> Option<ErrorInfo> {
    ERR_REGISTRY.iter().rev().skip_while(|(start, _)| start > key).next().map(|(start, arr)| arr.get(code - start).unwrap_or(None))
}

use crate::*;
#[allow(dead_code, unused_variables, unused_mut)]
fn parse_tl<'loc>(toks: &[Token<'loc>], flags: Flags) -> (Vec<Box<dyn AST<'loc>>>, Vec<Error<'loc>>) {
    let mut it = toks.iter();
    let mut outs = vec![];
    let mut errs = vec![];
    while let Some(val) = it.next() {
        match val.data {
            Special(';') => {},
            Keyword(ref x) => match x.as_str() {
                "module" => {},
                "import" => {},
                "fn" => {},
                "cr" => {},
                "let" => {},
                "mut" => {},
                _ => errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)))
            },
            _ => errs.push(Error::new(val.loc.clone(), 201, format!("unexpected top-level token: {:?}", val.data)))
        }
    };
    (outs, i, errs)
}
#[allow(unused_variables)]
pub fn parse(toks: &[Token], flags: Flags) -> (Box<dyn AST>, Vec<Error>) {
    panic!("parser has not been implemented")
}

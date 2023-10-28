use super::*;
use cobalt_errors::SourceSpan;
use std::borrow::Cow;

impl<'src> Parser<'src> {
    /// Advances the cursor past a collection of annotations.
    ///
    /// Going into this function, the current token is assumed to be '@'.
    /// Leaving this function, the current token will be the first token after
    /// the last annotation in the collection.
    pub fn eat_annotations(&mut self) {
        loop {
            match self.current_token {
                Some(Token {
                    kind: TokenKind::At,
                    ..
                }) => {
                    let _ = self.parse_annotation();
                }
                _ => break,
            }
        }
    }

    /// Parses a collection of annotations.
    ///
    /// Going into this function, the current token is assumed to be '@'.
    /// Leaving this function, the current token will be the first token after
    /// the last annotation in the collection.
    pub fn parse_annotations(
        &mut self,
    ) -> (
        Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
        Vec<CobaltError<'src>>,
    ) {
        let mut anns = vec![];
        let mut errs = vec![];

        while matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::At,
                ..
            })
        ) {
            let (ann, ann_errors) = self.parse_annotation();
            errs.extend(ann_errors);
            anns.push(ann);
        }

        (anns, errs)
    }
}

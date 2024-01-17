use super::*;
use cobalt_errors::SourceSpan;
use std::borrow::Cow;

impl<'src> Parser<'src> {
    pub fn parse_annotation(&mut self) -> (Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan) {
        assert!(matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::IntrinOrAnn(..),
                ..
            })
        ));

        let span = self.current_token.unwrap().span;
        let TokenKind::IntrinOrAnn((name_src, option_arg_src, skip)) = self.current_token.unwrap().kind else {
            unreachable!()
        };
        self.cursor.index += skip;
        self.next();

        let name = Cow::Borrowed(name_src);
        let arg = option_arg_src.map(Cow::Borrowed);

        (name, arg, span)
    }

    /// Advances the cursor past a collection of annotations.
    ///
    /// Going into this function, the current token is assumed to be '@'.
    /// Leaving this function, the current token will be the first token after
    /// the last annotation in the collection.
    pub fn eat_annotations(&mut self) {
        while let Some(Token {
            kind: TokenKind::IntrinOrAnn(..),
            ..
        }) = self.current_token
        {
            self.parse_annotation();
        }
    }

    /// Parses a collection of annotations.
    ///
    /// Going into this function, the current token is assumed to be '@'.
    /// Leaving this function, the current token will be the first token after
    /// the last annotation in the collection.
    pub fn parse_annotations(
        &mut self,
    ) -> Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)> {
        let mut anns = vec![];

        while matches!(
            self.current_token,
            Some(Token {
                kind: TokenKind::IntrinOrAnn(..),
                ..
            })
        ) {
            anns.push(self.parse_annotation());
        }

        anns
    }
}

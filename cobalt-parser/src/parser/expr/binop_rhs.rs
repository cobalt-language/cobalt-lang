use cobalt_ast::{ast::*, BoxedAST};
use cobalt_errors::CobaltError;

use crate::{
    lexer::tokens::{BinOpToken, TokenKind},
    parser::Parser,
};

impl<'src> Parser<'src> {
    /// - `lhs_precedence` is the precedence of the left-hand side operator. If it is
    /// greater than the precedence of the right-hand side operator, then the left hand
    /// side can be grouped together and calculated on its own.
    ///
    /// ## Example
    ///
    /// Suppose we are parsing `a + (b + c)`.
    /// - First `a` is parsed as a primary expression.
    /// - Then we see a `+` operator, which is a bin op so we treat `a` as the lhs
    /// and give it a precedence of 0. Then we call this function.
    /// - So entering this function we are looking at the `+` token. In the first
    /// run through the loop, we eat it (since it has precedence greater than 0),
    /// and now we are looking at the `(` token. We parse a primary expression,
    /// which in this case tries to parse a paren expression. Assuming this works,
    /// we are at the end of the input and the next run through the loop will break it.
    /// - So now let's look at the parse paren function.
    /// - After eating the '(', it calls the parse expr so as before we enter this
    /// function, now with the lhs being 'b' with precedence 0 and again pointing to
    /// a `+` token. We eat it, parse the `c` as a primary expression.
    pub(crate) fn parse_binop_rhs(
        &mut self,
        lhs_precedence: u8,
        mut lhs: BoxedAST<'src>,
        mut errors: Vec<CobaltError<'src>>,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        loop {
            if self.current_token.is_none() {
                return (lhs, errors);
            }
            let curr_token_precedence = self.current_token.unwrap().kind.precedence_value();

            // Since non-binary operators have precedence 0, we will return having consumed
            // the last token of the binop expression.
            if curr_token_precedence <= lhs_precedence {
                return (lhs, errors);
            }

            // If we are here, then we have a binary operator.
            let binop_token = self.current_token.unwrap();

            self.next();
            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "expression",
                    found: None,
                    loc: binop_token.span,
                });
                return (Box::new(ErrorAST::new(self.source.len().into())), errors);
            }
            let (mut rhs, rhs_errors) = self.parse_primary_expr();
            if !rhs_errors.is_empty() {
                return (
                    Box::new(ErrorAST::new(self.source.len().into())),
                    rhs_errors,
                );
            }

            // Look ahead at the next binary operator.
            let next_binop_precedence = {
                if self.current_token.is_none() {
                    0
                } else {
                    self.current_token.unwrap().kind.precedence_value()
                }
            };

            if curr_token_precedence < next_binop_precedence {
                (rhs, errors) = self.parse_binop_rhs(curr_token_precedence + 1, rhs, errors);
                if !errors.is_empty() {
                    return (Box::new(ErrorAST::new(self.source.len().into())), errors);
                }
            }

            // Merge rhs and lhs.

            lhs = match binop_token.kind {
                TokenKind::Colon => Box::new(CastAST::new(binop_token.span, lhs, rhs)),
                TokenKind::BinOp(BinOpToken::Colonq) => {
                    Box::new(BitCastAST::new(binop_token.span, lhs, rhs))
                }
                TokenKind::BinOp(tok) => {
                    Box::new(BinOpAST::new(binop_token.span, tok.as_str(), lhs, rhs))
                }
                TokenKind::UnOrBinOp(tok) => {
                    Box::new(BinOpAST::new(binop_token.span, tok.as_str(), lhs, rhs))
                }
                _ => unreachable!(),
            };
        }
    }
}

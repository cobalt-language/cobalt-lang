use std::borrow::Cow;

use cobalt_ast::{ast::*, BoxedAST};
use cobalt_errors::{CobaltError, ParserFound, SourceSpan};

use crate::lexer::tokens::{Delimiter, Keyword, TokenKind, UnOpToken, UnOrBinOpToken};

use super::Parser;

mod binop_rhs;
mod literal;

impl<'src> Parser<'src> {
    /// Parse an expression.
    ///
    /// ```text
    /// expr
    ///    := primary_expr [BINOP primary_expr]*
    /// ```
    pub fn parse_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let lhs = self.parse_primary_expr();

        if let Some(next_tok) = self.current_token {
            if let TokenKind::BinOp(_) = next_tok.kind {
                return self.parse_binop_rhs(0, lhs.0, lhs.1);
            }

            if let TokenKind::UnOrBinOp(_) = next_tok.kind {
                return self.parse_binop_rhs(0, lhs.0, lhs.1);
            }
        }

        lhs
    }

    /// Parse a primary expression. These are basically anything that can be (directly)
    /// on the left or right of a binary operator. For example:
    /// - Consider `a + b`. Both `a` and `b` are primary expressions.
    /// - Consider `a + b + c`. `a`, 'b', and 'c' are primary expressions.
    /// - Consider `(a + b) + c`. `(a + b)` and `c` are primary expressions. Note that
    /// `(a + b)` will be found to have primary expressions `a` and `b` upon being parsed
    /// recursively, but this is not the concern of the top level parsing.
    ///
    /// ```text
    /// primary_expr
    ///    := ident_expr
    ///    := literal
    ///    := paren_expr
    ///    := block_expr
    ///    := prefix_expr
    ///    := postfix_expr
    ///    := if_expr
    ///    := intrinsic
    ///    := fn_call
    ///    := dotted_expr
    ///    := index_expr
    ///    := cast_expr
    /// ```
    pub fn parse_primary_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let initial_span = self.current_token.unwrap().span;
        let mut errors = vec![];
        let mut working_ast: BoxedAST = Box::new(NullAST::new(SourceSpan::from((0, 1))));

        let start = 0;
        let parsed_something = 1 << 0;
        let can_be_dotted = 1 << 1;
        let can_be_indexed = 1 << 2;
        let can_be_postfixed = 1 << 3;
        let mut state = start;

        // ---

        match self.current_token.unwrap().kind {
            TokenKind::Literal(_) => {
                let (parsed_literal, parsed_errors) = self.parse_literal();
                errors.extend(parsed_errors);
                working_ast = parsed_literal;

                state |= parsed_something;
            }
            TokenKind::Ident(_) => {
                if self.check_fn_call() {
                    let (parsed_fn_call, parsed_errors) = self.parse_fn_call();
                    errors.extend(parsed_errors);
                    working_ast = parsed_fn_call;

                    state |= parsed_something;
                    state |= can_be_dotted;
                    state |= can_be_indexed;
                    state |= can_be_postfixed;
                } else {
                    let (parsed_ident, parsed_errors) = self.parse_ident_expr();
                    errors.extend(parsed_errors);
                    working_ast = parsed_ident;

                    state |= parsed_something;
                    state |= can_be_dotted;
                    state |= can_be_indexed;
                    state |= can_be_postfixed;
                }
            }
            TokenKind::OpenDelimiter(Delimiter::Paren) => {
                let (parsed_expr, parsed_errors) = self.parse_paren_expr();
                errors.extend(parsed_errors);
                working_ast = parsed_expr;

                state |= parsed_something;
                state |= can_be_dotted;
                state |= can_be_indexed;
                state |= can_be_postfixed;
            }
            TokenKind::OpenDelimiter(Delimiter::Brace) => {
                let (parsed_expr, parsed_errors) = self.parse_block_expr();
                errors.extend(parsed_errors);
                working_ast = parsed_expr;

                state |= parsed_something;
            }
            TokenKind::UnOp(_) => {
                let (parsed_expr, parsed_errors) = self.parse_prefix_expr();
                errors.extend(parsed_errors);
                working_ast = parsed_expr;

                state |= parsed_something;
                state |= can_be_dotted;
                state |= can_be_indexed;
            }
            TokenKind::UnOrBinOp(_) => {
                let (parsed_expr, parsed_errors) = self.parse_prefix_expr();
                errors.extend(parsed_errors);
                working_ast = parsed_expr;

                state |= parsed_something;
                state |= can_be_dotted;
                state |= can_be_indexed;
            }
            TokenKind::Keyword(kw) => {
                if kw == Keyword::If {
                    let (parsed_expr, parsed_errors) = self.parse_if_expr();
                    errors.extend(parsed_errors);
                    working_ast = parsed_expr;

                    state |= parsed_something;
                }

                if kw == Keyword::Mut {
                    let (parsed_expr, parsed_errors) = self.parse_prefix_expr();
                    errors.extend(parsed_errors);
                    working_ast = parsed_expr;

                    state |= parsed_something;
                }
            }
            TokenKind::At => {
                if self.check_fn_call() {
                    let (parsed_fn_call, parsed_errors) = self.parse_fn_call();
                    errors.extend(parsed_errors);
                    working_ast = parsed_fn_call;

                    state |= parsed_something;
                    state |= can_be_dotted;
                    state |= can_be_indexed;
                    state |= can_be_postfixed;
                } else {
                    let (parsed_expr, parsed_errors) = self.parse_intrinsic();
                    errors.extend(parsed_errors);
                    working_ast = parsed_expr;

                    state |= parsed_something;
                }
            }
            _ => {}
        }

        // ---

        loop {
            if self.current_token.is_none() {
                break;
            }

            if state & can_be_dotted != 0 && self.current_token.unwrap().kind == TokenKind::Dot {
                let (parsed_expr, parsed_errors) = self.parse_dotted_expr(working_ast);
                errors.extend(parsed_errors);
                working_ast = parsed_expr;
                continue;
            }

            if state & can_be_indexed != 0
                && self.current_token.unwrap().kind == TokenKind::OpenDelimiter(Delimiter::Bracket)
            {
                let (parsed_expr, parsed_errors) = self.parse_index_expr(working_ast);
                errors.extend(parsed_errors);
                working_ast = parsed_expr;
                continue;
            }

            if state & can_be_postfixed != 0 && self.check_postfix_expr() {
                let (parsed_expr, parsed_errors) = self.parse_postfix_expr(working_ast);
                errors.extend(parsed_errors);
                working_ast = parsed_expr;
                continue;
            }

            if self.current_token.unwrap().kind == TokenKind::Colon {
                let (parsed_expr, parsed_errors) = self.parse_cast_expr(working_ast);
                errors.extend(parsed_errors);
                working_ast = parsed_expr;
                break;
            }

            break;
        }

        // ---

        if state & parsed_something == 0 {
            errors.push(CobaltError::InvalidThing {
                ex: "primary expression",
                loc: initial_span,
            });
            self.next();
            return (working_ast, errors);
        }

        (working_ast, errors)
    }

    /// ```text
    /// ident_expr := ident ['.' ident]+
    /// ```
    pub(crate) fn parse_ident_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];

        let span = self.current_token.unwrap().span;
        let name = match self.current_token.unwrap().kind {
            TokenKind::Ident(name) => Cow::from(name),
            _ => {
                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    vec![CobaltError::ExpectedFound {
                        ex: "identifier",
                        found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                        loc: span,
                    }],
                );
            }
        };
        let is_global = false;

        self.next();

        // ---

        let mut working_ast: BoxedAST = Box::new(VarGetAST::new(span, name, is_global));

        loop {
            if self.current_token.is_none() {
                break;
            }

            if self.current_token.unwrap().kind != TokenKind::Dot {
                break;
            }

            self.next();

            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: ParserFound::Eof,
                    loc: span,
                });
                return (Box::new(ErrorAST::new(self.source.len().into())), errors);
            }

            if let TokenKind::Ident(name) = self.current_token.unwrap().kind {
                working_ast = Box::new(DotAST::new(
                    working_ast,
                    (Cow::Borrowed(name), self.current_token.unwrap().span),
                ));
            } else {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });

                loop {
                    if self.current_token.is_none() {
                        break;
                    }

                    if self.current_token.unwrap().kind == TokenKind::Semicolon {
                        self.next();
                        break;
                    }

                    self.next();
                }

                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    errors,
                );
            }

            self.next();
        }

        // ---

        (working_ast, errors)
    }

    /// Going into this function, `current_token` is assumed to be a '.'.
    ///
    /// ```text
    /// dotted_expr := primary_expr '.' ident
    /// ```
    ///
    /// - `target` is the thing on the left of the dot.
    pub(crate) fn parse_dotted_expr(
        &mut self,
        target: BoxedAST<'src>,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::Dot);

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        self.next();

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let name = match self.current_token.unwrap().kind {
            TokenKind::Ident(ident) => Cow::from(ident),
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "identifier",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });
                return (Box::new(ErrorAST::new(self.source.len().into())), errors);
            }
        };

        self.next();

        // ---

        (Box::new(DotAST::new(target, (name, span))), errors)
    }

    /// Going into this function, `current_token` is assumed to be a unary operator.
    ///
    /// ```text
    /// prefix_expr := [UNOP | 'mut'] primary_expr
    /// ```
    pub(crate) fn parse_prefix_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        let op = match self.current_token.unwrap().kind {
            TokenKind::UnOrBinOp(op) => match op {
                UnOrBinOpToken::And => "&",
                UnOrBinOpToken::Star => "*",
                UnOrBinOpToken::Add => "+",
                UnOrBinOpToken::Sub => "-",
            },
            TokenKind::UnOp(op) => match op {
                UnOpToken::Not => "!",
                UnOpToken::PlusPlus => "++",
                _ => {
                    errors.push(CobaltError::ExpectedFound {
                        ex: "unary operator",
                        found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                        loc: self.current_token.unwrap().span,
                    });
                    return (
                        Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                        errors,
                    );
                }
            },
            TokenKind::Keyword(Keyword::Mut) => "mut",
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "unary operator",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });
                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    errors,
                );
            }
        };

        // Eat the operator.
        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "primary expression",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (val, val_errors) = self.parse_primary_expr();
        errors.extend(val_errors);

        return (Box::new(PrefixAST::new(span, op, val)), errors);
    }

    /// Going into this function, `current_token` is assumed to be an open paren.
    ///
    /// ```text
    /// paren_expr := '(' expr ')'
    /// ```
    pub(crate) fn parse_paren_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Paren) {
            let found = ParserFound::Str(self.current_token.unwrap().kind.to_string());
            let loc = self.current_token.unwrap().span;
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found,
                loc,
            });
            return (
                Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                errors,
            );
        }

        self.next();
        (expr, errors)
    }

    /// Going into this function, `current_token` is assumed to be a open brace.
    ///
    /// ```text
    /// block_expr
    ///     := '{' [ expr? ';' | decl ]* '}'
    ///     := struct_literal
    /// ```
    pub(crate) fn parse_block_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::OpenDelimiter(Delimiter::Brace));

        if self.check_struct_literal() {
            return self.parse_struct_literal();
        }

        let span_start = self.current_token.unwrap().span.offset();
        let mut span_len = self.current_token.unwrap().span.len();
        let mut vals: Vec<BoxedAST<'src>> = vec![];

        let mut errors: Vec<CobaltError<'src>> = vec![];

        // Eat the opening brace.
        self.next();

        let start = 0;
        let last_was_decl = 1;
        let semicolon_trailing_expr = 2;
        let last_was_expr = 3;
        let mut local_state = start;

        loop {
            if self.current_token.is_none() {
                return (
                    Box::new(ErrorAST::new(self.source.len().into())),
                    vec![CobaltError::ExpectedFound {
                        ex: "'}'",
                        found: ParserFound::Eof,
                        loc: self.current_token.unwrap().span,
                    }],
                );
            }

            span_len += self.current_token.unwrap().span.len();

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Brace) {
                break;
            }

            // If it's just a semicolon that's ok.
            if self.current_token.unwrap().kind == TokenKind::Semicolon {
                self.next();
                if local_state == last_was_expr {
                    local_state = semicolon_trailing_expr;
                }
                continue;
            }

            if let TokenKind::Keyword(kw) = self.current_token.unwrap().kind {
                if kw == Keyword::Let || kw == Keyword::Type || kw == Keyword::Fn {
                    let (decl, decl_errors) = self.parse_decl();
                    errors.extend(decl_errors);
                    vals.push(decl);

                    local_state = last_was_decl;
                    continue;
                }
            }

            let (expr, expr_errors) = self.parse_expr();
            errors.extend(expr_errors);
            vals.push(expr);
            local_state = last_was_expr;
        }

        // Eat the closing brace.
        self.next();

        // If the last val was an expr followed by a semicolon, then making the last val a
        // null ast will indicate that the block should evaluate to null (and not the value
        // of the last expr).
        if local_state == semicolon_trailing_expr || local_state == start {
            vals.push(Box::new(NullAST::new(SourceSpan::from((0, 1)))));
        }

        (
            Box::new(BlockAST::new(
                SourceSpan::from((span_start, span_len)),
                vals,
            )),
            errors,
        )
    }

    /// Going into this function, `current_token` is assumed to be an `if` keyword.
    ///
    /// ```text
    /// if_expr :=
    ///    'if' primary_expr block_expr [ 'else' block_expr ]?
    /// ```
    pub(crate) fn parse_if_expr(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        // Eat the `if`.
        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "primary expression",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (cond, cond_errors) = self.parse_primary_expr();
        errors.extend(cond_errors);

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "block expression",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (if_true, if_true_errors) = self.parse_block_expr();
        errors.extend(if_true_errors);

        // Return if there's no else.

        if self.current_token.is_none() {
            return (
                Box::new(IfAST::new(
                    span,
                    cond,
                    if_true,
                    Box::new(NullAST::new(SourceSpan::from((0, 1)))),
                )),
                errors,
            );
        }

        if let TokenKind::Keyword(kw) = self.current_token.unwrap().kind {
            if kw != Keyword::Else {
                return (
                    Box::new(IfAST::new(
                        span,
                        cond,
                        if_true,
                        Box::new(NullAST::new(SourceSpan::from((0, 1)))),
                    )),
                    errors,
                );
            }
        }

        // Handle the else.
        // We know the current token is an `else` keyword.

        self.next();
        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "block expression",
                found: ParserFound::Eof,
                loc: self.current_token.unwrap().span,
            });
            return (
                Box::new(IfAST::new(
                    span,
                    cond,
                    if_true,
                    Box::new(NullAST::new(SourceSpan::from((0, 1)))),
                )),
                errors,
            );
        }

        let (if_false, if_false_errors) = self.parse_block_expr();
        errors.extend(if_false_errors);

        (Box::new(IfAST::new(span, cond, if_true, if_false)), errors)
    }

    /// Going into this function, `current_token` is assumed to be an `@`.
    /// Instrinsics can be functions; these are parsed in the function call
    /// parsing methods.
    ///
    /// ```text
    /// instinsic := '@' [ident | keyword]
    /// ```
    pub(crate) fn parse_intrinsic(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert_eq!(self.current_token.unwrap().kind, TokenKind::At);

        let span = self.current_token.unwrap().span;
        let mut errors = vec![];
        self.next();

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "name of intrinsic",
                found: ParserFound::Eof,
                loc: self.current_token.unwrap().span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let name: Cow<'_, str> = match self.current_token.unwrap().kind {
            TokenKind::Ident(ident) => Cow::Borrowed(ident),
            TokenKind::Keyword(kw) => Cow::Borrowed(kw.as_str()),
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "name of intrinsic",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });
                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    errors,
                );
            }
        };

        self.next();

        // ---

        (Box::new(IntrinsicAST::new(span, name)), errors)
    }

    pub(crate) fn check_fn_call(&mut self) -> bool {
        if self.current_token.is_none() {
            return false;
        }

        let idx_on_entry = self.cursor.index;

        match self.current_token.unwrap().kind {
            TokenKind::Ident(_) => {
                self.next();
            }
            TokenKind::At => {
                self.parse_intrinsic();
            }
            _ => {
                self.rewind_to_idx(idx_on_entry);
                return false;
            }
        }

        if self.current_token.is_none() {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Paren) {
            self.rewind_to_idx(idx_on_entry);
            return false;
        }

        self.rewind_to_idx(idx_on_entry);
        true
    }

    /// Going into this function, `current_token` is assumed to be an identifier.
    ///
    /// ```text
    /// fn_call := [ident | intrinsic] '(' [ expr [',' expr]*]? ')'
    /// ```
    pub(crate) fn parse_fn_call(&mut self) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut errors = vec![];
        let span = self.current_token.unwrap().span;

        // ---

        let name = match self.current_token.unwrap().kind {
            TokenKind::Ident(s) => {
                self.next();
                Box::new(VarGetAST::new(
                    self.current_token.unwrap().span,
                    Cow::Borrowed(s),
                    false,
                ))
            }
            TokenKind::At => {
                let (intrinsic, intrinsics_errors) = self.parse_intrinsic();
                errors.extend(intrinsics_errors);
                intrinsic
            }
            _ => {
                errors.push(CobaltError::ExpectedFound {
                    ex: "function arg",
                    found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                    loc: self.current_token.unwrap().span,
                });
                return (
                    Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                    errors,
                );
            }
        };

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::OpenDelimiter(Delimiter::Paren) {
            errors.push(CobaltError::ExpectedFound {
                ex: "'('",
                found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                loc: self.current_token.unwrap().span,
            });
            return (
                Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                errors,
            );
        }

        self.next();

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "')'",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
            let cparen_span = self.current_token.unwrap().span;

            self.next();

            return (Box::new(CallAST::new(cparen_span, name, vec![])), errors);
        }

        let start = 0;
        let atleast_one_arg = 1;
        let mut local_state = start;

        let mut args = vec![];
        let cparen_span: SourceSpan;
        loop {
            if self.current_token.is_none() {
                errors.push(CobaltError::ExpectedFound {
                    ex: "')'",
                    found: ParserFound::Eof,
                    loc: span,
                });
                cparen_span = span;
                break;
            }

            if self.current_token.unwrap().kind == TokenKind::CloseDelimiter(Delimiter::Paren) {
                cparen_span = self.current_token.unwrap().span;
                break;
            }

            if local_state == atleast_one_arg {
                if self.current_token.unwrap().kind != TokenKind::Comma {
                    errors.push(CobaltError::ExpectedFound {
                        ex: ",",
                        found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                        loc: self.current_token.unwrap().span,
                    });
                } else {
                    self.next();
                }
            }

            let (arg, arg_errors) = self.parse_expr();
            errors.extend(arg_errors);
            args.push(arg);
            local_state = atleast_one_arg;
        }

        self.next();

        // ---

        (Box::new(CallAST::new(cparen_span, name, args)), errors)
    }

    /// Going into this function, `current_token` is assumed to be an `[`.
    ///
    /// ```text
    /// index_expr := '[' expr ']
    /// ```
    pub(crate) fn parse_index_expr(
        &mut self,
        target: BoxedAST<'src>,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::OpenDelimiter(Delimiter::Bracket));

        let span = self.current_token.unwrap().span;
        let mut errors = vec![];
        self.next();

        // ---

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "]",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        if self.current_token.unwrap().kind != TokenKind::CloseDelimiter(Delimiter::Bracket) {
            errors.push(CobaltError::ExpectedFound {
                ex: "]",
                found: ParserFound::Str(self.current_token.unwrap().kind.to_string()),
                loc: self.current_token.unwrap().span,
            });
            return (
                Box::new(ErrorAST::new(self.current_token.unwrap().span)),
                errors,
            );
        }

        self.next();

        // ---

        (Box::new(SubAST::new(span, target, expr)), errors)
    }

    fn check_postfix_expr(&mut self) -> bool {
        if self.current_token.is_none() {
            return false;
        }

        if let TokenKind::UnOp(unop) = self.current_token.unwrap().kind {
            if unop == UnOpToken::Q || unop == UnOpToken::Not || unop == UnOpToken::PlusPlus {
                return true;
            }
        }

        false
    }

    /// Going into this function, `current_token` is assumed to be the first token after the
    /// target.
    ///
    /// ```text
    /// postfix_expr := primary_expr [ '!' | '?' ]+
    /// ```
    pub(crate) fn parse_postfix_expr(
        &mut self,
        target: BoxedAST<'src>,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());

        let mut working_ast = target;

        loop {
            if self.current_token.is_none() {
                break;
            }

            if let TokenKind::UnOp(unop) = self.current_token.unwrap().kind {
                if unop == UnOpToken::Not {
                    working_ast = Box::new(PostfixAST::new(
                        self.current_token.unwrap().span,
                        "!",
                        working_ast,
                    ));
                    self.next();
                    continue;
                }

                if unop == UnOpToken::Q {
                    working_ast = Box::new(PostfixAST::new(
                        self.current_token.unwrap().span,
                        "?",
                        working_ast,
                    ));
                    self.next();
                    continue;
                }

                if unop == UnOpToken::PlusPlus {
                    working_ast = Box::new(PostfixAST::new(
                        self.current_token.unwrap().span,
                        "++",
                        working_ast,
                    ));
                    self.next();
                    continue;
                }
            }

            break;
        }

        (working_ast, vec![])
    }

    /// - `val` is the thing being casted.
    ///
    /// ```text
    /// cast_expr := primary_expr ':' expr
    /// ```
    pub(crate) fn parse_cast_expr(
        &mut self,
        val: BoxedAST<'src>,
    ) -> (BoxedAST<'src>, Vec<CobaltError<'src>>) {
        assert!(self.current_token.is_some());
        assert!(self.current_token.unwrap().kind == TokenKind::Colon);

        let span = self.current_token.unwrap().span;
        let mut errors = vec![];
        self.next();

        // ---

        if self.current_token.is_none() {
            errors.push(CobaltError::ExpectedFound {
                ex: "identifier",
                found: ParserFound::Eof,
                loc: span,
            });
            return (Box::new(ErrorAST::new(self.source.len().into())), errors);
        }

        let (expr, expr_errors) = self.parse_expr();
        errors.extend(expr_errors);

        // ---

        (Box::new(CastAST::new(span, val, expr)), errors)
    }
}

use crate::*;
use glob::Pattern;
#[derive(Debug, Clone)]
pub struct ModuleAST<'src> {
    loc: SourceSpan,
    pub name: DottedName<'src>,
    pub vals: Vec<BoxedAST<'src>>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
}
impl<'src> AST<'src> for ModuleAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn nodes(&self) -> usize {
        self.vals.iter().map(|x| x.nodes()).sum::<usize>() + 1
    }
    fn varfwd_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            Some(_) => {}
                        }
                    }
                }
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            Some(_) => {}
                        }
                    }
                }
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        ctx.map_vars(|mut v| match v.lookup_mod(&self.name) {
            Ok((m, i, _)) => Box::new(VarMap {
                parent: Some(v),
                symbols: m,
                imports: i,
            }),
            Err(_) => Box::new(VarMap::new(Some(v))),
        });
        let old_scope = ctx.push_scope(&self.name);
        let old_vis = if let Some(v) = vis_spec {
            ctx.export.replace(v)
        } else {
            false
        };
        self.vals.iter().for_each(|val| val.varfwd_prepass(ctx));
        ctx.restore_scope(old_scope);
        if vis_spec.is_some() {
            ctx.export.set(old_vis)
        }
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms, ctx.mangle(&self.name))));
    }
    fn constinit_prepass(&self, ctx: &CompCtx<'src, '_>, needs_another: &mut bool) {
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            Some(_) => {}
                        }
                    }
                }
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            Some(_) => {}
                        }
                    }
                }
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        ctx.map_vars(|mut v| match v.lookup_mod(&self.name) {
            Ok((m, i, _)) => Box::new(VarMap {
                parent: Some(v),
                symbols: m,
                imports: i,
            }),
            Err(_) => Box::new(VarMap::new(Some(v))),
        });
        let old_scope = ctx.push_scope(&self.name);
        let old_vis = if let Some(v) = vis_spec {
            ctx.export.replace(v)
        } else {
            false
        };
        self.vals
            .iter()
            .for_each(|val| val.constinit_prepass(ctx, needs_another));
        ctx.restore_scope(old_scope);
        if vis_spec.is_some() {
            ctx.export.set(old_vis)
        }
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms, ctx.mangle(&self.name))));
    }
    fn fwddef_prepass(&self, ctx: &CompCtx<'src, '_>) {
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, _) in self.annotations.iter() {
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        if let Ok(pat) = Pattern::new(arg) {
                            if target_match != 1 {
                                target_match = u8::from(
                                    negate
                                        ^ pat.matches(
                                            &ctx.module.get_triple().as_str().to_string_lossy(),
                                        ),
                                )
                            }
                        }
                    }
                }
                "export" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(true),
                            Some("false") | Some("0") => vis_spec = Some(false),
                            Some(_) => {}
                        }
                    }
                }
                "private" => {
                    if vis_spec.is_none() {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => vis_spec = Some(false),
                            Some("false") | Some("0") => vis_spec = Some(true),
                            Some(_) => {}
                        }
                    }
                }
                _ => {}
            }
        }
        if target_match == 0 {
            return;
        }
        ctx.map_vars(|mut v| match v.lookup_mod(&self.name) {
            Ok((m, i, _)) => Box::new(VarMap {
                parent: Some(v),
                symbols: m,
                imports: i,
            }),
            Err(_) => Box::new(VarMap::new(Some(v))),
        });
        let old_scope = ctx.push_scope(&self.name);
        let old_vis = if let Some(v) = vis_spec {
            ctx.export.replace(v)
        } else {
            false
        };
        self.vals.iter().for_each(|val| val.fwddef_prepass(ctx));
        ctx.restore_scope(old_scope);
        if vis_spec.is_some() {
            ctx.export.set(old_vis)
        }
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms, ctx.mangle(&self.name))));
    }
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = Vec::<CobaltError>::new();
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        match Pattern::new(arg) {
                            Ok(pat) => {
                                if target_match != 1 {
                                    target_match = u8::from(
                                        negate
                                            ^ pat.matches(
                                                &ctx.module.get_triple().as_str().to_string_lossy(),
                                            ),
                                    )
                                }
                            }
                            Err(err) => errs.push(CobaltError::GlobPatternError {
                                pos: err.pos,
                                msg: err.msg,
                                loc,
                            }),
                        }
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc,
                        });
                    }
                }
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((true, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((false, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "module",
                }),
            }
        }
        if target_match == 0 {
            return (Value::null(), errs);
        }
        ctx.map_vars(|mut v| match v.lookup_mod(&self.name) {
            Ok((m, i, _)) => Box::new(VarMap {
                parent: Some(v),
                symbols: m,
                imports: i,
            }),
            Err(UndefVariable::NotAModule(x)) => {
                errs.push(CobaltError::NotAModule {
                    name: self.name.start(x).to_string(),
                    loc: self.name.ids[x - 1].1,
                });
                Box::new(VarMap::new(Some(v)))
            }
            Err(UndefVariable::DoesNotExist(x)) => {
                errs.push(CobaltError::RedefVariable {
                    name: self.name.start(x).to_string(),
                    loc: self.name.ids[x - 1].1,
                    prev: None,
                });
                Box::new(VarMap::new(Some(v)))
            }
        });
        let old_scope = ctx.push_scope(&self.name);
        let old_vis = if let Some((v, _)) = vis_spec {
            ctx.export.replace(v)
        } else {
            false
        };
        // if ctx.flags.prepass {
        //     self.vals.iter().for_each(|val| val.varfwd_prepass(ctx));
        //     let mut again = true;
        //     while again {
        //         again = false;
        //         self.vals
        //             .iter()
        //             .for_each(|val| val.constinit_prepass(ctx, &mut again));
        //     }
        //     self.vals.iter().for_each(|val| val.fwddef_prepass(ctx));
        // }
        errs.extend(self.vals.iter().flat_map(|val| val.codegen(ctx).1));
        ctx.restore_scope(old_scope);
        if vis_spec.is_some() {
            ctx.export.set(old_vis)
        }
        let syms = ctx.map_split_vars(|v| (v.parent.unwrap(), (v.symbols, v.imports)));
        std::mem::drop(ctx.with_vars(|v| v.insert_mod(&self.name, syms, ctx.mangle(&self.name))));
        (Value::null(), errs)
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        pre: &mut TreePrefix,
        file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "module: {}", self.name)?;
        let mut count = self.vals.len();
        for val in self.vals.iter() {
            print_ast_child(f, pre, &**val, count == 1, file)?;
            count -= 1;
        }
        Ok(())
    }
}
impl<'src> ModuleAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: DottedName<'src>,
        vals: Vec<BoxedAST<'src>>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    ) -> Self {
        ModuleAST {
            loc,
            name,
            vals,
            annotations,
        }
    }
}
#[derive(Debug, Clone)]
pub struct ImportAST<'src> {
    loc: SourceSpan,
    pub name: CompoundDottedName<'src>,
    pub annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
}
impl<'src> ImportAST<'src> {
    pub fn new(
        loc: SourceSpan,
        name: CompoundDottedName<'src>,
        annotations: Vec<(Cow<'src, str>, Option<Cow<'src, str>>, SourceSpan)>,
    ) -> Self {
        ImportAST {
            loc,
            name,
            annotations,
        }
    }
}
impl<'src> AST<'src> for ImportAST<'src> {
    fn loc(&self) -> SourceSpan {
        self.loc
    }
    fn varfwd_prepass(&self, ctx: &CompCtx<'src, '_>) {
        self.codegen(ctx);
    }
    fn constinit_prepass(&self, ctx: &CompCtx<'src, '_>, _needs_another: &mut bool) {
        self.codegen(ctx);
    }
    fn fwddef_prepass(&self, ctx: &CompCtx<'src, '_>) {
        self.codegen(ctx);
    }
    fn codegen<'ctx>(
        &self,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> (Value<'src, 'ctx>, Vec<CobaltError<'src>>) {
        let mut errs = vec![];
        let mut target_match = 2u8;
        let mut vis_spec = None;
        for (ann, arg, loc) in self.annotations.iter() {
            let loc = *loc;
            match &**ann {
                "target" => {
                    if let Some(arg) = arg {
                        let mut arg = &**arg;
                        let negate = if arg.as_bytes().first() == Some(&0x21) {
                            arg = &arg[1..];
                            true
                        } else {
                            false
                        };
                        match Pattern::new(arg) {
                            Ok(pat) => {
                                if target_match != 1 {
                                    target_match = u8::from(
                                        negate
                                            ^ pat.matches(
                                                &ctx.module.get_triple().as_str().to_string_lossy(),
                                            ),
                                    )
                                }
                            }
                            Err(err) => errs.push(CobaltError::GlobPatternError {
                                pos: err.pos,
                                msg: err.msg,
                                loc,
                            }),
                        }
                    } else {
                        errs.push(CobaltError::InvalidAnnArgument {
                            name: "target",
                            found: arg.clone(),
                            expected: Some("target glob"),
                            loc,
                        });
                    }
                }
                "export" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "export",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((true, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((false, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "export",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                "private" => {
                    if let Some((_, prev)) = vis_spec {
                        errs.push(CobaltError::RedefAnnArgument {
                            name: "private",
                            loc,
                            prev,
                        });
                    } else {
                        match arg.as_deref() {
                            None | Some("true") | Some("1") | Some("") => {
                                vis_spec = Some((false, loc))
                            }
                            Some("false") | Some("0") => vis_spec = Some((true, loc)),
                            Some(_) => errs.push(CobaltError::InvalidAnnArgument {
                                name: "private",
                                found: arg.clone(),
                                expected: Some(r#"no argument, "true", or "false""#),
                                loc,
                            }),
                        }
                    }
                }
                _ => errs.push(CobaltError::UnknownAnnotation {
                    loc,
                    name: ann.clone(),
                    def: "import",
                }),
            }
        }
        if target_match == 0 {
            return (Value::null(), errs);
        }
        ctx.with_vars(|v| {
            let vec = v.verify(&self.name);
            errs.extend(
                vec.into_iter()
                    .map(|loc| CobaltError::UselessImport { loc }),
            );
            v.imports.push((
                self.name.clone(),
                vis_spec.map_or(ctx.export.get(), |(v, _)| v),
            ))
        });
        (Value::null(), errs)
    }
    fn print_impl(
        &self,
        f: &mut std::fmt::Formatter,
        _pre: &mut TreePrefix,
        _file: Option<CobaltFile>,
    ) -> std::fmt::Result {
        writeln!(f, "import: {}", self.name)
    }
}

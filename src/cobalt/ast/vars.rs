use crate::*;
use inkwell::values::BasicValueEnum::*;
use std::{cell::Cell, rc::Rc};
pub struct VarDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub global: bool
}
impl AST for VarDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        if self.global {
            if self.val.is_const() {
                let (val, mut errs) = self.val.codegen(ctx);
                match if let Some(v) = val.comp_val {
                    let t = val.data_type.llvm_type(ctx).unwrap();
                    let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                    gv.set_constant(true);
                    gv.set_initializer(&v);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(gv.as_pointer_value())),
                        inter_val: val.inter_val,
                        data_type: val.data_type,
                        good: Rc::new(Cell::new(true))
                    })))
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let mut errs;
                match if let Some(t) = t.llvm_type(ctx) {
                    let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                    gv.set_constant(true);
                    let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                    ctx.context.append_basic_block(f, "entry");
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    if let Some(v) = val.comp_val {
                        ctx.builder.build_store(gv.as_pointer_value(), v);
                        ctx.builder.build_return(None);
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: val.data_type,
                            good: Rc::new(Cell::new(true))
                        })))
                    }
                    else {
                        unsafe {
                            gv.delete();
                            f.as_global_value().delete();
                        }
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                    }
                }
                else {
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
        }
        else {
            todo!("local variables aren't implemented yet")
        }
    }
    fn to_code(&self) -> String {
        format!("let {} = {}", self.name, self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "vardef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl VarDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, global: bool) -> Self {VarDefAST {loc, name, val, global}}
}
pub struct MutDefAST {
    loc: Location,
    pub name: DottedName,
    pub val: Box<dyn AST>,
    pub global: bool
}
impl AST for MutDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {self.val.res_type(ctx)}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        if self.global {
            if self.val.is_const() {
                let (val, mut errs) = self.val.codegen(ctx);
                match if let Some(v) = val.comp_val {
                    let t = val.data_type.llvm_type(ctx).unwrap();
                    let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                    gv.set_constant(false);
                    gv.set_initializer(&v);
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(gv.as_pointer_value())),
                        inter_val: val.inter_val,
                        data_type: val.data_type,
                        good: Rc::new(Cell::new(true))
                    })))
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
            else {
                let t = self.val.res_type(ctx);
                let mut errs;
                match if let Some(t) = t.llvm_type(ctx) {
                    let gv = ctx.module.add_global(t, None, format!("{}", self.name).as_str());
                    gv.set_constant(false);
                    let f = ctx.module.add_function(format!("__internals.init.{}", self.name).as_str(), ctx.context.void_type().fn_type(&[], false), Some(inkwell::module::Linkage::Private));
                    ctx.context.append_basic_block(f, "entry");
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    if let Some(v) = val.comp_val {
                        ctx.builder.build_store(gv.as_pointer_value(), v);
                        ctx.builder.build_return(None);
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                            comp_val: Some(PointerValue(gv.as_pointer_value())),
                            inter_val: val.inter_val,
                            data_type: val.data_type,
                            good: Rc::new(Cell::new(true))
                        })))
                    }
                    else {
                        unsafe {
                            gv.delete();
                            f.as_global_value().delete();
                        }
                        ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                    }
                }
                else {
                    let (val, es) = self.val.codegen(ctx);
                    errs = es;
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(val)))
                } {
                    Ok(x) => (x.as_var().unwrap().clone(), errs),
                    Err(RedefVariable::NotAModule(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::AlreadyExists(x, _)) => {
                        errs.push(Error::new(self.loc.clone(), 321, format!("{} has already been defined", self.name.start(x))));
                        (Variable::error(), errs)
                    },
                    Err(RedefVariable::MergeConflict(_, _)) => panic!("merge conflicts shouldn't be reachable when inserting a variable")
                }
            }
        }
        else {
            todo!("local variables aren't implemented yet")
        }
    }
    fn to_code(&self) -> String {
        format!("mut {} = {}", self.name, self.val.to_code())
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "mutdef: {}", self.name)?;
        print_ast_child(f, pre, &*self.val, true)
    }
}
impl MutDefAST {
    pub fn new(loc: Location, name: DottedName, val: Box<dyn AST>, global: bool) -> Self {MutDefAST {loc, name, val, global}}
}
pub struct VarGetAST {
    loc: Location,
    pub name: DottedName
}
impl VarGetAST {
    pub fn new(loc: Location, name: DottedName) -> Self {VarGetAST {loc, name}}
}
impl AST for VarGetAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        if let Ok(Symbol::Variable(x)) = ctx.with_vars(|v| v.lookup(&self.name)) {x.data_type.clone()}
        else {Type::Null}
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        match ctx.with_vars(|v| v.lookup(&self.name)) {
            Ok(Symbol::Variable(x)) =>
                (x.clone(), if x.good.get() {if !x.data_type.copyable() {x.good.set(false);} vec![]}
                else {vec![Error::new(self.loc.clone(), 90, format!("{} has been moved from and is now in an undefined state", self.name))]}),
            Ok(Symbol::Module(_)) => (Variable::error(), vec![Error::new(self.loc.clone(), 322, format!("{} is not a variable", self.name))]),
            Err(UndefVariable::NotAModule(idx)) => (Variable::error(), vec![Error::new(self.loc.clone(), 320, format!("{} is not a module", self.name.start(idx)))]),
            Err(UndefVariable::DoesNotExist(idx)) => (Variable::error(), vec![Error::new(self.loc.clone(), 323, format!("{} does not exist", self.name.start(idx)))])
        }
    }
    fn to_code(&self) -> String {
        format!("{}", self.name)
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "varget: {}", self.name)
    }
}

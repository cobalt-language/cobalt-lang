use crate::*;
use std::cell::Cell;
use inkwell::types::{BasicType, BasicMetadataTypeEnum, BasicTypeEnum::*};
use inkwell::values::BasicValueEnum::*;
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Normal,
    Mutable,
    Constant
}
pub struct FnDefAST {
    loc: Location,
    pub name: DottedName,
    pub ret: ParsedType,
    pub params: Vec<(String, ParamType, ParsedType, Option<Box<dyn AST>>)>, // parameter, mutable, type, default
    pub body: Box<dyn AST>
}
impl FnDefAST {
    pub fn new(loc: Location, name: DottedName, ret: ParsedType, params: Vec<(String, ParamType, ParsedType, Option<Box<dyn AST>>)>, body: Box<dyn AST>) -> Self {FnDefAST {loc, name, ret, params, body}}
}
impl AST for FnDefAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name)) => {
                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime) => {
                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                Type::Null
            }
        };
        Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name)) => {
                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime) => {
                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect())
    }
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {
        let (ret, mut errs) = self.ret.into_type(ctx);
        let ret = match ret {
            Ok(t) => t,
            Err(IntoTypeError::NotAnInt(name)) => {
                errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                Type::Null
            },
            Err(IntoTypeError::NotCompileTime) => {
                errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                Type::Null
            },
            Err(IntoTypeError::NotAModule(name)) => {
                errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                Type::Null
            },
            Err(IntoTypeError::DoesNotExist(name)) => {
                errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                Type::Null
            }
        };
        let fty = Type::Function(Box::new(ret), self.params.iter().map(|(_, pt, ty, _)| ({
            let (ty, mut es) = ty.into_type(ctx);
            errs.append(&mut es);
            match ty {
                Ok(t) => t,
                Err(IntoTypeError::NotAnInt(name)) => {
                    errs.push(Error::new(self.loc.clone(), 311, format!("cannot convert value of type {name} to u64")));
                    Type::Null
                },
                Err(IntoTypeError::NotCompileTime) => {
                    errs.push(Error::new(self.loc.clone(), 312, format!("array size cannot be determined at compile time")));
                    Type::Null
                },
                Err(IntoTypeError::NotAModule(name)) => {
                    errs.push(Error::new(self.loc.clone(), 320, format!("{name} is not a module")));
                    Type::Null
                },
                Err(IntoTypeError::DoesNotExist(name)) => {
                    errs.push(Error::new(self.loc.clone(), 321, format!("{name} does not exist")));
                    Type::Null
                }
            }
        }, pt == &ParamType::Constant)).collect());
        let old_ip = ctx.builder.get_insert_block();
        let val = if let Type::Function(ref ret, ref params) = fty {
            match if let Some(llt) = ret.llvm_type(ctx) {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = llt.fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(format!("{}", self.name).as_str(), ft, None);
                    ctx.map_vars(|v| Box::new(VarMap::new(Some(v))));
                    {
                        let mut param_count = 0;
                        for (name, (ty, is_const)) in self.params.iter().map(|x| &x.0).zip(params.iter()) {
                            if name.len() == 0 {
                                if !is_const {
                                    param_count += 1;
                                }
                                continue;
                            }
                            if !is_const {
                                let param = f.get_nth_param(param_count).unwrap();
                                param.set_name(name.as_str());
                                ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                    comp_val: Some(param),
                                    inter_val: None,
                                    data_type: ty.clone(),
                                    good: Cell::new(true)
                                }))).map_or((), |x| ());
                                param_count += 1;
                            }
                            else {
                                ctx.with_vars(|v| v.insert(&DottedName::local(name.clone()), Symbol::Variable(Variable {
                                    comp_val: None,
                                    inter_val: None,
                                    data_type: ty.clone(),
                                    good: Cell::new(true)
                                }))).map_or((), |x| ());
                            }
                        }
                    }
                    let entry = ctx.context.append_basic_block(f, "entry");
                    ctx.builder.position_at_end(entry);
                    let (body, mut es) = self.body.codegen(ctx);
                    errs.append(&mut es);
                    ctx.map_vars(|v| v.parent.unwrap());
                    let err = format!("cannot convert value of type {} to {}", body.data_type, *ret);
                    ctx.builder.build_return(Some(&types::utils::impl_convert(body, (&**ret).clone(), ctx).and_then(|v| v.comp_val).unwrap_or_else(|| {
                        errs.push(Error::new(self.loc.clone(), 311, err));
                        llt.const_zero()
                    })));
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
                else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
            }
            else if **ret == Type::Null {
                let mut good = true;
                let ps = params.iter().filter_map(|(x, c)| if *c {None} else {Some(BasicMetadataTypeEnum::from(x.llvm_type(ctx).unwrap_or_else(|| {good = false; IntType(ctx.context.i8_type())})))}).collect::<Vec<_>>();
                if good && !ctx.is_const.get() {
                    let ft = ctx.context.void_type().fn_type(ps.as_slice(), false);
                    let f = ctx.module.add_function(format!("{}", self.name).as_str(), ft, None);
                    let entry = ctx.context.append_basic_block(f, "entry");
                    ctx.builder.position_at_end(entry);
                    let (body, mut es) = self.body.codegen(ctx);
                    errs.append(&mut es);
                    ctx.builder.build_return(None);
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
                else {
                    let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: Some(InterData::Function(FnData {
                            defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                                let old_const = ctx.is_const.replace(true);
                                let (val, mut es) = a.codegen(ctx);
                                let err = format!("cannot convert value of type {} to {t}", val.data_type);
                                let val = types::utils::impl_convert(val, t.clone(), ctx);
                                ctx.is_const.set(old_const);
                                errs.append(&mut es);
                                if let Some(val) = val {
                                    if let Some(val) = val.inter_val {val}
                                    else {
                                        errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                        InterData::Null
                                    }
                                }
                                else {
                                    errs.push(Error::new(a.loc(), 311, err));
                                    InterData::Null
                                }
                            })).collect()
                        })),
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
            }
            else {
                let cloned = params.clone(); // Rust doesn't like me using params in the following closure
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: None,
                    inter_val: Some(InterData::Function(FnData {
                        defaults: self.params.iter().zip(cloned).filter_map(|((_, _, _, d), (t, _))| d.as_ref().map(|a| {
                            let old_const = ctx.is_const.replace(true);
                            let (val, mut es) = a.codegen(ctx);
                            let err = format!("cannot convert value of type {} to {t}", val.data_type);
                            let val = types::utils::impl_convert(val, t.clone(), ctx);
                            ctx.is_const.set(old_const);
                            errs.append(&mut es);
                            if let Some(val) = val {
                                if let Some(val) = val.inter_val {val}
                                else {
                                    errs.push(Error::new(a.loc(), 312, "function parameter's default value must be constant".to_string()));
                                    InterData::Null
                                }
                            }
                            else {
                                errs.push(Error::new(a.loc(), 311, err));
                                InterData::Null
                            }
                        })).collect()
                    })),
                    data_type: fty,
                    good: Cell::new(true)
                })))
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
        } else {panic!("In order for this to be reachable, fty would have to somehow be mutated, which is impossible")}.clone();
        if let Some(bb) = old_ip {ctx.builder.position_at_end(bb);}
        else {ctx.builder.clear_insertion_position();}
        val
    }
    fn to_code(&self) -> String {
        let mut out = format!("fn {}(", self.name);
        let mut len = self.params.len();
        for (param, param_ty, ty, default) in self.params.iter() {
            out += match param_ty {
                ParamType::Normal => "",
                ParamType::Mutable => "mut ",
                ParamType::Constant => "const "
            };
            out += format!("{}: {}", param, ty).as_str();
            if let Some(val) = default {
                out += format!(" = {}", val.to_code()).as_str();
            }
            if len > 1 {
                out += ", ";
            }
            len -= 1;
        }
        out + format!("): {} = {}", self.ret, self.body.to_code()).as_str()
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        write!(f, "function: {}(", self.name)?;
        let mut len = self.params.len(); 
        for (param, param_ty, ty, default) in self.params.iter() {
            write!(f, "{}", match param_ty {
                ParamType::Normal => "",
                ParamType::Mutable => "mut ",
                ParamType::Constant => "const "
            })?;
            write!(f, "{}: {}", param, ty)?;
            if let Some(val) = default {
                write!(f, " = {}", val.to_code())?;
            }
            if len > 1 {
                write!(f, ", ")?;
            }
            len -= 1;
        }
        writeln!(f, "): {}", self.ret)?;
        print_ast_child(f, pre, &*self.body, true)
    }
}
pub struct CallAST {
    loc: Location,
    pub target: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>
}
impl CallAST {
    pub fn new(loc: Location, target: Box<dyn AST>, args: Vec<Box<dyn AST>>) -> Self {CallAST {loc, target, args}}
}
impl AST for CallAST {
    fn loc(&self) -> Location {self.loc.clone()}
    fn res_type<'ctx>(&self, ctx: &CompCtx<'ctx>) -> Type {todo!("function calls haven't been implemented")}
    fn codegen<'ctx>(&self, ctx: &CompCtx<'ctx>) -> (Variable<'ctx>, Vec<Error>) {todo!("function calls haven't been implemented")}
    fn to_code(&self) -> String {
        let mut out = format!("{}(", self.target.to_code());
        let mut count = self.args.len();
        for arg in self.args.iter() {
            out += arg.to_code().as_str();
            if count > 1 {
                out += ", ";
            }
            count -= 1;
        }
        out + ")"
    }
    fn print_impl(&self, f: &mut std::fmt::Formatter, pre: &mut TreePrefix) -> std::fmt::Result {
        writeln!(f, "call")?;
        let mut count = self.args.len();
        print_ast_child(f, pre, &*self.target, count == 0)?;
        for arg in self.args.iter() {
            print_ast_child(f, pre, &**arg, count <= 1)?;
            count -= 1;
        }
        Ok(())
    }
}

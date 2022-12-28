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
                    let entry = ctx.context.append_basic_block(f, "entry");
                    ctx.builder.position_at_end(entry);
                    let (body, mut es) = self.body.codegen(ctx);
                    errs.append(&mut es);
                    let err = format!("cannot convert value of type {} to {}", body.data_type, *ret);
                    ctx.builder.build_return(Some(&types::utils::impl_convert(body, (&**ret).clone(), ctx).and_then(|v| v.comp_val).unwrap_or_else(|| {
                        errs.push(Error::new(self.loc.clone(), 311, err));
                        llt.const_zero()
                    })));
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: None, // TODO: constant functions
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: None, // TODO: constant functions
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
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: Some(PointerValue(f.as_global_value().as_pointer_value())),
                        inter_val: None, // TODO: constant functions
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
                else {
                    ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                        comp_val: None,
                        inter_val: None, // TODO: constant functions
                        data_type: fty,
                        good: Cell::new(true)
                    })))
                }
            }
            else {
                ctx.with_vars(|v| v.insert(&self.name, Symbol::Variable(Variable {
                    comp_val: None,
                    inter_val: None, // TODO: constant functions
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

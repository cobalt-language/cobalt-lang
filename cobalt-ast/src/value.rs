use crate::*;
use hashbrown::HashMap;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum::*};
use inkwell::values::{BasicValueEnum, PointerValue};
use std::cell::Cell;
use std::io::{self, BufRead, Read, Write};
use std::rc::Rc;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodType {
    Static,
    Method,
    Getter,
}
#[derive(Debug, Clone)]
pub struct FnData<'src, 'ctx> {
    pub defaults: Vec<InterData<'src, 'ctx>>,
    pub cconv: u32,
    pub mt: MethodType,
}

/// Used for compile-time constants.
#[derive(Debug, Clone)]
pub enum InterData<'src, 'ctx> {
    Null,
    Int(i128),
    Float(f64),
    /// Used for tuples, structs, arrays, and bound methods.
    Array(Vec<Self>),
    /// Used for default values of function parameters.
    Function(FnData<'src, 'ctx>),
    InlineAsm(String, String),
    Type(TypeRef),
    Module(
        HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        Vec<(CompoundDottedName<'src>, bool)>,
        String,
    ),
}
impl<'src, 'ctx> InterData<'src, 'ctx> {
    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        match self {
            InterData::Null => out.write_all(&[1]),
            InterData::Int(v) => out
                .write_all(&[2])
                .and_then(|_| out.write_all(&v.to_be_bytes())),
            InterData::Float(v) => out
                .write_all(&[3])
                .and_then(|_| out.write_all(&v.to_be_bytes())),
            InterData::Array(v) => {
                out.write_all(&[5])?;
                out.write_all(&(v.len() as u32).to_be_bytes())?; // length
                for val in v.iter() {
                    val.save(out)?;
                } // InterData is self-puncatuating
                Ok(())
            }
            InterData::Function(v) => {
                // serialized the same as InterData::Array
                out.write_all(&[6])?;
                out.write_all(&(v.defaults.len() as u32).to_be_bytes())?;
                for val in v.defaults.iter() {
                    val.save(out)?;
                }
                out.write_all(&v.cconv.to_be_bytes())?;
                out.write_all(std::slice::from_ref(&match v.mt {
                    MethodType::Method => 1,
                    MethodType::Static => 2,
                    MethodType::Getter => 3,
                }))
            }
            InterData::InlineAsm(c, b) => {
                out.write_all(&[7])?;
                out.write_all(c.as_bytes())?;
                out.write_all(&[0])?;
                out.write_all(b.as_bytes())?;
                out.write_all(&[0])
            }
            InterData::Type(t) => {
                out.write_all(&[8])?;
                t.save(out)
            }
            InterData::Module(v, i, n) => {
                out.write_all(&[9])?;
                out.write_all(n.as_bytes())?;
                out.write_all(&[0])?;
                for (name, sym) in v.iter() {
                    if sym.1.export {
                        out.write_all(name.as_bytes())?; // name, null-terminated
                        out.write_all(&[0])?;
                        sym.save(out)?;
                    }
                }
                out.write_all(&[0])?; // null terminator for symbol list
                for import in i
                    .iter()
                    .filter_map(|(s, b)| if *b { Some(s) } else { None })
                {
                    import.save(out)?;
                }
                out.write_all(&[0])
            }
        }
    }
    pub fn load<R: Read + BufRead>(
        buf: &mut R,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> io::Result<Option<Self>> {
        let mut c = 0u8;
        buf.read_exact(std::slice::from_mut(&mut c))?;
        Ok(match c {
            0 => None,
            1 => Some(InterData::Null),
            2 => {
                let mut bytes = [0; 16];
                buf.read_exact(&mut bytes)?;
                Some(InterData::Int(i128::from_be_bytes(bytes)))
            }
            3 => {
                let mut bytes = [0; 8];
                buf.read_exact(&mut bytes)?;
                Some(InterData::Float(f64::from_be_bytes(bytes)))
            }
            5 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                let len = u32::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    vec.push(
                        Self::load(buf, ctx)?.expect(
                            "# of unwrapped array elements doesn't match the prefixed count",
                        ),
                    )
                }
                Some(InterData::Array(vec))
            }
            6 => {
                let mut bytes = [0; 4];
                buf.read_exact(&mut bytes)?;
                let len = u32::from_be_bytes(bytes);
                let mut vec = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    vec.push(Self::load(buf, ctx)?.expect(
                        "# of unwrapped default parameters doesn't match the prefixed count",
                    ))
                }
                buf.read_exact(&mut bytes)?;
                let mut c = 0u8;
                buf.read_exact(std::slice::from_mut(&mut c))?;
                let mt = match c {
                    1 => MethodType::Method,
                    2 => MethodType::Static,
                    3 => MethodType::Getter,
                    x => panic!("Expected 1, 2, or 3 for method type, got {x}"),
                };
                Some(InterData::Function(FnData {
                    defaults: vec,
                    cconv: u32::from_be_bytes(bytes),
                    mt,
                }))
            }
            7 => {
                let mut constraint = Vec::new();
                let mut body = Vec::new();
                buf.read_until(0, &mut constraint)?;
                buf.read_until(0, &mut body)?;
                Some(InterData::InlineAsm(
                    String::from_utf8(constraint)
                        .expect("Inline assmebly constraint should be valid UTF-8"),
                    String::from_utf8(body).expect("Inline assembly should be valid UTF-8"),
                ))
            }
            8 => Some(InterData::Type(types::load_type(buf)?)),
            9 => {
                let mut out = HashMap::new();
                let mut imports = vec![];
                let mut vec = Vec::new();
                buf.read_until(0, &mut vec)?;
                if vec.last() == Some(&0) {
                    vec.pop();
                }
                loop {
                    let mut name = vec![];
                    buf.read_until(0, &mut name)?;
                    if name.last() == Some(&0) {
                        name.pop();
                    }
                    if name.is_empty() {
                        break;
                    }
                    out.insert(
                        String::from_utf8(name)
                            .expect("Cobalt symbols should be valid UTF-8")
                            .into(),
                        Symbol::load(buf, ctx)?,
                    );
                }
                while let Some(val) = CompoundDottedName::load(buf)? {
                    imports.push((val, false));
                }
                Some(InterData::Module(
                    out,
                    imports,
                    String::from_utf8(vec).expect("Module names should be valid UTF-8"),
                ))
            }
            x => panic!("read interpreted data type expecting number in 1..=9, got {x}"),
        })
    }
}
#[derive(Debug, Clone)]
pub struct Value<'src, 'ctx> {
    pub loc: SourceSpan,
    pub comp_val: Option<BasicValueEnum<'ctx>>,
    pub inter_val: Option<InterData<'src, 'ctx>>,
    pub data_type: TypeRef,
    pub address: Rc<Cell<Option<PointerValue<'ctx>>>>,
    pub name: Option<(Cow<'src, str>, usize)>,
    pub frozen: Option<SourceSpan>,
}
impl<'src, 'ctx> Value<'src, 'ctx> {
    pub fn error() -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: None,
            data_type: types::Error::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn null() -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: None,
            data_type: types::Null::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn new(
        comp_val: Option<BasicValueEnum<'ctx>>,
        inter_val: Option<InterData<'src, 'ctx>>,
        data_type: TypeRef,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val,
            inter_val,
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn with_addr(
        comp_val: Option<BasicValueEnum<'ctx>>,
        inter_val: Option<InterData<'src, 'ctx>>,
        data_type: TypeRef,
        addr: PointerValue<'ctx>,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val,
            inter_val,
            data_type,
            address: Rc::new(Cell::new(Some(addr))),
            name: None,
            frozen: None,
        }
    }
    pub fn compiled(comp_val: BasicValueEnum<'ctx>, data_type: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: Some(comp_val),
            inter_val: None,
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn interpreted(
        comp_val: BasicValueEnum<'ctx>,
        inter_val: InterData<'src, 'ctx>,
        data_type: TypeRef,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: Some(comp_val),
            inter_val: Some(inter_val),
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn metaval(inter_val: InterData<'src, 'ctx>, data_type: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(inter_val),
            data_type,
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn make_type(type_: TypeRef) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Type(type_)),
            data_type: types::TypeData::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn empty_mod(name: String) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Module(HashMap::new(), vec![], name)),
            data_type: types::Module::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }
    pub fn make_mod(
        syms: HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        imps: Vec<(CompoundDottedName<'src>, bool)>,
        name: String,
    ) -> Self {
        Value {
            loc: unreachable_span(),
            comp_val: None,
            inter_val: Some(InterData::Module(syms, imps, name)),
            data_type: types::Module::new(),
            address: Rc::default(),
            name: None,
            frozen: None,
        }
    }

    pub fn addr(&self, ctx: &CompCtx<'src, 'ctx>) -> Option<PointerValue<'ctx>> {
        if self.data_type.size() == SizeType::Static(0) {
            Some(ctx.null_type.ptr_type(Default::default()).const_null())
        } else {
            self.address.get().or_else(|| {
                let ctv = self.value(ctx)?;
                let alloca = ctx.builder.build_alloca(ctv.get_type(), "");
                ctx.builder.build_store(alloca, ctv);
                self.address.set(Some(alloca));
                Some(alloca)
            })
        }
    }
    pub fn freeze(self, loc: SourceSpan) -> Value<'src, 'ctx> {
        Value {
            frozen: Some(loc),
            ..self
        }
    }
    pub fn with_loc(self, loc: SourceSpan) -> Value<'src, 'ctx> {
        Value { loc, ..self }
    }

    pub fn value(&self, ctx: &CompCtx<'src, 'ctx>) -> Option<BasicValueEnum<'ctx>> {
        self.comp_val.or_else(|| {
            self.inter_val
                .as_ref()
                .and_then(|v| self.data_type.compiled(v, ctx))
        })
    }

    pub fn ins_dtor(&self, ctx: &CompCtx<'src, 'ctx>) {
        self.data_type.ins_dtor(self, ctx)
    }

    pub fn as_type(&self) -> Option<TypeRef> {
        if let (types::TypeData::KIND, Some(InterData::Type(t))) =
            (self.data_type.kind(), &self.inter_val)
        {
            Some(*t)
        } else {
            None
        }
    }
    pub fn into_type(self, ctx: &CompCtx<'src, 'ctx>) -> Result<TypeRef, CobaltError<'src>> {
        let loc = self.loc;
        self.impl_convert((types::TypeData::new(), Some(loc)), ctx)
            .map(|v| v.as_type().unwrap_or(types::Error::new()))
    }
    pub fn into_mod(
        self,
    ) -> Option<(
        HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        Vec<(CompoundDottedName<'src>, bool)>,
        String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }
    pub fn as_mod(
        &self,
    ) -> Option<(
        &HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &Vec<(CompoundDottedName<'src>, bool)>,
        &String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), &self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }
    pub fn as_mod_mut(
        &mut self,
    ) -> Option<(
        &mut HashMap<Cow<'src, str>, Symbol<'src, 'ctx>>,
        &mut Vec<(CompoundDottedName<'src>, bool)>,
        &mut String,
    )> {
        if let (types::Module::KIND, Some(InterData::Module(s, m, n))) =
            (self.data_type.kind(), &mut self.inter_val)
        {
            Some((s, m, n))
        } else {
            None
        }
    }

    pub fn decay(self, ctx: &CompCtx<'src, 'ctx>) -> Value<'src, 'ctx> {
        let ty = self.data_type.decay();
        let name = self.name.clone();
        let loc = self.loc;
        Value {
            name,
            loc,
            ..self.impl_convert((ty, None), ctx).unwrap()
        }
    }
    pub fn attr(
        self,
        attr: (Cow<'src, str>, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.attr(self, attr, ctx)
    }
    pub fn impl_convert(
        self,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.data_type == target.0 {
            Ok(self)
        } else if self.data_type._can_iconv_to(target.0, ctx) {
            self.data_type._iconv_to(self, target, ctx)
        } else if target.0._can_iconv_from(self.data_type, ctx) {
            target.0._iconv_from(self, target.1, ctx)
        } else {
            Err(cant_iconv(&self, target.0, target.1))
        }
    }
    pub fn expl_convert(
        self,
        target: (TypeRef, Option<SourceSpan>),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self.data_type == target.0 {
            Ok(self)
        } else if self.data_type._can_econv_to(target.0, ctx) {
            self.data_type._econv_to(self, target, ctx)
        } else if target.0._can_econv_from(self.data_type, ctx) {
            target.0._econv_from(self, target.1, ctx)
        } else {
            let err = cant_econv(&self, target.0, target.1);
            self.impl_convert(target, ctx).map_err(|_| err)
        }
    }
    pub fn pre_op(
        self,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.pre_op(self, op.0, op.1, ctx, true)
    }
    pub fn post_op(
        self,
        op: (&'static str, SourceSpan),
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.post_op(self, op.0, op.1, ctx, true)
    }
    pub fn bin_op(
        self,
        op: (&'static str, SourceSpan),
        other: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        if self
            .data_type
            ._has_bin_lhs(other.data_type, op.0, ctx, true, true)
        {
            self.data_type._bin_lhs(self, other, op, ctx, true, true)
        } else if other
            .data_type
            ._has_bin_rhs(self.data_type, op.0, ctx, true, true)
        {
            other.data_type._bin_rhs(self, other, op, ctx, true, true)
        } else {
            Err(invalid_binop(&self, &other, op.0, op.1))
        }
    }
    pub fn call(
        self,
        cparen: Option<SourceSpan>,
        args: Vec<Value<'src, 'ctx>>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.call(self, cparen, args, ctx)
    }
    pub fn subscript(
        self,
        other: Value<'src, 'ctx>,
        ctx: &CompCtx<'src, 'ctx>,
    ) -> Result<Value<'src, 'ctx>, CobaltError<'src>> {
        self.data_type.subscript(self, other, ctx)
    }

    pub fn save<W: Write>(&self, out: &mut W) -> io::Result<()> {
        out.write_all(
            self.comp_val
                .as_ref()
                .map(|v| v.into_pointer_value().get_name().to_bytes().to_owned())
                .unwrap_or_else(Vec::new)
                .as_slice(),
        )?; // LLVM symbol name, null-terminated
        out.write_all(&[0])?;
        if let Some(v) = self.inter_val.as_ref() {
            v.save(out)?
        } else {
            out.write_all(&[0])?
        } // Interpreted value, self-punctuating
        types::save_type(out, self.data_type) // Type
    }
    pub fn load<R: Read + BufRead>(buf: &mut R, ctx: &CompCtx<'src, 'ctx>) -> io::Result<Self> {
        let mut var = Value::error();
        let mut name = vec![];
        buf.read_until(0, &mut name)?;
        if name.last() == Some(&0) {
            name.pop();
        }
        var.inter_val = InterData::load(buf, ctx)?;
        var.data_type = types::load_type(buf)?;
        if !name.is_empty() {
            use inkwell::module::Linkage::DLLImport;
            if let Some(ty) = var.data_type.downcast::<types::Function>() {
                let mut good = true;
                let ps = ty
                    .params()
                    .iter()
                    .filter_map(|(x, c)| {
                        if *c {
                            None
                        } else {
                            Some(BasicMetadataTypeEnum::from(
                                x.llvm_type(ctx).unwrap_or_else(|| {
                                    good = false;
                                    IntType(ctx.context.i8_type())
                                }),
                            ))
                        }
                    })
                    .collect::<Vec<_>>();
                if good {
                    if let Some(llt) = ty.ret().llvm_type(ctx) {
                        let ft = llt.fn_type(&ps, false);
                        let fv = ctx.module.add_function(
                            std::str::from_utf8(&name)
                                .expect("LLVM function names should be valid UTF-8"),
                            ft,
                            None,
                        );
                        if let Some(InterData::Function(FnData { cconv, .. })) = var.inter_val {
                            fv.set_call_conventions(cconv)
                        }
                        let gv = fv.as_global_value();
                        gv.set_linkage(DLLImport);
                        var.comp_val = Some(gv.as_pointer_value().into());
                    } else if ty.ret().size() == SizeType::Static(0) {
                        let ft = ctx.context.void_type().fn_type(&ps, false);
                        let fv = ctx.module.add_function(
                            std::str::from_utf8(&name)
                                .expect("LLVM function names should be valid UTF-8"),
                            ft,
                            None,
                        );
                        if let Some(InterData::Function(FnData { cconv, .. })) = var.inter_val {
                            fv.set_call_conventions(cconv)
                        }
                        let gv = fv.as_global_value();
                        gv.set_linkage(DLLImport);
                        var.comp_val = Some(gv.as_pointer_value().into());
                    }
                }
            } else if let Some(t) = var
                .data_type
                .downcast::<types::Reference>()
                .and_then(|t| t.llvm_type(ctx))
            {
                let gv = ctx.module.add_global(
                    t,
                    None,
                    std::str::from_utf8(&name).expect("LLVM variable names should be valid UTF-8"),
                ); // maybe do something with linkage/call convention?
                gv.set_linkage(DLLImport);
                var.comp_val = Some(gv.as_pointer_value().into());
            }
        }
        Ok(var)
    }
}

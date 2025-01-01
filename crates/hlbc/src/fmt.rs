//! Formatting code for displaying bytecode. Formatting is handled through the [BytecodeFmt] trait which permit easy
//! extensibility and re-usability across formatting implementations.
//!
//! - [DebugFmt]: based on the [Debug] impl.
//! - [DisplayFmt]: based on the [Display] impl. This formatting can't access the [Bytecode] context and is limited.
//! - [EnhancedFmt]: Advanced formatter for showing the bytecode with the most help for the reader.

use std::fmt::{Debug, Display, Formatter, Result};
use std::iter::repeat;

pub use fmtools::fmt;

use crate::opcodes::Opcode;
use crate::types::{
    FunPtr, Function, Native, RefEnumConstruct, RefField, RefFloat, RefGlobal, RefInt, RefString,
    RefType, Reg, Type, TypeFun, TypeObj,
};
use crate::Resolve;
use crate::{Bytecode, RefFun};

//region Display impls

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "reg{}", self.0)
    }
}

impl Display for RefInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for RefFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for RefString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for RefType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // We can already know the type for some of them
        // We prefer to show a name instead of @number
        if self.is_known() {
            Display::fmt(&self.to_known(), f)
        } else {
            write!(f, "@{}", self.0)
        }
    }
}

impl Display for RefGlobal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for RefField {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<field{}>", self.0)
    }
}

impl Display for RefEnumConstruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<construct{}>", self.0)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use crate::types::Type::*;
        write!(
            f,
            "{}",
            fmtools::fmt! {
                match self {
                    Void => "void",
                    UI8 => "i8",
                    UI16 => "i16",
                    I32 => "i32",
                    I64 => "i64",
                    F32 => "f32",
                    F64 => "f64",
                    Bool => "bool",
                    Bytes => "bytes",
                    Dyn => "dynamic",
                    Fun(_) => "<fun>",
                    Obj(_) => "<obj>",
                    Array => "array",
                    Type => "type",
                    Ref(reftype) =>"ref<"{ reftype }">",
                    Virtual { .. } => "<virtual>",
                    DynObj => "dynobj",
                    Abstract { .. } => "<abstract>",
                    Enum { .. } => "<enum>",
                    Null(reftype) => "null<"{ reftype }">",
                    Method(_) => "<method>",
                    Struct(_) => "<struct>",
                    Packed(reftype) => "packed<"{ reftype }">",
                }
            }
        )
    }
}

impl Display for TypeFun {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        fmtools::write!(f,
            "("{ fmtools::join(", ", repeat("...").take(self.args.len())) }") -> (...)"
        )
    }
}

impl Display for RefFun {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for Native {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<native fn>")?;
        Display::fmt(&self.findex, f)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "<fn>")?;
        Display::fmt(&self.findex, f)
    }
}

//endregion

/*trait BcVisitor {
    type Input;
    type Output;

    fn visit_reg(&mut self, i: Self::Input, v: Reg) -> Self::Output;
}

struct DebugVisitor;

impl BcVisitor for DebugVisitor {
    type Input = ();
    type Output = impl Display;

    fn visit_reg(&mut self, i: Self::Input, v: Reg) -> Self::Output {
        fmtools::fmt(move |f| Debug::fmt(&v, f))
    }
}*/

#[allow(unused_variables)]
pub trait BytecodeFmt {
    fn fmt_reg(&self, f: &mut Formatter, ctx: &Bytecode, v: Reg) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_refint(&self, f: &mut Formatter, ctx: &Bytecode, v: RefInt) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_reffloat(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFloat) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_refstring(&self, f: &mut Formatter, ctx: &Bytecode, v: RefString) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_reftype(&self, f: &mut Formatter, ctx: &Bytecode, v: RefType) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_reffield(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefField,
        parent: &Type,
    ) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_refenumconstruct(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefEnumConstruct,
        parent: &Type,
    ) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_type(&self, f: &mut Formatter, ctx: &Bytecode, v: &Type) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_typefun(&self, f: &mut Formatter, ctx: &Bytecode, v: &TypeFun) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_reffun(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFun) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_native(&self, f: &mut Formatter, ctx: &Bytecode, v: &Native) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_function_header(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        Display::fmt(&v, f)
    }

    fn fmt_function(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        Display::fmt(&v, f)
    }
}

/// [BytecodeFmt] impl that delegates to the [Debug] impl.
#[derive(Copy, Clone, Default)]
pub struct DebugFmt;

#[allow(unused_variables)]
impl BytecodeFmt for DebugFmt {
    fn fmt_reg(&self, f: &mut Formatter, ctx: &Bytecode, v: Reg) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_refint(&self, f: &mut Formatter, ctx: &Bytecode, v: RefInt) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_reffloat(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFloat) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_refstring(&self, f: &mut Formatter, ctx: &Bytecode, v: RefString) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_reftype(&self, f: &mut Formatter, ctx: &Bytecode, v: RefType) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_reffield(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefField,
        parent: &Type,
    ) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_refenumconstruct(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefEnumConstruct,
        parent: &Type,
    ) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_type(&self, f: &mut Formatter, ctx: &Bytecode, v: &Type) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_typefun(&self, f: &mut Formatter, ctx: &Bytecode, v: &TypeFun) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_reffun(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFun) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_native(&self, f: &mut Formatter, ctx: &Bytecode, v: &Native) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_function_header(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        Debug::fmt(&v, f)
    }

    fn fmt_function(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        Debug::fmt(&v, f)
    }
}

/// Formatter that delegates to the [Display] impl, without even using the [Bytecode] struct.
/// This [BytecodeFmt] impl is just for the sake of completeness, might as well use [Display] directly.
#[derive(Copy, Clone, Default)]
pub struct DisplayFmt;

impl BytecodeFmt for DisplayFmt {}

#[derive(Copy, Clone, Default)]
pub struct EnhancedFmt;

impl BytecodeFmt for EnhancedFmt {
    fn fmt_refint(&self, f: &mut Formatter, ctx: &Bytecode, v: RefInt) -> Result {
        write!(f, "{}", ctx[v])
    }

    fn fmt_reffloat(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFloat) -> Result {
        write!(f, "{}", ctx[v])
    }

    fn fmt_refstring(&self, f: &mut Formatter, ctx: &Bytecode, v: RefString) -> Result {
        f.write_str(&ctx[v])
    }

    fn fmt_reftype(&self, f: &mut Formatter, ctx: &Bytecode, v: RefType) -> Result {
        let ty = &ctx[v];
        self.fmt_type(f, ctx, ty)?;
        // No need to display @number if type is known
        if !v.is_known() && !ty.is_wrapper_type() {
            Display::fmt(&v, f)?
        }
        Ok(())
    }

    fn fmt_reffield(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefField,
        parent: &Type,
    ) -> Result {
        if let Some(obj) = parent.get_type_obj() {
            if v.0 < obj.fields.len() {
                self.fmt_refstring(f, ctx, obj.fields[v.0].name)
            } else {
                // panic!(
                //     "Trying to use an unknown field here ({}, field: {})",
                //     fmt(|f| self.fmt_type(f, ctx, parent)),
                //     v
                // );
                Display::fmt(&v, f)
            }
        } else if let Type::Virtual { fields } = parent {
            self.fmt_refstring(f, ctx, fields[v.0].name)
        } else {
            Display::fmt(&v, f)
        }
    }

    fn fmt_refenumconstruct(
        &self,
        f: &mut Formatter,
        ctx: &Bytecode,
        v: RefEnumConstruct,
        parent: &Type,
    ) -> Result {
        match parent {
            Type::Enum { constructs, .. } => {
                let name = constructs[v.0].name;
                if name.0 != 0 {
                    self.fmt_refstring(f, ctx, name)
                } else {
                    Display::fmt(&v, f)
                }
            }
            _ => Display::fmt(&v, f),
        }
    }

    fn fmt_type(&self, f: &mut Formatter, ctx: &Bytecode, v: &Type) -> Result {
        match v {
            Type::Fun(fun) => self.fmt_typefun(f, ctx, fun),
            Type::Obj(TypeObj { name, .. }) => self.fmt_refstring(f, ctx, *name),
            Type::Ref(reftype) => fmtools::write!(f,
                "ref<"
                |f| self.fmt_type(f, ctx, &ctx[*reftype])?;
                ">"
            ),
            Type::Virtual { fields } => fmtools::write!(f,
                "virtual<"{fmtools::join(", ", fields.iter().map(|fi|
                    fmtools::fmt!{
                        |f| self.fmt_refstring(f, ctx, fi.name)?;": "
                        match &ctx[fi.t] {
                            Type::Virtual {..} => {
                                {v}{fi.t}
                            }
                            Type::Fun(fun) | Type::Method(fun) => {
                                {fun}{fi.t}
                            }
                            _ => {
                                |f| self.fmt_type(f, ctx, &ctx[fi.t])?;
                            }
                        }
                    }
                ))}">"
            ),
            Type::Abstract { name } => self.fmt_refstring(f, ctx, *name),
            Type::Enum { name, .. } => fmtools::write!(f,
                "enum<"
                |f| self.fmt_refstring(f, ctx, *name)?;
                ">"
            ),
            Type::Null(reftype) => fmtools::write!(f,
                "null<"
                |f| self.fmt_reftype(f, ctx, *reftype)?;
                ">"
            ),
            Type::Method(fun) => self.fmt_typefun(f, ctx, fun),
            Type::Struct(TypeObj { name, .. }) => self.fmt_refstring(f, ctx, *name),
            Type::Packed(reftype) => fmtools::write!(f,
                "packed<"
                |f| self.fmt_reftype(f, ctx, *reftype)?;
                ">"
            ),
            _ => Display::fmt(v, f),
        }
    }

    fn fmt_typefun(&self, f: &mut Formatter, ctx: &Bytecode, v: &TypeFun) -> Result {
        fmtools::write!(f,
            "("{fmtools::join(", ", v.args.iter().map(|a| fmt(|f| self.fmt_type(f, ctx, &ctx[*a]))))}
            ") -> "|f| self.fmt_type(f, ctx, &ctx[v.ret])?;
        )
    }

    fn fmt_reffun(&self, f: &mut Formatter, ctx: &Bytecode, v: RefFun) -> Result {
        write!(f, "{}{}", v.name(ctx), v)
    }

    fn fmt_native(&self, f: &mut Formatter, ctx: &Bytecode, v: &Native) -> Result {
        write!(
            f,
            "{}/{}{} {}",
            fmt(|f| self.fmt_refstring(f, ctx, v.lib)),
            fmt(|f| self.fmt_refstring(f, ctx, v.name)),
            v.findex,
            fmt(|f| self.fmt_type(f, ctx, &ctx[v.t]))
        )
    }

    fn fmt_function_header(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        write!(
            f,
            "fn {} {}",
            fmt(|f| self.fmt_reffun(f, ctx, v.findex)),
            fmt(|f| self.fmt_type(f, ctx, &ctx[v.t]))
        )
    }

    fn fmt_function(&self, f: &mut Formatter, ctx: &Bytecode, v: &Function) -> Result {
        write!(
            f,
            "{}",
            fmtools::fmt! {
                |f| self.fmt_function_header(f, ctx, v)?;" ("{v.regs.len()}" regs, "{v.ops.len()}" ops)\n"
                for (i, reg) in v.regs.iter().enumerate() {
                    "    reg"{i:<2}" "|f| self.fmt_type(f, ctx, &ctx[*reg])?;"\n"
                }
                if let Some(debug) = &v.debug_info {
                    for ((i, o), (file, line)) in v.ops
                        .iter()
                        .enumerate()
                        .zip(debug.iter())
                    {
                        {ctx.debug_files.as_ref().unwrap()[*file]:>12}":"{line:<3}" "{i:>3}": "{o.display(ctx, v, i as i32, 11)}"\n"
                    }
                } else {
                    for (i, o) in v.ops
                        .iter()
                        .enumerate() {
                        {i:>3}": "{o.display(ctx, v, i as i32, 11)}"\n"
                    }
                }
            }
        )
    }
}

//region Display methods
// Boilerplate code that makes using [BytecodeFmt] spark a bit more joy.

macro_rules! sparks_joy {
    ($ty:ty, $meth:ident, nocopy) => {
        impl $ty {
            pub fn display_fmt<'a, Fmt: BytecodeFmt + 'a>(
                &'a self,
                bcfmt: Fmt,
                ctx: &'a Bytecode,
            ) -> impl Display + 'a {
                fmt(move |f| bcfmt.$meth(f, ctx, self))
            }

            pub fn display<'a, Fmt: BytecodeFmt + Default + 'a>(
                &'a self,
                ctx: &'a Bytecode,
            ) -> impl Display + 'a {
                self.display_fmt(Fmt::default(), ctx)
            }
        }
    };
    ($ty:ty, $meth:ident $(, $parent:ident)?) => {
        impl $ty {
            pub fn display_fmt<'a, Fmt: BytecodeFmt + 'a>(
                &'a self,
                bcfmt: Fmt,
                ctx: &'a Bytecode,
                $($parent: &'a Type,)?
            ) -> impl Display + 'a {
                fmt(move |f| bcfmt.$meth(f, ctx, *self $(, $parent)?))
            }

            pub fn display<'a, Fmt: BytecodeFmt + Default + 'a>(
                &'a self,
                ctx: &'a Bytecode,
                $($parent: &'a Type,)?
            ) -> impl Display + 'a {
                self.display_fmt(Fmt::default(), ctx $(, $parent)?)
            }
        }
    };
}

sparks_joy!(RefInt, fmt_refint);
sparks_joy!(RefFloat, fmt_reffloat);
sparks_joy!(RefString, fmt_refstring);
sparks_joy!(RefType, fmt_reftype);
sparks_joy!(Native, fmt_native, nocopy);
sparks_joy!(RefField, fmt_reffield, parent);
sparks_joy!(RefEnumConstruct, fmt_refenumconstruct, parent);
sparks_joy!(RefFun, fmt_reffun);
sparks_joy!(Type, fmt_type, nocopy);
sparks_joy!(Function, fmt_function, nocopy);

impl Function {
    pub fn display_header_fmt<'a, Fmt: BytecodeFmt + 'a>(
        &'a self,
        bcfmt: Fmt,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        fmt(move |f| bcfmt.fmt_function_header(f, ctx, self))
    }

    pub fn display_header<'a, Fmt: BytecodeFmt + Default + 'a>(
        &'a self,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        self.display_header_fmt(Fmt::default(), ctx)
    }
}

impl RefFun {
    pub fn display_header_fmt<'a, Fmt: BytecodeFmt + 'a>(
        &'a self,
        bcfmt: Fmt,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        fmt(move |f| match ctx.get(*self) {
            FunPtr::Fun(fun) => bcfmt.fmt_function_header(f, ctx, fun),
            FunPtr::Native(n) => bcfmt.fmt_native(f, ctx, n),
        })
    }

    pub fn display_header<'a, Fmt: BytecodeFmt + Default + 'a>(
        &'a self,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        self.display_header_fmt(Fmt::default(), ctx)
    }
}

impl FunPtr<'_> {
    pub fn display_header_fmt<'a, Fmt: BytecodeFmt + 'a>(
        &'a self,
        bcfmt: Fmt,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        fmt(move |f| match self {
            FunPtr::Fun(fun) => bcfmt.fmt_function_header(f, ctx, fun),
            FunPtr::Native(n) => bcfmt.fmt_native(f, ctx, n),
        })
    }

    pub fn display_header<'a, Fmt: BytecodeFmt + Default + 'a>(
        &'a self,
        ctx: &'a Bytecode,
    ) -> impl Display + 'a {
        self.display_header_fmt(Fmt::default(), ctx)
    }
}

//endregion

impl Opcode {
    /// This display is an enhanced assembly view, with nice printing and added information from the context
    pub fn display(
        &self,
        ctx: &Bytecode,
        parent: &Function,
        pos: i32,
        align: usize,
    ) -> impl Display {
        macro_rules! op {
            ($($arg:tt)*) => {
                format!("{:<align$} {}", self.name(), format_args!($($arg)*))
            };
        }

        match self {
            Opcode::Mov { dst, src } => op!("{dst} = {src}"),
            Opcode::Int { dst, ptr } => op!("{dst} = {}", ptr.display::<EnhancedFmt>(ctx)),
            Opcode::Float { dst, ptr } => op!("{dst} = {}", ptr.display::<EnhancedFmt>(ctx)),
            Opcode::Bool { dst, _value } => op!("{dst} = {}", _value),
            Opcode::String { dst, ptr } => op!("{dst} = \"{}\"", ptr.display::<EnhancedFmt>(ctx)),
            Opcode::Null { dst } => op!("{dst} = null"),
            Opcode::Add { dst, a, b } => op!("{dst} = {a} + {b}"),
            Opcode::Sub { dst, a, b } => op!("{dst} = {a} - {b}"),
            Opcode::Mul { dst, a, b } => op!("{dst} = {a} * {b}"),
            Opcode::SDiv { dst, a, b } => op!("{dst} = {a} / {b}"),
            Opcode::UDiv { dst, a, b } => op!("{dst} = {a} / {b}"),
            Opcode::SMod { dst, a, b } => op!("{dst} = {a} % {b}"),
            Opcode::UMod { dst, a, b } => op!("{dst} = {a} % {b}"),
            Opcode::Shl { dst, a, b } => op!("{dst} = {a} << {b}"),
            Opcode::SShr { dst, a, b } => op!("{dst} = {a} >> {b}"),
            Opcode::UShr { dst, a, b } => op!("{dst} = {a} >> {b}"),
            Opcode::And { dst, a, b } => op!("{dst} = {a} & {b}"),
            Opcode::Or { dst, a, b } => op!("{dst} = {a} | {b}"),
            Opcode::Xor { dst, a, b } => op!("{dst} = {a} ^ {b}"),
            Opcode::Neg { dst, src } => op!("{dst} = -{src}"),
            Opcode::Not { dst, src } => op!("{dst} = !{src}"),
            Opcode::Incr { dst } => op!("{dst}++"),
            Opcode::Decr { dst } => op!("{dst}--"),
            Opcode::Call0 { dst, fun } => op!("{dst} = {}()", fun.display::<EnhancedFmt>(ctx)),
            Opcode::Call1 { dst, fun, arg0 } => {
                op!("{dst} = {}({arg0})", fun.display::<EnhancedFmt>(ctx))
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => op!(
                "{dst} = {}({arg0}, {arg1})",
                fun.display::<EnhancedFmt>(ctx)
            ),
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => op!(
                "{dst} = {}({arg0}, {arg1}, {arg2})",
                fun.display::<EnhancedFmt>(ctx)
            ),
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => op!(
                "{dst} = {}({arg0}, {arg1},{arg2}, {arg3})",
                fun.display::<EnhancedFmt>(ctx)
            ),
            Opcode::CallN { dst, fun, args } => {
                op!(
                    "{dst} = {}({})",
                    fun.display::<EnhancedFmt>(ctx),
                    fmtools::join(", ", args)
                )
            }
            Opcode::CallMethod { dst, field, args } => {
                let mut args = args.iter();
                let arg0 = args.next().unwrap();
                op!(
                    "{dst} = {}.{}({})",
                    arg0,
                    field.display::<EnhancedFmt>(ctx, &ctx[parent[*arg0]]),
                    fmtools::join(", ", args)
                )
            }
            Opcode::CallThis { dst, field, args } => {
                op!(
                    "{dst} = reg0.{}({})",
                    field.display::<EnhancedFmt>(ctx, &ctx[parent.regs[0]]),
                    fmtools::join(", ", args)
                )
            }
            Opcode::CallClosure { dst, fun, args } => {
                op!("{dst} = {fun}({})", fmtools::join(", ", args))
            }
            Opcode::StaticClosure { dst, fun } => {
                op!("{dst} = {:?}", ctx.get(*fun))
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                op!("{dst} = {obj}.{:?}", ctx.get(*fun))
            }
            Opcode::GetGlobal { dst, global } => {
                op!("{dst} = global@{}", global.0)
            }
            Opcode::SetGlobal { global, src } => {
                op!("global@{} = {src}", global.0)
            }
            Opcode::Field { dst, obj, field } => {
                op!(
                    "{dst} = {obj}.{}",
                    field.display::<EnhancedFmt>(ctx, &ctx[parent[*obj]])
                )
            }
            Opcode::SetField { obj, field, src } => {
                op!(
                    "{obj}.{} = {src}",
                    field.display::<EnhancedFmt>(ctx, &ctx[parent[*obj]])
                )
            }
            Opcode::GetThis { dst, field } => {
                op!(
                    "{dst} = this.{}",
                    field.display::<EnhancedFmt>(ctx, &ctx[parent.regs[0]])
                )
            }
            Opcode::SetThis { field, src } => {
                op!(
                    "this.{} = {src}",
                    field.display::<EnhancedFmt>(ctx, &ctx[parent.regs[0]])
                )
            }
            Opcode::DynGet { dst, obj, field } => {
                op!("{dst} = {obj}[\"{}\"]", ctx[*field])
            }
            Opcode::DynSet { obj, field, src } => {
                op!("{obj}[\"{}\"] = {src}", ctx[*field])
            }
            Opcode::JTrue { cond, offset } => {
                op!("if {cond} == true jump to {}", pos + offset + 1)
            }
            Opcode::JFalse { cond, offset } => {
                op!("if {cond} == false jump to {}", pos + offset + 1)
            }
            Opcode::JNull { reg, offset } => {
                op!("if {reg} == null jump to {}", pos + offset + 1)
            }
            Opcode::JNotNull { reg, offset } => {
                op!("if {reg} != null jump to {}", pos + offset + 1)
            }
            Opcode::JSLt { a, b, offset } => {
                op!("if {a} < {b} jump to {}", pos + offset + 1)
            }
            Opcode::JSGte { a, b, offset } => {
                op!("if {a} >= {b} jump to {}", pos + offset + 1)
            }
            Opcode::JSGt { a, b, offset } => {
                op!("if {a} > {b} jump to {}", pos + offset + 1)
            }
            Opcode::JSLte { a, b, offset } => {
                op!("if {a} <= {b} jump to {}", pos + offset + 1)
            }
            Opcode::JULt { a, b, offset } => {
                op!("if {a} < {b} jump to {}", pos + offset + 1)
            }
            Opcode::JUGte { a, b, offset } => {
                op!("if {a} >= {b} jump to {}", pos + offset + 1)
            }
            Opcode::JNotLt { a, b, offset } => {
                op!("if {a} !< {b} jump to {}", pos + offset + 1)
            }
            Opcode::JNotGte { a, b, offset } => {
                op!("if {a} !>= {b} jump to {}", pos + offset + 1)
            }
            Opcode::JEq { a, b, offset } => {
                op!("if {a} == {b} jump to {}", pos + offset + 1)
            }
            Opcode::JNotEq { a, b, offset } => {
                op!("if {a} != {b} jump to {}", pos + offset + 1)
            }
            Opcode::JAlways { offset } => {
                op!("jump to {}", pos + offset + 1)
            }
            Opcode::ToDyn { dst, src } => {
                op!("{dst} = cast {src}")
            }
            Opcode::ToInt { dst, src } => {
                op!("{dst} = cast {src}")
            }
            Opcode::SafeCast { dst, src } => {
                op!("{dst} = cast {src}")
            }
            Opcode::UnsafeCast { dst, src } => {
                op!("{dst} = cast {src}")
            }
            Opcode::ToVirtual { dst, src } => {
                op!("{dst} = cast {src}")
            }
            Opcode::Ret { ret } => op!("{ret}"),
            Opcode::Throw { exc } => {
                op!("throw {exc}")
            }
            Opcode::Rethrow { exc } => {
                op!("rethrow {exc}")
            }
            Opcode::NullCheck { reg } => {
                op!("if {reg} == null throw exc")
            }
            Opcode::Trap { exc, offset } => {
                op!("try {exc} jump to {}", pos + offset + 1)
            }
            Opcode::EndTrap { exc } => {
                op!("catch {exc}")
            }
            Opcode::GetArray { dst, array, index } => {
                op!("{dst} = {array}[{index}]")
            }
            Opcode::SetArray { array, index, src } => {
                op!("{array}[{index}] = {src}")
            }
            Opcode::New { dst } => {
                op!("{dst} = new {}", parent[*dst].display::<EnhancedFmt>(ctx))
            }
            Opcode::ArraySize { dst, array } => {
                op!("{dst} = {array}.length")
            }
            Opcode::Type { dst, ty } => {
                op!("{dst} = {}", ty.display::<EnhancedFmt>(ctx))
            }
            Opcode::Ref { dst, src } => {
                op!("{dst} = &{src}")
            }
            Opcode::Unref { dst, src } => {
                op!("{dst} = *{src}")
            }
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                op!(
                    "{dst} = variant {} ({})",
                    construct.display::<EnhancedFmt>(ctx, &ctx[parent[*dst]]),
                    fmtools::join(", ", args)
                )
            }
            Opcode::EnumAlloc { dst, construct } => {
                op!(
                    "{dst} = new {}",
                    construct.display::<EnhancedFmt>(ctx, &ctx[parent[*dst]])
                )
            }
            Opcode::EnumIndex { dst, value } => {
                op!("{dst} = variant of {value}")
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                op!(
                    "{dst} = ({value} as {}).{}",
                    construct.display::<EnhancedFmt>(ctx, &ctx[parent[*value]]),
                    field.0
                )
            }
            Opcode::SetEnumField { value, field, src } => {
                op!("{value}.{} = {src}", field.0)
            }
            // Fallback to debug impl
            _ => format!("{self:?}"),
        }
    }
}

#[cfg(test)]
mod test {
    use std::fmt::{Display, Write};
    use std::fs;
    use std::path::Path;

    use crate::fmt::{fmt, DisplayFmt, EnhancedFmt};
    use crate::fmt::{BytecodeFmt, DebugFmt};
    use crate::types::{FunPtr, Reg};
    use crate::Bytecode;

    struct Null;

    impl Write for Null {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            Ok(())
        }
    }

    #[test]
    fn debug_formatter() {
        let ctx = Bytecode::default();
        // TODO test all
        assert_eq!(
            format!("{:?}", Reg(0)),
            format!("{}", fmt(|f| DebugFmt.fmt_reg(f, &ctx, Reg(0))))
        )
    }

    #[test]
    fn display_formatter() {
        let ctx = Bytecode::default();
        // TODO test all
        assert_eq!(
            format!("{}", Reg(0)),
            format!("{}", fmt(|f| DisplayFmt.fmt_reg(f, &ctx, Reg(0))))
        )
    }

    fn test_fmt(path: impl AsRef<Path>) {
        let code = Bytecode::from_file(path).unwrap();
        for f in code.functions() {
            write!(Null, "{}", f.display_header::<EnhancedFmt>(&code)).unwrap();
            write!(Null, "{}", f.display_header::<DisplayFmt>(&code)).unwrap();
            match f {
                FunPtr::Fun(fun) => {
                    write!(Null, "{}", fun.display_header::<EnhancedFmt>(&code)).unwrap();
                    write!(Null, "{}", fun.display::<EnhancedFmt>(&code)).unwrap();
                    write!(Null, "{}", fun.display_header::<DisplayFmt>(&code)).unwrap();
                    write!(Null, "{}", fun.display::<DisplayFmt>(&code)).unwrap();
                }
                FunPtr::Native(n) => {
                    write!(Null, "{}", n.display::<EnhancedFmt>(&code)).unwrap();
                    write!(Null, "{}", n.display::<DisplayFmt>(&code)).unwrap();
                }
            }
        }
        for t in &code.types {
            write!(Null, "{}", t.display::<EnhancedFmt>(&code)).unwrap();
            write!(Null, "{}", t.display::<DisplayFmt>(&code)).unwrap();
        }
    }

    #[test]
    fn fmt_all() {
        for entry in fs::read_dir("../../data").unwrap() {
            let path = entry.unwrap().path();
            if let Some(ext) = path.extension() {
                if ext == "hl" {
                    test_fmt(&path);
                }
            }
        }
    }

    #[test]
    fn fmt_wartales() {
        let path = "E:\\Games\\Wartales\\hlboot.dat";
        test_fmt(path);
    }

    #[test]
    fn fmt_northgard() {
        let path = "E:\\Games\\Northgard\\hlboot.dat";
        test_fmt(path);
    }
}

use crate::opcodes::Opcode;
use crate::types::{Function, RefField, RefType, Reg, Type, TypeFun, TypeObj};
use crate::{Bytecode, Error, Result};

fn reference(kind: &'static str, index: usize, len: usize) -> Result<()> {
    if index < len {
        Ok(())
    } else {
        Err(Error::InvalidReference { kind, index, len })
    }
}

impl Bytecode {
    /// Validate all parser-visible cross references and function-local operands.
    pub fn validate(&self) -> Result<()> {
        reference(
            "entrypoint function",
            self.entrypoint.0,
            self.findexes.len(),
        )?;
        for ty in &self.types {
            validate_type(self, ty)?;
        }
        for global in &self.globals {
            reference("global type", global.0, self.types.len())?;
        }
        for native in &self.natives {
            reference("native library", native.lib.0, self.strings.len())?;
            reference("native name", native.name.0, self.strings.len())?;
            validate_function_type(self, native.t, "native type")?;
            reference("native function", native.findex.0, self.findexes.len())?;
        }
        for function in &self.functions {
            validate_function_inner(self, function)?;
        }
        if let Some(constants) = &self.constants {
            for constant in constants {
                reference("constant global", constant.global.0, self.globals.len())?;
            }
        }
        if let Some((bytes, positions)) = &self.bytes {
            let mut previous = 0;
            for &position in positions {
                if position < previous || position > bytes.len() {
                    return Err(Error::MalformedBytecode(format!(
                        "Invalid bytes pool offset {position} after {previous} (blob length {})",
                        bytes.len()
                    )));
                }
                previous = position;
            }
        }
        Ok(())
    }

    /// Validate a function supplied to parser or decompiler APIs against this bytecode.
    pub fn validate_function(&self, function: &Function) -> Result<()> {
        validate_function_inner(self, function)
    }
}

fn validate_function_type(code: &Bytecode, reference_: RefType, kind: &'static str) -> Result<()> {
    reference(kind, reference_.0, code.types.len())?;
    match &code.types[reference_.0] {
        Type::Fun(_) | Type::Method(_) => Ok(()),
        _ => Err(Error::MalformedBytecode(format!(
            "{kind} {} does not reference a function type",
            reference_.0
        ))),
    }
}

fn validate_type(code: &Bytecode, ty: &Type) -> Result<()> {
    match ty {
        Type::Fun(function) | Type::Method(function) => validate_type_fun(code, function),
        Type::Obj(object) | Type::Struct(object) => validate_type_object(code, object),
        Type::Ref(inner) | Type::Null(inner) | Type::Packed(inner) => {
            reference("inner type", inner.0, code.types.len())
        }
        Type::Virtual { fields } => {
            for field in fields {
                reference("virtual field name", field.name.0, code.strings.len())?;
                reference("virtual field type", field.t.0, code.types.len())?;
            }
            Ok(())
        }
        Type::Abstract { name } => reference("abstract name", name.0, code.strings.len()),
        Type::Enum {
            name,
            global,
            constructs,
        } => {
            reference("enum name", name.0, code.strings.len())?;
            if global.0 > code.globals.len() {
                return Err(Error::InvalidReference {
                    kind: "enum static global",
                    index: global.0,
                    len: code.globals.len() + 1,
                });
            }
            for construct in constructs {
                reference(
                    "enum constructor name",
                    construct.name.0,
                    code.strings.len(),
                )?;
                for parameter in &construct.params {
                    reference("enum parameter type", parameter.0, code.types.len())?;
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn validate_type_fun(code: &Bytecode, function: &TypeFun) -> Result<()> {
    for argument in &function.args {
        reference("argument type", argument.0, code.types.len())?;
    }
    reference("return type", function.ret.0, code.types.len())
}

fn validate_type_object(code: &Bytecode, object: &TypeObj) -> Result<()> {
    reference("object name", object.name.0, code.strings.len())?;
    if let Some(parent) = object.super_ {
        reference("parent type", parent.0, code.types.len())?;
    }
    if object.global.0 > code.globals.len() {
        return Err(Error::InvalidReference {
            kind: "object static global",
            index: object.global.0,
            len: code.globals.len() + 1,
        });
    }
    for field in object.own_fields.iter().chain(object.fields.iter()) {
        reference("object field name", field.name.0, code.strings.len())?;
        reference("object field type", field.t.0, code.types.len())?;
    }
    for prototype in &object.protos {
        reference("prototype name", prototype.name.0, code.strings.len())?;
        reference(
            "prototype function",
            prototype.findex.0,
            code.findexes.len(),
        )?;
    }
    for (field, function) in &object.bindings {
        reference("binding field", field.0, object.fields.len())?;
        reference("binding function", function.0, code.findexes.len())?;
    }
    Ok(())
}

fn validate_function_inner(code: &Bytecode, function: &Function) -> Result<()> {
    validate_function_type(code, function.t, "function type")?;
    reference("function index", function.findex.0, code.findexes.len())?;
    reference("function name", function.name.0, code.strings.len())?;
    if let Some(parent) = function.parent {
        reference("function parent", parent.0, code.types.len())?;
    }
    for register in &function.regs {
        reference("register type", register.0, code.types.len())?;
    }
    if let Some(debug_info) = &function.debug_info {
        if debug_info.len() != function.ops.len() {
            return Err(Error::MalformedBytecode(format!(
                "Function {} has {} debug entries for {} opcodes",
                function.findex.0,
                debug_info.len(),
                function.ops.len()
            )));
        }
        if let Some(files) = &code.debug_files {
            for &(file, _) in debug_info {
                reference("debug file", file, files.len())?;
            }
        }
    }
    if let Some(assignments) = &function.assigns {
        for &(name, opcode) in assignments {
            reference("assignment name", name.0, code.strings.len())?;
            // HashLink writes this field with INDEX(), so negative debug markers
            // are valid and are preserved by the reader's usize representation.
            if (opcode as i32) >= 0 && opcode > function.ops.len() {
                return Err(Error::InvalidReference {
                    kind: "assignment opcode",
                    index: opcode,
                    len: function.ops.len() + 1,
                });
            }
        }
    }
    for opcode in &function.ops {
        validate_opcode(code, function, opcode)?;
    }
    Ok(())
}

fn register_type<'a>(code: &'a Bytecode, function: &Function, register: Reg) -> Result<&'a Type> {
    reference("register", register.0 as usize, function.regs.len())?;
    let type_reference = function.regs[register.0 as usize];
    reference("register type", type_reference.0, code.types.len())?;
    let mut ty = &code.types[type_reference.0];
    for _ in 0..=code.types.len() {
        let Some(inner) = ty.get_inner() else {
            return Ok(ty);
        };
        reference("wrapped register type", inner.0, code.types.len())?;
        ty = &code.types[inner.0];
    }
    Err(Error::MalformedBytecode(format!(
        "Register {} has a cyclic wrapper type",
        register.0
    )))
}

fn validate_field(
    code: &Bytecode,
    function: &Function,
    receiver: Reg,
    field: RefField,
    kind: &'static str,
) -> Result<()> {
    let ty = register_type(code, function, receiver)?;
    let field_count = match ty {
        Type::Obj(object) | Type::Struct(object) => object.fields.len(),
        Type::Virtual { fields } => fields.len(),
        _ => {
            return Err(Error::MalformedBytecode(format!(
                "{kind} {} uses non-object register {}",
                field.0, receiver.0
            )))
        }
    };
    reference(kind, field.0, field_count)
}

fn validate_method(
    code: &Bytecode,
    function: &Function,
    receiver: Reg,
    field: RefField,
    kind: &'static str,
) -> Result<()> {
    let ty = register_type(code, function, receiver)?;
    match ty {
        Type::Obj(object) | Type::Struct(object) => {
            let mut current = Some(object);
            let mut maximum = None;
            for _ in 0..=code.types.len() {
                let Some(object) = current else {
                    break;
                };
                for prototype in &object.protos {
                    if let Ok(index) = usize::try_from(prototype.pindex) {
                        maximum = Some(maximum.map_or(index, |value: usize| value.max(index)));
                        if index == field.0 {
                            return Ok(());
                        }
                    }
                }
                current = object
                    .super_
                    .and_then(|parent| code.types.get(parent.0))
                    .and_then(Type::get_type_obj);
            }
            Err(Error::InvalidReference {
                kind,
                index: field.0,
                len: maximum.map_or(0, |maximum| maximum + 1),
            })
        }
        Type::Virtual { fields } => reference(kind, field.0, fields.len()),
        _ => Err(Error::MalformedBytecode(format!(
            "{kind} {} uses non-object register {}",
            field.0, receiver.0
        ))),
    }
}

fn enum_construct<'a>(
    code: &'a Bytecode,
    function: &Function,
    register: Reg,
    construct: usize,
) -> Result<&'a crate::types::EnumConstruct> {
    match register_type(code, function, register)? {
        Type::Enum { constructs, .. } => constructs.get(construct).ok_or(Error::InvalidReference {
            kind: "enum constructor",
            index: construct,
            len: constructs.len(),
        }),
        _ => Err(Error::MalformedBytecode(format!(
            "Enum constructor {construct} uses non-enum register {}",
            register.0
        ))),
    }
}

fn validate_opcode(code: &Bytecode, function: &Function, opcode: &Opcode) -> Result<()> {
    let reg = |register: Reg| reference("register", register.0 as usize, function.regs.len());
    let regs = |registers: &[Reg]| -> Result<()> {
        for &register in registers {
            reg(register)?;
        }
        Ok(())
    };
    match opcode {
        Opcode::Mov { dst, src }
        | Opcode::Neg { dst, src }
        | Opcode::Not { dst, src }
        | Opcode::ToDyn { dst, src }
        | Opcode::ToSFloat { dst, src }
        | Opcode::ToUFloat { dst, src }
        | Opcode::ToInt { dst, src }
        | Opcode::SafeCast { dst, src }
        | Opcode::UnsafeCast { dst, src }
        | Opcode::ToVirtual { dst, src }
        | Opcode::GetType { dst, src }
        | Opcode::GetTID { dst, src }
        | Opcode::Ref { dst, src }
        | Opcode::Unref { dst, src }
        | Opcode::RefData { dst, src } => regs(&[*dst, *src]),
        Opcode::Add { dst, a, b }
        | Opcode::Sub { dst, a, b }
        | Opcode::Mul { dst, a, b }
        | Opcode::SDiv { dst, a, b }
        | Opcode::UDiv { dst, a, b }
        | Opcode::SMod { dst, a, b }
        | Opcode::UMod { dst, a, b }
        | Opcode::Shl { dst, a, b }
        | Opcode::SShr { dst, a, b }
        | Opcode::UShr { dst, a, b }
        | Opcode::And { dst, a, b }
        | Opcode::Or { dst, a, b }
        | Opcode::Xor { dst, a, b } => regs(&[*dst, *a, *b]),
        Opcode::Int { dst, ptr } => {
            reg(*dst)?;
            reference("integer constant", ptr.0, code.ints.len())
        }
        Opcode::Float { dst, ptr } => {
            reg(*dst)?;
            reference("float constant", ptr.0, code.floats.len())
        }
        Opcode::Bytes { dst, ptr } => {
            reg(*dst)?;
            reference(
                "bytes constant",
                ptr.0,
                code.bytes
                    .as_ref()
                    .map_or(0, |(_, positions)| positions.len()),
            )
        }
        Opcode::String { dst, ptr } => {
            reg(*dst)?;
            reference("string constant", ptr.0, code.strings.len())
        }
        Opcode::Bool { dst, .. }
        | Opcode::Null { dst }
        | Opcode::Incr { dst }
        | Opcode::Decr { dst }
        | Opcode::New { dst } => reg(*dst),
        Opcode::Call0 { dst, fun } | Opcode::StaticClosure { dst, fun } => {
            reg(*dst)?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::Call1 { dst, fun, arg0 } => {
            regs(&[*dst, *arg0])?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::Call2 {
            dst,
            fun,
            arg0,
            arg1,
        } => {
            regs(&[*dst, *arg0, *arg1])?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::Call3 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
        } => {
            regs(&[*dst, *arg0, *arg1, *arg2])?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::Call4 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
            arg3,
        } => {
            regs(&[*dst, *arg0, *arg1, *arg2, *arg3])?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::CallN { dst, fun, args } => {
            reg(*dst)?;
            regs(args)?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::CallMethod { dst, field, args } => {
            reg(*dst)?;
            regs(args)?;
            let receiver = args.first().copied().ok_or_else(|| {
                Error::MalformedBytecode("CallMethod has no receiver argument".into())
            })?;
            validate_method(code, function, receiver, *field, "method field")
        }
        Opcode::CallThis { dst, field, args } => {
            reg(*dst)?;
            regs(args)?;
            validate_method(code, function, Reg(0), *field, "this method field")
        }
        Opcode::CallClosure { dst, fun, args } => {
            regs(&[*dst, *fun])?;
            regs(args)
        }
        Opcode::InstanceClosure { dst, fun, obj } => {
            regs(&[*dst, *obj])?;
            reference("function operand", fun.0, code.findexes.len())
        }
        Opcode::VirtualClosure { dst, obj, field } => {
            regs(&[*dst, *obj])?;
            if !matches!(
                register_type(code, function, *obj)?,
                Type::Obj(_) | Type::Struct(_)
            ) {
                return Err(Error::MalformedBytecode(format!(
                    "virtual closure call target {} uses non-object receiver register {}",
                    field.0, obj.0
                )));
            }
            validate_method(code, function, *obj, *field, "virtual closure call target")
        }
        Opcode::GetGlobal { dst, global } => {
            reg(*dst)?;
            reference("global operand", global.0, code.globals.len())
        }
        Opcode::SetGlobal { global, src } => {
            reg(*src)?;
            reference("global operand", global.0, code.globals.len())
        }
        Opcode::Field { dst, obj, field } => {
            regs(&[*dst, *obj])?;
            validate_field(code, function, *obj, *field, "object field")
        }
        Opcode::SetField { obj, field, src } => {
            regs(&[*obj, *src])?;
            validate_field(code, function, *obj, *field, "object field")
        }
        Opcode::GetThis { dst, field } => {
            reg(*dst)?;
            validate_field(code, function, Reg(0), *field, "this field")
        }
        Opcode::SetThis { field, src } => {
            reg(*src)?;
            validate_field(code, function, Reg(0), *field, "this field")
        }
        Opcode::DynGet { dst, obj, field } => {
            regs(&[*dst, *obj])?;
            reference("dynamic field name", field.0, code.strings.len())
        }
        Opcode::DynSet { obj, field, src } => {
            regs(&[*obj, *src])?;
            reference("dynamic field name", field.0, code.strings.len())
        }
        Opcode::JTrue { cond, .. } | Opcode::JFalse { cond, .. } => reg(*cond),
        Opcode::JNull { reg: value, .. }
        | Opcode::JNotNull { reg: value, .. }
        | Opcode::NullCheck { reg: value } => reg(*value),
        Opcode::JSLt { a, b, .. }
        | Opcode::JSGte { a, b, .. }
        | Opcode::JSGt { a, b, .. }
        | Opcode::JSLte { a, b, .. }
        | Opcode::JULt { a, b, .. }
        | Opcode::JUGte { a, b, .. }
        | Opcode::JNotLt { a, b, .. }
        | Opcode::JNotGte { a, b, .. }
        | Opcode::JEq { a, b, .. }
        | Opcode::JNotEq { a, b, .. } => regs(&[*a, *b]),
        Opcode::Ret { ret } => reg(*ret),
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => reg(*exc),
        Opcode::Switch { reg: value, .. } => reg(*value),
        Opcode::Trap { exc, .. } | Opcode::EndTrap { exc } => reg(*exc),
        Opcode::GetI8 { dst, bytes, index }
        | Opcode::GetI16 { dst, bytes, index }
        | Opcode::GetMem { dst, bytes, index } => regs(&[*dst, *bytes, *index]),
        Opcode::GetArray { dst, array, index } => regs(&[*dst, *array, *index]),
        Opcode::SetI8 { bytes, index, src }
        | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } => regs(&[*bytes, *index, *src]),
        Opcode::SetArray { array, index, src } => regs(&[*array, *index, *src]),
        Opcode::ArraySize { dst, array } => regs(&[*dst, *array]),
        Opcode::Type { dst, ty } => {
            reg(*dst)?;
            reference("type operand", ty.0, code.types.len())
        }
        Opcode::Setref { dst, value } => regs(&[*dst, *value]),
        Opcode::MakeEnum {
            dst,
            construct,
            args,
        } => {
            reg(*dst)?;
            regs(args)?;
            let construct = enum_construct(code, function, *dst, construct.0)?;
            if construct.params.len() != args.len() {
                return Err(Error::MalformedBytecode(format!(
                    "Enum constructor expects {} arguments but opcode supplies {}",
                    construct.params.len(),
                    args.len()
                )));
            }
            Ok(())
        }
        Opcode::EnumAlloc { dst, construct } => {
            reg(*dst)?;
            enum_construct(code, function, *dst, construct.0).map(|_| ())
        }
        Opcode::EnumIndex { dst, value } => regs(&[*dst, *value]),
        Opcode::EnumField {
            dst,
            value,
            construct,
            field,
        } => {
            regs(&[*dst, *value])?;
            let construct = enum_construct(code, function, *value, construct.0)?;
            reference("enum field", field.0, construct.params.len())
        }
        Opcode::SetEnumField { value, field, src } => {
            regs(&[*value, *src])?;
            let construct = enum_construct(code, function, *value, 0)?;
            reference("enum field", field.0, construct.params.len())
        }
        Opcode::RefOffset {
            dst,
            reg: value,
            offset,
        } => regs(&[*dst, *value, *offset]),
        Opcode::Prefetch { value, field, mode } => {
            reg(*value)?;
            if !(0..=4).contains(mode) {
                return Err(Error::MalformedBytecode(format!(
                    "Prefetch mode {mode} is outside the HashLink range 0..=4"
                )));
            }
            if field.0 > 0 {
                validate_field(
                    code,
                    function,
                    *value,
                    RefField(field.0 - 1),
                    "prefetch field",
                )?;
            }
            Ok(())
        }
        Opcode::Asm { reg: value, .. } if value.0 > 0 => {
            reference("asm register", value.0 as usize - 1, function.regs.len())
        }
        Opcode::Label
        | Opcode::JAlways { .. }
        | Opcode::Assert
        | Opcode::Nop
        | Opcode::Catch { .. }
        | Opcode::Asm { .. } => Ok(()),
    }
}

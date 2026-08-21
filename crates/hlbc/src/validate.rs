use std::collections::HashSet;

use crate::opcodes::Opcode;
use crate::types::{Function, RefField, RefType, Reg, Type, TypeFun, TypeObj};
use crate::{Bytecode, Error, RefFunKnown, Result};

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
        if !(2..=5).contains(&self.version) {
            return Err(Error::UnsupportedVersion {
                version: self.version,
                min: 2,
                max: 5,
            });
        }
        if self.bytes.is_some() != (self.version >= 5) {
            return Err(Error::MalformedBytecode(format!(
                "Bytecode version {} {} a v5 bytes pool",
                self.version,
                if self.bytes.is_some() {
                    "contains"
                } else {
                    "is missing"
                }
            )));
        }
        if self.constants.is_some() != (self.version >= 4) {
            return Err(Error::MalformedBytecode(format!(
                "Bytecode version {} {} a v4 constants table",
                self.version,
                if self.constants.is_some() {
                    "contains"
                } else {
                    "is missing"
                }
            )));
        }

        validate_findexes(self)?;

        for (index, ty) in self.types.iter().enumerate() {
            validate_type(self, ty)?;
            if let Some(object) = ty.get_type_obj() {
                let expected = flattened_fields(self, index)?;
                if object.fields != expected {
                    return Err(Error::MalformedBytecode(format!(
                        "Type {index} has stale flattened fields (expected {}, found {})",
                        expected.len(),
                        object.fields.len()
                    )));
                }
            }
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
        // HashLink v2-v4 compiler output uses legacy storage and calling conventions
        // which cannot be verified from the declared register types alone.
        let enforce_semantics = self.version >= 5;
        for function in &self.functions {
            validate_function_inner(self, function, true, enforce_semantics)?;
        }
        if let Some(constants) = &self.constants {
            for constant in constants {
                validate_constant(self, constant)?;
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
        validate_function_inner(self, function, false, true)
    }

    /// Validate references and control-flow operands for a detached function body.
    ///
    /// This intentionally does not enforce signature-level register types, which lets
    /// analysis tools inspect partially reconstructed functions without accepting them
    /// as serializable bytecode.
    pub fn validate_function_references(&self, function: &Function) -> Result<()> {
        validate_function_inner(self, function, false, false)
    }
}

fn validate_findexes(code: &Bytecode) -> Result<()> {
    let total = code.functions.len() + code.natives.len();
    if code.findexes.len() != total {
        return Err(Error::MalformedBytecode(format!(
            "Function index cache has {} entries for {total} functions and natives",
            code.findexes.len()
        )));
    }
    let mut seen = vec![false; total];
    for (pool_index, function) in code.functions.iter().enumerate() {
        reference("function index", function.findex.0, total)?;
        if std::mem::replace(&mut seen[function.findex.0], true) {
            return Err(Error::MalformedBytecode(format!(
                "Duplicate function index {}",
                function.findex.0
            )));
        }
        if !matches!(code.findexes[function.findex.0], RefFunKnown::Fun(i) if i == pool_index) {
            return Err(Error::MalformedBytecode(format!(
                "Stale function index cache entry {}",
                function.findex.0
            )));
        }
    }
    for (pool_index, native) in code.natives.iter().enumerate() {
        reference("native function index", native.findex.0, total)?;
        if std::mem::replace(&mut seen[native.findex.0], true) {
            return Err(Error::MalformedBytecode(format!(
                "Duplicate function index {}",
                native.findex.0
            )));
        }
        if !matches!(code.findexes[native.findex.0], RefFunKnown::Native(i) if i == pool_index) {
            return Err(Error::MalformedBytecode(format!(
                "Stale native function index cache entry {}",
                native.findex.0
            )));
        }
    }
    if let Some(index) = seen.iter().position(|present| !present) {
        return Err(Error::MalformedBytecode(format!(
            "Missing function index {index}"
        )));
    }
    reference("entrypoint function", code.entrypoint.0, total)?;
    if !matches!(code.findexes[code.entrypoint.0], RefFunKnown::Fun(_)) {
        return Err(Error::MalformedBytecode(
            "Entrypoint references a native function".into(),
        ));
    }
    Ok(())
}

fn flattened_fields(code: &Bytecode, type_index: usize) -> Result<Vec<crate::types::ObjField>> {
    let mut hierarchy = Vec::new();
    let mut current = Some(RefType(type_index));
    let mut seen = HashSet::new();
    while let Some(reference_) = current {
        if !seen.insert(reference_.0) {
            return Err(Error::MalformedBytecode(format!(
                "Cycle in object inheritance at type {}",
                reference_.0
            )));
        }
        let object = code
            .types
            .get(reference_.0)
            .and_then(Type::get_type_obj)
            .ok_or_else(|| {
                Error::MalformedBytecode(format!(
                    "Parent type {} is not an object or struct",
                    reference_.0
                ))
            })?;
        hierarchy.push(object);
        current = object.super_;
    }
    let mut fields = Vec::new();
    for object in hierarchy.into_iter().rev() {
        fields.extend(object.own_fields.iter().cloned());
    }
    Ok(fields)
}

fn validate_constant(code: &Bytecode, constant: &crate::ConstantDef) -> Result<()> {
    reference("constant global", constant.global.0, code.globals.len())?;
    let type_reference = code.globals[constant.global.0];
    reference("constant global type", type_reference.0, code.types.len())?;
    let object = code.types[type_reference.0].get_type_obj().ok_or_else(|| {
        Error::MalformedBytecode(format!(
            "Constant global {} does not have an object or struct type",
            constant.global.0
        ))
    })?;
    if constant.fields.len() > object.fields.len() {
        return Err(Error::MalformedBytecode(format!(
            "Constant global {} has {} values for {} fields",
            constant.global.0,
            constant.fields.len(),
            object.fields.len()
        )));
    }
    for (&value, field) in constant.fields.iter().zip(&object.fields) {
        reference("constant field type", field.t.0, code.types.len())?;
        match code.types[field.t.0] {
            Type::I32 => reference("constant integer", value, code.ints.len())?,
            Type::Bool => {}
            Type::F64 => reference("constant float", value, code.floats.len())?,
            Type::Bytes => reference("constant string", value, code.strings.len())?,
            Type::Type => reference("constant type", value, code.types.len())?,
            _ => reference("constant global value", value, code.globals.len())?,
        }
    }
    Ok(())
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

fn validate_function_inner(
    code: &Bytecode,
    function: &Function,
    enforce_container_layout: bool,
    enforce_semantics: bool,
) -> Result<()> {
    validate_function_type(code, function.t, "function type")?;
    reference("function index", function.findex.0, code.findexes.len())?;
    if let Some(parent) = function.parent {
        reference("function parent", parent.0, code.types.len())?;
    }
    for register in &function.regs {
        reference("register type", register.0, code.types.len())?;
    }
    if enforce_container_layout {
        let expects_debug = code.debug_files.is_some();
        if function.debug_info.is_some() != expects_debug {
            return Err(Error::MalformedBytecode(format!(
                "Function {} debug information does not match the bytecode debug flag",
                function.findex.0
            )));
        }
        if function.assigns.is_some() != (expects_debug && code.version >= 3) {
            return Err(Error::MalformedBytecode(format!(
                "Function {} debug assignments do not match bytecode version {} and debug flag",
                function.findex.0, code.version
            )));
        }
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
            for &(file, line) in debug_info {
                reference("debug file", file, files.len())?;
                if file > 0x7FFF {
                    return Err(Error::ValueOutOfBounds {
                        value: file as i32,
                        limit: 0x8000,
                    });
                }
                if line > 0x1F_FFFF {
                    return Err(Error::ValueOutOfBounds {
                        value: i32::try_from(line).unwrap_or(i32::MAX),
                        limit: 0x20_0000,
                    });
                }
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
    for (index, opcode) in function.ops.iter().enumerate() {
        validate_opcode(code, function, index, opcode, enforce_semantics).map_err(|error| {
            match error {
                Error::MalformedBytecode(message) => Error::MalformedBytecode(format!(
                    "Function {} opcode {index} ({opcode:?}): {message}",
                    function.findex.0
                )),
                error => error,
            }
        })?;
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

fn register_type_ref(function: &Function, register: Reg) -> Result<RefType> {
    function
        .regs
        .get(register.0 as usize)
        .copied()
        .ok_or(Error::InvalidReference {
            kind: "register",
            index: register.0 as usize,
            len: function.regs.len(),
        })
}

fn expect_register_type(
    code: &Bytecode,
    function: &Function,
    register: Reg,
    expected: RefType,
    context: &str,
) -> Result<()> {
    let actual = register_type_ref(function, register)?;
    if !types_compatible(code, actual, expected) {
        return Err(Error::MalformedBytecode(format!(
            "{context} register {} has type {} ({:?}) but expected {} ({:?})",
            register.0,
            actual.0,
            code.types.get(actual.0),
            expected.0,
            code.types.get(expected.0)
        )));
    }
    Ok(())
}

fn expect_destination_type(
    code: &Bytecode,
    function: &Function,
    register: Reg,
    value_type: RefType,
    context: &str,
) -> Result<()> {
    let destination = register_type_ref(function, register)?;
    if !types_compatible(code, value_type, destination) {
        return Err(Error::MalformedBytecode(format!(
            "{context} value type {} ({:?}) is not assignable to register {} type {} ({:?})",
            value_type.0,
            code.types.get(value_type.0),
            register.0,
            destination.0,
            code.types.get(destination.0)
        )));
    }
    Ok(())
}

fn types_compatible(code: &Bytecode, actual: RefType, expected: RefType) -> bool {
    if actual == expected {
        return true;
    }
    let (Some(actual_type), Some(expected_type)) =
        (code.types.get(actual.0), code.types.get(expected.0))
    else {
        return false;
    };
    if matches!(actual_type, Type::Dyn) || matches!(expected_type, Type::Dyn) {
        return true;
    }
    if primitive_types_compatible(actual_type, expected_type) {
        return true;
    }
    match (actual_type, expected_type) {
        (Type::Null(inner), _) => types_compatible(code, *inner, expected),
        (_, Type::Null(inner)) => types_compatible(code, actual, *inner),
        (Type::Packed(inner), _) => types_compatible(code, *inner, expected),
        (
            Type::Virtual {
                fields: actual_fields,
            },
            Type::Virtual {
                fields: expected_fields,
            },
        ) => {
            expected_fields.len() < actual_fields.len()
                && actual_fields
                    .iter()
                    .zip(expected_fields)
                    .all(|(actual, expected)| {
                        actual.name == expected.name
                            && types_same(code, actual.t, expected.t, &mut HashSet::new())
                    })
        }
        (Type::Obj(_) | Type::Struct(_), Type::Obj(_) | Type::Struct(_)) => {
            let mut current = Some(actual);
            let mut seen = HashSet::new();
            while let Some(reference_) = current {
                if reference_ == expected {
                    return true;
                }
                if !seen.insert(reference_.0) {
                    return false;
                }
                current = code
                    .types
                    .get(reference_.0)
                    .and_then(Type::get_type_obj)
                    .and_then(|object| object.super_);
            }
            false
        }
        (Type::Fun(actual), Type::Fun(expected))
        | (Type::Method(actual), Type::Method(expected)) => {
            actual.args.len() == expected.args.len()
                && actual
                    .args
                    .iter()
                    .zip(&expected.args)
                    .all(|(&actual, &expected)| types_compatible(code, expected, actual))
                && types_compatible(code, actual.ret, expected.ret)
        }
        _ => false,
    }
}

fn primitive_types_compatible(actual: &Type, expected: &Type) -> bool {
    // HashLink compares these primitive types by kind, not by their index in
    // the serialized type table. Valid bytecode may contain duplicate entries.
    matches!(
        actual,
        Type::Void
            | Type::UI8
            | Type::UI16
            | Type::I32
            | Type::I64
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Bytes
            | Type::Dyn
            | Type::Array
            | Type::Type
            | Type::DynObj
            | Type::Guid
    ) && std::mem::discriminant(actual) == std::mem::discriminant(expected)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn duplicate_primitive_types_are_compatible() {
        let primitives = [
            Type::Void,
            Type::UI8,
            Type::UI16,
            Type::I32,
            Type::I64,
            Type::F32,
            Type::F64,
            Type::Bool,
            Type::Bytes,
            Type::Dyn,
            Type::Array,
            Type::Type,
            Type::DynObj,
            Type::Guid,
        ];
        let mut code = Bytecode::default();

        for primitive in primitives {
            code.types = vec![primitive.clone(), primitive];
            assert!(types_compatible(&code, RefType(0), RefType(1)));
        }

        code.types = vec![Type::Array, Type::Bytes];
        assert!(!types_compatible(&code, RefType(0), RefType(1)));
    }

    #[test]
    fn legacy_bytecode_uses_structural_validation() {
        use crate::types::Native;
        use crate::{RefFun, RefString, Str};

        let mut code = Bytecode::default();
        code.version = 4;
        code.bytes = None;
        code.constants = Some(Vec::new());
        code.strings = vec![Str::from_borrowed("")];
        code.types = vec![
            Type::Fun(TypeFun {
                args: Vec::new(),
                ret: RefType(4),
            }),
            Type::Fun(TypeFun {
                args: vec![RefType(2)],
                ret: RefType(4),
            }),
            Type::Bool,
            Type::Ref(RefType(2)),
            Type::Void,
        ];
        code.natives = vec![Native {
            name: RefString(0),
            lib: RefString(0),
            t: RefType(1),
            findex: RefFun(1),
        }];
        code.functions = vec![Function {
            t: RefType(0),
            findex: RefFun(0),
            regs: vec![RefType(4), RefType(3)],
            ops: vec![
                Opcode::Call1 {
                    dst: Reg(0),
                    fun: RefFun(1),
                    arg0: Reg(1),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        }];
        code.entrypoint = RefFun(0);
        code.findexes = vec![RefFunKnown::Fun(0), RefFunKnown::Native(0)];

        for version in 2..=4 {
            code.version = version;
            code.constants = (version >= 4).then(Vec::new);

            let mut serialized = Vec::new();
            code.serialize(&mut serialized).unwrap();
            let decoded = Bytecode::deserialize(serialized.as_slice()).unwrap();
            assert_eq!(decoded.version, version);
        }

        code.version = 5;
        code.bytes = Some((Vec::new(), Vec::new()));
        code.constants = Some(Vec::new());
        assert!(code.validate().is_err());
    }
}

fn function_signature(code: &Bytecode, findex: usize) -> Result<&TypeFun> {
    reference("function operand", findex, code.findexes.len())?;
    let type_reference = match code.findexes[findex] {
        RefFunKnown::Fun(index) => code.functions[index].t,
        RefFunKnown::Native(index) => code.natives[index].t,
    };
    validate_function_type(code, type_reference, "called function type")?;
    Ok(code.types[type_reference.0]
        .get_type_fun()
        .expect("validated function type"))
}

fn validate_call(
    code: &Bytecode,
    function: &Function,
    dst: Reg,
    findex: usize,
    args: &[Reg],
) -> Result<()> {
    let signature = function_signature(code, findex)?;
    if signature.args.len() != args.len() {
        return Err(Error::MalformedBytecode(format!(
            "Call to function {findex} supplies {} arguments but expects {}",
            args.len(),
            signature.args.len()
        )));
    }
    expect_destination_type(code, function, dst, signature.ret, "call result")?;
    for (&argument, &expected) in args.iter().zip(&signature.args) {
        expect_register_type(code, function, argument, expected, "call argument")?;
    }
    Ok(())
}

fn validate_direct_call(
    code: &Bytecode,
    function: &Function,
    dst: Reg,
    findex: usize,
    args: &[Reg],
    enforce_semantics: bool,
) -> Result<()> {
    if enforce_semantics {
        validate_call(code, function, dst, findex, args)
    } else {
        reference("register", dst.0 as usize, function.regs.len())?;
        for &argument in args {
            reference("register", argument.0 as usize, function.regs.len())?;
        }
        reference("function operand", findex, code.findexes.len())
    }
}

fn validate_jump(
    function: &Function,
    opcode_index: usize,
    offset: i32,
    kind: &str,
    require_backward_label: bool,
) -> Result<()> {
    let target = opcode_index as i64 + 1 + offset as i64;
    if !(0..=function.ops.len() as i64).contains(&target) {
        return Err(Error::MalformedBytecode(format!(
            "Function {} {kind} at opcode {opcode_index} targets {target}, outside 0..={}",
            function.findex.0,
            function.ops.len()
        )));
    }
    if require_backward_label
        && offset < 0
        && !matches!(function.ops.get(target as usize), Some(Opcode::Label))
    {
        return Err(Error::MalformedBytecode(format!(
            "Function {} backward {kind} at opcode {opcode_index} does not target Label",
            function.findex.0
        )));
    }
    Ok(())
}

fn validate_switch_jump(function: &Function, opcode_index: usize, offset: u32) -> Result<()> {
    let target = opcode_index as u64 + 1 + offset as u64;
    if target > function.ops.len() as u64 {
        return Err(Error::MalformedBytecode(format!(
            "Function {} switch at opcode {opcode_index} targets {target}, outside 0..={}",
            function.findex.0,
            function.ops.len()
        )));
    }
    Ok(())
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

fn field_type(
    code: &Bytecode,
    function: &Function,
    receiver: Reg,
    field: RefField,
    kind: &'static str,
) -> Result<RefType> {
    let ty = register_type(code, function, receiver)?;
    let field_index = field.0;
    let (field, len) = match ty {
        Type::Obj(object) | Type::Struct(object) => {
            (object.fields.get(field_index), object.fields.len())
        }
        Type::Virtual { fields } => (fields.get(field_index), fields.len()),
        _ => {
            return Err(Error::MalformedBytecode(format!(
                "{kind} {} uses non-object register {}",
                field_index, receiver.0
            )))
        }
    };
    field.map(|field| field.t).ok_or(Error::InvalidReference {
        kind,
        index: field_index,
        len,
    })
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

fn validate_opcode(
    code: &Bytecode,
    function: &Function,
    opcode_index: usize,
    opcode: &Opcode,
    enforce_semantics: bool,
) -> Result<()> {
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
            reference("integer constant", ptr.0, code.ints.len())?;
            if enforce_semantics
                && !matches!(
                    register_type(code, function, *dst)?,
                    Type::UI8 | Type::UI16 | Type::I32 | Type::I64 | Type::Guid
                )
            {
                return Err(Error::MalformedBytecode(format!(
                    "Int destination register {} is not an integer type",
                    dst.0
                )));
            }
            Ok(())
        }
        Opcode::Float { dst, ptr } => {
            reg(*dst)?;
            reference("float constant", ptr.0, code.floats.len())?;
            if enforce_semantics
                && !matches!(register_type(code, function, *dst)?, Type::F32 | Type::F64)
            {
                return Err(Error::MalformedBytecode(format!(
                    "Float destination register {} is not f32 or f64",
                    dst.0
                )));
            }
            Ok(())
        }
        Opcode::Bytes { dst, ptr } => {
            reg(*dst)?;
            let len = if code.version >= 5 {
                code.bytes
                    .as_ref()
                    .map_or(0, |(_, positions)| positions.len())
            } else {
                code.strings.len()
            };
            reference("bytes constant", ptr.0, len)?;
            if enforce_semantics && !matches!(register_type(code, function, *dst)?, Type::Bytes) {
                return Err(Error::MalformedBytecode(format!(
                    "Bytes destination register {} is not bytes",
                    dst.0
                )));
            }
            Ok(())
        }
        Opcode::String { dst, ptr } => {
            reg(*dst)?;
            reference("string constant", ptr.0, code.strings.len())?;
            if enforce_semantics && !matches!(register_type(code, function, *dst)?, Type::Bytes) {
                return Err(Error::MalformedBytecode(format!(
                    "String destination register {} is not bytes",
                    dst.0
                )));
            }
            Ok(())
        }
        Opcode::Bool { dst, .. }
        | Opcode::Null { dst }
        | Opcode::Incr { dst }
        | Opcode::Decr { dst }
        | Opcode::New { dst } => reg(*dst),
        Opcode::Call0 { dst, fun } => {
            validate_direct_call(code, function, *dst, fun.0, &[], enforce_semantics)
        }
        Opcode::Call1 { dst, fun, arg0 } => {
            validate_direct_call(code, function, *dst, fun.0, &[*arg0], enforce_semantics)
        }
        Opcode::Call2 {
            dst,
            fun,
            arg0,
            arg1,
        } => validate_direct_call(
            code,
            function,
            *dst,
            fun.0,
            &[*arg0, *arg1],
            enforce_semantics,
        ),
        Opcode::Call3 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
        } => validate_direct_call(
            code,
            function,
            *dst,
            fun.0,
            &[*arg0, *arg1, *arg2],
            enforce_semantics,
        ),
        Opcode::Call4 {
            dst,
            fun,
            arg0,
            arg1,
            arg2,
            arg3,
        } => validate_direct_call(
            code,
            function,
            *dst,
            fun.0,
            &[*arg0, *arg1, *arg2, *arg3],
            enforce_semantics,
        ),
        Opcode::CallN { dst, fun, args } => {
            validate_direct_call(code, function, *dst, fun.0, args, enforce_semantics)
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
            regs(args)?;
            if !enforce_semantics {
                return Ok(());
            }
            let signature = match register_type(code, function, *fun)? {
                Type::Fun(signature) | Type::Method(signature) => signature,
                Type::Dyn => return Ok(()),
                _ => {
                    return Err(Error::MalformedBytecode(format!(
                        "CallClosure register {} is not a function",
                        fun.0
                    )))
                }
            };
            if signature.args.len() != args.len() {
                return Err(Error::MalformedBytecode(format!(
                    "CallClosure supplies {} arguments but expects {}",
                    args.len(),
                    signature.args.len()
                )));
            }
            expect_destination_type(code, function, *dst, signature.ret, "closure result")?;
            for (&argument, &expected) in args.iter().zip(&signature.args) {
                expect_register_type(code, function, argument, expected, "closure argument")?;
            }
            Ok(())
        }
        Opcode::StaticClosure { dst, fun } => {
            reg(*dst)?;
            reference("function operand", fun.0, code.findexes.len())?;
            if !enforce_semantics {
                return Ok(());
            }
            let target = function_signature(code, fun.0)?;
            match register_type(code, function, *dst)? {
                Type::Fun(signature) | Type::Method(signature) if signature == target => Ok(()),
                _ => Err(Error::MalformedBytecode(format!(
                    "StaticClosure destination register {} has the wrong function type",
                    dst.0
                ))),
            }
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
            reference("global operand", global.0, code.globals.len())?;
            if enforce_semantics {
                expect_destination_type(code, function, *dst, code.globals[global.0], "global read")
            } else {
                Ok(())
            }
        }
        Opcode::SetGlobal { global, src } => {
            reg(*src)?;
            reference("global operand", global.0, code.globals.len())?;
            if enforce_semantics {
                expect_register_type(code, function, *src, code.globals[global.0], "global write")
            } else {
                Ok(())
            }
        }
        Opcode::Field { dst, obj, field } => {
            regs(&[*dst, *obj])?;
            if enforce_semantics {
                let expected = field_type(code, function, *obj, *field, "object field")?;
                expect_destination_type(code, function, *dst, expected, "field read")
            } else {
                validate_field(code, function, *obj, *field, "object field")
            }
        }
        Opcode::SetField { obj, field, src } => {
            regs(&[*obj, *src])?;
            if enforce_semantics {
                let expected = field_type(code, function, *obj, *field, "object field")?;
                expect_register_type(code, function, *src, expected, "field write")
            } else {
                validate_field(code, function, *obj, *field, "object field")
            }
        }
        Opcode::GetThis { dst, field } => {
            reg(*dst)?;
            if enforce_semantics {
                let expected = field_type(code, function, Reg(0), *field, "this field")?;
                expect_destination_type(code, function, *dst, expected, "this field read")
            } else {
                validate_field(code, function, Reg(0), *field, "this field")
            }
        }
        Opcode::SetThis { field, src } => {
            reg(*src)?;
            if enforce_semantics {
                let expected = field_type(code, function, Reg(0), *field, "this field")?;
                expect_register_type(code, function, *src, expected, "this field write")
            } else {
                validate_field(code, function, Reg(0), *field, "this field")
            }
        }
        Opcode::DynGet { dst, obj, field } => {
            regs(&[*dst, *obj])?;
            reference("dynamic field name", field.0, code.strings.len())
        }
        Opcode::DynSet { obj, field, src } => {
            regs(&[*obj, *src])?;
            reference("dynamic field name", field.0, code.strings.len())
        }
        Opcode::JTrue { cond, offset } | Opcode::JFalse { cond, offset } => {
            reg(*cond)?;
            validate_jump(
                function,
                opcode_index,
                *offset,
                "conditional jump",
                enforce_semantics,
            )
        }
        Opcode::NullCheck { reg: value } => reg(*value),
        Opcode::JSLt { a, b, offset }
        | Opcode::JSGte { a, b, offset }
        | Opcode::JSGt { a, b, offset }
        | Opcode::JSLte { a, b, offset }
        | Opcode::JULt { a, b, offset }
        | Opcode::JUGte { a, b, offset }
        | Opcode::JNotLt { a, b, offset }
        | Opcode::JNotGte { a, b, offset }
        | Opcode::JEq { a, b, offset }
        | Opcode::JNotEq { a, b, offset } => {
            regs(&[*a, *b])?;
            validate_jump(
                function,
                opcode_index,
                *offset,
                "comparison jump",
                enforce_semantics,
            )
        }
        Opcode::Ret { ret } => {
            reg(*ret)?;
            if enforce_semantics {
                let return_type = code.types[function.t.0]
                    .get_type_fun()
                    .expect("validated function type")
                    .ret;
                expect_register_type(code, function, *ret, return_type, "return")
            } else {
                Ok(())
            }
        }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => reg(*exc),
        Opcode::Switch {
            reg: value,
            offsets,
            end,
        } => {
            reg(*value)?;
            for &offset in offsets {
                validate_switch_jump(function, opcode_index, offset)?;
            }
            validate_switch_jump(function, opcode_index, *end)
        }
        Opcode::Trap { exc, offset } => {
            reg(*exc)?;
            validate_jump(function, opcode_index, *offset, "trap", enforce_semantics)
        }
        Opcode::EndTrap { .. } => Ok(()),
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
            reference("type operand", ty.0, code.types.len())?;
            if enforce_semantics && !matches!(register_type(code, function, *dst)?, Type::Type) {
                return Err(Error::MalformedBytecode(format!(
                    "Type destination register {} is not a runtime type",
                    dst.0
                )));
            }
            Ok(())
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
            let max_fields = match register_type(code, function, *value)? {
                Type::Enum { constructs, .. } => constructs
                    .iter()
                    .map(|construct| construct.params.len())
                    .max()
                    .unwrap_or(0),
                _ => {
                    return Err(Error::MalformedBytecode(format!(
                        "SetEnumField uses non-enum register {}",
                        value.0
                    )))
                }
            };
            reference("enum field", field.0, max_fields)
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
        Opcode::JAlways { offset } => validate_jump(
            function,
            opcode_index,
            *offset,
            "unconditional jump",
            enforce_semantics,
        ),
        Opcode::JNull { reg: value, offset } | Opcode::JNotNull { reg: value, offset } => {
            reg(*value)?;
            validate_jump(
                function,
                opcode_index,
                *offset,
                "null jump",
                enforce_semantics,
            )
        }
        Opcode::Catch { global } => reference("catch type global", global.0, code.globals.len()),
        Opcode::Label | Opcode::Assert | Opcode::Nop | Opcode::Asm { .. } => Ok(()),
    }
}

fn types_same(
    code: &Bytecode,
    actual: RefType,
    expected: RefType,
    seen: &mut HashSet<(usize, usize)>,
) -> bool {
    if actual == expected {
        return true;
    }
    if !seen.insert((actual.0, expected.0)) {
        return true;
    }
    let (Some(actual), Some(expected)) = (code.types.get(actual.0), code.types.get(expected.0))
    else {
        return false;
    };
    if primitive_types_compatible(actual, expected) {
        return true;
    }
    match (actual, expected) {
        (Type::Ref(actual), Type::Ref(expected))
        | (Type::Null(actual), Type::Null(expected))
        | (Type::Packed(actual), Type::Packed(expected)) => {
            types_same(code, *actual, *expected, seen)
        }
        (Type::Fun(actual), Type::Fun(expected))
        | (Type::Method(actual), Type::Method(expected)) => {
            actual.args.len() == expected.args.len()
                && actual
                    .args
                    .iter()
                    .zip(&expected.args)
                    .all(|(&actual, &expected)| types_same(code, actual, expected, seen))
                && types_same(code, actual.ret, expected.ret, seen)
        }
        _ => false,
    }
}

#[cfg(test)]
mod compatibility_tests {
    use super::*;

    #[test]
    fn wider_virtual_type_is_compatible_with_matching_prefix() {
        let mut code = Bytecode::default();
        code.strings = vec!["first".into(), "second".into(), "third".into()];
        code.types = vec![Type::I32, Type::I32];
        let fields = vec![
            crate::types::ObjField {
                name: crate::types::RefString(0),
                t: RefType(0),
            },
            crate::types::ObjField {
                name: crate::types::RefString(1),
                t: RefType(0),
            },
            crate::types::ObjField {
                name: crate::types::RefString(2),
                t: RefType(0),
            },
        ];
        code.types.push(Type::Virtual {
            fields: fields.clone(),
        });
        code.types.push(Type::Virtual {
            fields: fields[..2].to_vec(),
        });

        assert!(types_compatible(&code, RefType(2), RefType(3)));
        assert!(!types_compatible(&code, RefType(3), RefType(2)));
    }

    #[test]
    fn packed_source_is_compatible_with_its_inner_type() {
        let mut code = Bytecode::default();
        code.types = vec![Type::Bool, Type::Packed(RefType(0))];

        assert!(types_compatible(&code, RefType(1), RefType(0)));
        assert!(!types_compatible(&code, RefType(0), RefType(1)));
    }
}

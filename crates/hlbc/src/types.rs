use std::collections::HashMap;
use std::ops::Index;

use crate::{Bytecode, Opcode, Resolve, Str};
use serde::{Serialize, Deserialize};

/// Offset for a jump instruction. Can be negative, indicating a backward jump.
pub type JumpOffset = i32;

pub type InlineInt = i32;
pub type InlineBool = bool;

/// A register argument
///
/// Registers are a function local variables.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash, Serialize, Deserialize)]
pub struct Reg(pub u32);

/// A reference to the i32 constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct RefInt(pub usize);

/// A reference to the f64 constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct RefFloat(pub usize);

/// A reference to the bytes constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct RefBytes(pub usize);

/// Reference to the string constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct RefString(pub usize);

impl RefString {
    /// If a [RefString] is null, it indicates an element has no name
    pub fn is_null(&self) -> bool {
        self.0 == 0
    }
}

/// A reference to a global
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash, Serialize, Deserialize)]
pub struct RefGlobal(pub usize);

/// An object field definition
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ObjField {
    /// Field name
    pub name: RefString,
    /// Field type
    pub t: RefType,
}

impl ObjField {
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }
}

/// A reference to an object field
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, Serialize, Deserialize)]
pub struct RefField(pub usize);

/// An object method definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ObjProto {
    /// Method name
    pub name: RefString,
    /// Function bound to this method
    pub findex: RefFun,
    /// Don't know what this is used for
    pub pindex: i32,
}

impl ObjProto {
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }
}

/// An enum variant definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumConstruct {
    /// Variant name
    pub name: RefString,
    /// Variant fields types
    pub params: Vec<RefType>,
}

impl EnumConstruct {
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }
}

/// A reference to an enum variant
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RefEnumConstruct(pub usize);

/// Common type for [Type::Fun] and [Type::Method]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeFun {
    pub args: Vec<RefType>,
    pub ret: RefType,
}

/// Common type for [Type::Obj] and [Type::Struct]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeObj {
    pub name: RefString,
    pub super_: Option<RefType>,
    pub global: RefGlobal,
    /// Fields defined in this type
    pub own_fields: Vec<ObjField>,
    /// Methods in this class
    pub protos: Vec<ObjProto>,
    /// Functions bounds to class fields
    pub bindings: HashMap<RefField, RefFun>,

    // Data below is not stored in the bytecode
    /// Fields including parents in the hierarchy
    pub fields: Vec<ObjField>,
}

impl TypeObj {
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }

    /// Get the static part of this class
    pub fn get_static_type<'a>(&self, ctx: &'a Bytecode) -> Option<&'a TypeObj> {
        if self.global.0 > 0 {
            ctx.globals[self.global.0 - 1].as_obj(ctx)
        } else {
            None
        }
    }
}

/// Type available in the hashlink type system. Every type is one of those.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Void,
    UI8,
    UI16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Bytes,
    Dyn,
    /// The type of a function
    Fun(TypeFun),
    /// The type of an object (class)
    Obj(TypeObj),
    /// Type of arrays, arrays are dynamic
    Array,
    /// The type of a type object
    Type,
    /// Reference to an inner type
    Ref(RefType),
    /// The type of anonymous objects
    Virtual {
        fields: Vec<ObjField>,
    },
    DynObj,
    /// Abstract class ?
    Abstract {
        name: RefString,
    },
    /// Enum, algebraic data types
    Enum {
        name: RefString,
        global: RefGlobal,
        constructs: Vec<EnumConstruct>,
    },
    /// Null wrapper
    Null(RefType),
    /// Type of a method
    Method(TypeFun),
    /// Type of a struct
    Struct(TypeObj),
    /// Packed wrapper
    Packed(RefType),
}

impl Type {
    pub fn is_wrapper_type(&self) -> bool {
        matches!(self, Type::Ref(_) | Type::Null(_) | Type::Packed(_))
    }

    pub fn get_type_obj(&self) -> Option<&TypeObj> {
        match self {
            Type::Obj(obj) => Some(obj),
            Type::Struct(obj) => Some(obj),
            _ => None,
        }
    }

    pub fn get_type_obj_mut(&mut self) -> Option<&mut TypeObj> {
        match self {
            Type::Obj(obj) => Some(obj),
            Type::Struct(obj) => Some(obj),
            _ => None,
        }
    }

    pub fn get_type_fun(&self) -> Option<&TypeFun> {
        match self {
            Type::Fun(fun) => Some(fun),
            Type::Method(fun) => Some(fun),
            _ => None,
        }
    }

    /// If this type is a wrapper type, return the inner type.
    pub fn get_inner(&self) -> Option<RefType> {
        match self {
            &Type::Ref(inner) | &Type::Null(inner) | &Type::Packed(inner) => Some(inner),
            _ => None,
        }
    }

    /// Replace the fields of an object or struct type.
    pub fn set_fields(&mut self, fields: Vec<ObjField>) {
        if let Some(obj) = self.get_type_obj_mut() {
            obj.fields = fields;
        }
    }

    /// Add a new field to an object or struct type.
    pub fn add_field(&mut self, field: ObjField) {
        if let Some(obj) = self.get_type_obj_mut() {
            obj.fields.push(field);
        }
    }

    /// Remove a field by index from an object or struct type.
    pub fn remove_field(&mut self, index: usize) -> Option<ObjField> {
        if let Some(obj) = self.get_type_obj_mut() {
            if index < obj.fields.len() {
                return Some(obj.fields.remove(index));
            }
        }
        None
    }

    /// Replace the prototypes (methods) of an object or struct type.
    pub fn set_protos(&mut self, protos: Vec<ObjProto>) {
        if let Some(obj) = self.get_type_obj_mut() {
            obj.protos = protos;
        }
    }

    /// Add a new prototype (method) to an object or struct type.
    pub fn add_proto(&mut self, proto: ObjProto) {
        if let Some(obj) = self.get_type_obj_mut() {
            obj.protos.push(proto);
        }
    }

    /// Remove a prototype (method) by index from an object or struct type.
    pub fn remove_proto(&mut self, index: usize) -> Option<ObjProto> {
        if let Some(obj) = self.get_type_obj_mut() {
            if index < obj.protos.len() {
                return Some(obj.protos.remove(index));
            }
        }
        None
    }

    /// Set the name of an object, struct, or abstract type.
    pub fn set_name(&mut self, name: RefString) {
        match self {
            Type::Obj(obj) | Type::Struct(obj) => obj.name = name,
            Type::Abstract { name: ref mut n } => *n = name,
            _ => {}
        }
    }

    /// Set the super type of an object or struct type.
    pub fn set_super(&mut self, super_type: Option<RefType>) {
        if let Some(obj) = self.get_type_obj_mut() {
            obj.super_ = super_type;
        }
    }

    /// Replace the enum constructs of an enum type.
    pub fn set_enum_constructs(&mut self, constructs: Vec<EnumConstruct>) {
        if let Type::Enum { constructs: ref mut c, .. } = self {
            *c = constructs;
        }
    }

    /// Add a new construct to an enum type.
    pub fn add_enum_construct(&mut self, construct: EnumConstruct) {
        if let Type::Enum { constructs, .. } = self {
            constructs.push(construct);
        }
    }

    /// Remove a construct by index from an enum type.
    pub fn remove_enum_construct(&mut self, index: usize) -> Option<EnumConstruct> {
        if let Type::Enum { constructs, .. } = self {
            if index < constructs.len() {
                return Some(constructs.remove(index));
            }
        }
        None
    }

    /// Load a Type from a JSON string
    pub fn from_json(s: &str) -> serde_json::Result<Self> {
        serde_json::from_str(s)
    }

    /// Serialize a Type to a JSON string
    pub fn to_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(self)
    }
}

/// Reference to a type in the constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct RefType(pub usize);

impl RefType {
    pub fn is_void(&self) -> bool {
        self.0 == 0
    }

    /// Some type references points to the base hashlink types that are always loaded in the same place.
    /// This is a bit risky to rely on this.
    pub fn is_known(&self) -> bool {
        self.0 <= 9 || self.0 == 11 || self.0 == 14
    }

    /// ### Panics
    /// Panics if `self.is_known() == false`
    pub fn to_known(&self) -> Type {
        match self.0 {
            0 => Type::Void,
            1 => Type::UI8,
            2 => Type::UI16,
            3 => Type::I32,
            4 => Type::I64,
            5 => Type::F32,
            6 => Type::F64,
            7 => Type::Bool,
            8 => Type::Type,
            9 => Type::Dyn,
            11 => Type::Array,
            14 => Type::Bytes,
            _ => {
                panic!("This not a known type")
            }
        }
    }

    pub fn as_fun<'a>(&self, ctx: &'a Bytecode) -> Option<&'a TypeFun> {
        match ctx.get(*self) {
            Type::Fun(fun) => Some(fun),
            Type::Method(fun) => Some(fun),
            _ => None,
        }
    }

    pub fn as_obj<'a>(&self, ctx: &'a Bytecode) -> Option<&'a TypeObj> {
        ctx.get(*self).get_type_obj()
    }

    pub fn field<'a>(&self, field: RefField, ctx: &'a Bytecode) -> Option<&'a ObjField> {
        self.as_obj(ctx).map(|obj| &obj.fields[field.0])
    }

    pub fn method<'a>(&self, meth: usize, ctx: &'a Bytecode) -> Option<&'a ObjProto> {
        self.as_obj(ctx).and_then(|obj| obj.protos.get(meth))
    }
}

/// A native function reference. Contains no code but indicates the library from where to load it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Native {
    /// Native function name
    pub name: RefString,
    /// Native lib name
    pub lib: RefString,
    pub t: RefType,
    pub findex: RefFun,
}

impl Native {
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }

    pub fn lib(&self, code: &Bytecode) -> Str {
        code.get(self.lib)
    }

    /// If the lib name starts with '?', then it is lazily loaded.
    pub fn lib_is_lazy(&self, code: &Bytecode) -> bool {
        self.lib(code).starts_with('?')
    }

    /// Get the native function signature type
    pub fn ty<'a>(&self, code: &'a Bytecode) -> &'a TypeFun {
        // Guaranteed to be a TypeFun
        self.t.as_fun(code).expect("Unknown type ?")
    }

    pub fn args<'a>(&self, code: &'a Bytecode) -> &'a [RefType] {
        &self.ty(code).args
    }

    pub fn ret<'a>(&self, code: &'a Bytecode) -> &'a Type {
        code.get(self.ty(code).ret)
    }
}

/// A function definition with its code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    /// Type of the function : args and return type. Guaranteed to be a [TypeFun].
    pub t: RefType,
    pub findex: RefFun,
    /// The types of the registers used by this function
    pub regs: Vec<RefType>,
    /// Instructions
    pub ops: Vec<Opcode>,
    /// *Debug* File and line information for each instruction
    pub debug_info: Option<Vec<(usize, usize)>>,
    /// *Debug* Information about variable names. Correspond to variable assignments.
    pub assigns: Option<Vec<(RefString, usize)>>,

    // Fields below are not part of the bytecode
    /// Functions have no name per se, this is the name of the field or method they are attached to
    pub name: RefString,
    /// Parent type (Obj/Struct) this function is a member of.
    /// This does not mean it's a method
    pub parent: Option<RefType>,
}

impl Function {
    /// Get the type of a register
    pub fn regtype(&self, reg: Reg) -> RefType {
        self[reg]
    }

    /// Convenience method to resolve the function name
    pub fn name(&self, code: &Bytecode) -> Str {
        code.get(self.name)
    }

    /// Get the function signature type
    pub fn ty<'a>(&self, code: &'a Bytecode) -> &'a TypeFun {
        // Guaranteed to be a TypeFun
        self.t.as_fun(code).expect("Unknown type ?")
    }

    /// Convenience method to resolve the function args
    pub fn args<'a>(&self, code: &'a Bytecode) -> &'a [RefType] {
        &self.ty(code).args
    }

    /// Convenience method to resolve the function return type
    pub fn ret<'a>(&self, code: &'a Bytecode) -> &'a Type {
        code.index(self.ty(code).ret)
    }

    /// Uses the assigns to find the name of an argument
    pub fn arg_name(&self, code: &Bytecode, pos: usize) -> Option<Str> {
        self.assigns.as_ref().and_then(|a| {
            a.iter()
                .filter(|&&(_, i)| i == 0)
                .enumerate()
                .find_map(|(j, &(s, _))| {
                    if j == pos {
                        Some(code[s].clone())
                    } else {
                        None
                    }
                })
        })
    }

    /// Uses the assigns to find the name of a variable
    pub fn var_name(&self, code: &Bytecode, pos: usize) -> Option<Str> {
        self.assigns.as_ref().and_then(|a| {
            a.iter().find_map(|&(s, i)| {
                if pos + 1 == i {
                    Some(code[s].clone())
                } else {
                    None
                }
            })
        })
    }

    /// A function is a method if the first argument has the same type as the parent type
    pub fn is_method(&self) -> bool {
        self.parent
            .map(|parent| !self.regs.is_empty() && self.regs[0] == parent)
            .unwrap_or(false)
    }

    /// Short for `.ops.iter().enumerate()`
    pub fn ops(&self) -> impl Iterator<Item = (usize, &Opcode)> {
        self.ops.iter().enumerate()
    }

    /// Replace the entire ops vector (bytecode) with a new one.
    pub fn set_ops(&mut self, ops: Vec<Opcode>) {
        self.ops = ops;
    }

    /// Replace a single opcode at the given index.
    pub fn set_op(&mut self, idx: usize, op: Opcode) {
        if let Some(slot) = self.ops.get_mut(idx) {
            *slot = op;
        }
    }

    /// Insert an opcode at the given index.
    pub fn insert_op(&mut self, idx: usize, op: Opcode) {
        self.ops.insert(idx, op);
    }

    /// Remove an opcode at the given index.
    pub fn remove_op(&mut self, idx: usize) -> Option<Opcode> {
        if idx < self.ops.len() {
            Some(self.ops.remove(idx))
        } else {
            None
        }
    }

    /// Replace the register types vector.
    pub fn set_regs(&mut self, regs: Vec<RefType>) {
        self.regs = regs;
    }

    /// Set the type of a specific register.
    pub fn set_reg_type(&mut self, reg: Reg, ty: RefType) {
        if let Some(slot) = self.regs.get_mut(reg.0 as usize) {
            *slot = ty;
        }
    }

    /// Set the function's type (signature).
    pub fn set_type(&mut self, t: RefType) {
        self.t = t;
    }

    /// Set the function's name.
    pub fn set_name(&mut self, name: RefString) {
        self.name = name;
    }

    /// Set the parent type.
    pub fn set_parent(&mut self, parent: Option<RefType>) {
        self.parent = parent;
    }

    /// Set the debug info.
    pub fn set_debug_info(&mut self, debug_info: Option<Vec<(usize, usize)>>) {
        self.debug_info = debug_info;
    }

    /// Set the assigns info.
    pub fn set_assigns(&mut self, assigns: Option<Vec<(RefString, usize)>>) {
        self.assigns = assigns;
    }

    /// Removes all debug-related information from the function.
    pub fn strip_debug_info(&mut self) {
        self.debug_info = None;
        self.assigns = None;
    }

    /// Load a Function from a JSON string
    pub fn from_json(s: &str) -> serde_json::Result<Self> {
        serde_json::from_str(s)
    }

    /// Serialize a Function to a JSON string
    pub fn to_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(self)
    }
}

impl Index<Reg> for Function {
    type Output = RefType;

    /// Get the type of a register
    fn index(&self, index: Reg) -> &Self::Output {
        &self.regs[index.0 as usize]
    }
}

/// Index reference to a function or a native in the pool (findex)
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default, Serialize, Deserialize)]
pub struct RefFun(pub usize);

impl RefFun {
    /// Useful when you already know you should be getting a Function
    pub fn as_fn<'a>(&self, code: &'a Bytecode) -> Option<&'a Function> {
        code.get(*self).as_fn()
    }

    pub fn name(&self, code: &Bytecode) -> Str {
        // Use safe_get_ref_fun. If None, return fallback.
        match code.safe_get_ref_fun(*self) {
            Some(FunPtr::Fun(fun)) => fun.name(code),
            Some(FunPtr::Native(n)) => n.name(code),
            None => Str::from_static("[invalid function ref]"),
        }
    }

    pub fn ty<'a>(&self, code: &'a Bytecode) -> &'a TypeFun {
        match code.get(*self) {
            FunPtr::Fun(fun) => fun.ty(code),
            FunPtr::Native(n) => n.ty(code),
        }
    }

    pub fn args<'a>(&self, code: &'a Bytecode) -> &'a [RefType] {
        &self.ty(code).args
    }

    pub fn ret<'a>(&self, code: &'a Bytecode) -> &'a Type {
        code.get(self.ty(code).ret)
    }
}

/// Reference to a function or a native object
#[derive(Debug, Copy, Clone)]
pub enum FunPtr<'a> {
    Fun(&'a Function),
    Native(&'a Native),
}

impl<'a> FunPtr<'a> {
    pub fn as_fn(self: FunPtr<'a>) -> Option<&'a Function> {
        match self {
            FunPtr::Fun(fun) => Some(fun),
            FunPtr::Native(_) => None,
        }
    }

    pub fn findex(&self) -> RefFun {
        match self {
            FunPtr::Fun(fun) => fun.findex,
            FunPtr::Native(n) => n.findex,
        }
    }

    pub fn name(&self, code: &Bytecode) -> Str {
        match *self {
            FunPtr::Fun(fun) => fun.name(code),
            FunPtr::Native(n) => n.name(code),
        }
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, FunPtr::Fun(_))
    }

    pub fn is_native(&self) -> bool {
        matches!(self, FunPtr::Native(_))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantDef {
    pub global: RefGlobal,
    pub fields: Vec<usize>,
}

use crate::types::{
    InlineBool, InlineInt, JumpOffset, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun,
    RefGlobal, RefInt, RefString, RefType, Reg, SwitchOffset,
};
use crate::{AdjustReferences, IndexMapping};
use serde::{Deserialize, Serialize};

/// Static description of one opcode operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct OperandMetadata {
    pub name: &'static str,
    pub kind: &'static str,
}

/// Static metadata generated from the [`Opcode`] declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct OpcodeMetadata {
    pub code: u8,
    pub name: &'static str,
    pub operands: &'static [OperandMetadata],
    pub semantics: &'static OpcodeSemantics,
}

/// Observable runtime effects beyond register reads and writes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SideEffect {
    Call,
    Allocation,
    MemoryRead,
    MemoryWrite,
    GlobalRead,
    GlobalWrite,
    ReferenceAlias,
    ReferenceRead,
    ReferenceWrite,
    ExceptionState,
    DebugBreak,
    Prefetch,
    InlineAssembly,
}

/// How execution continues after an opcode completes normally.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ControlFlowBehavior {
    Fallthrough,
    ConditionalBranch,
    UnconditionalBranch,
    Switch,
    Return,
    Throw,
    TrapSetup,
    TrapEnd,
    Label,
    CatchMarker,
    Unknown,
}

/// Exceptions or runtime faults an opcode can cause directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum PossibleException {
    Arithmetic,
    Callee,
    NullReference,
    Bounds,
    Cast,
    DynamicAccess,
    MemoryFault,
    ExplicitThrow,
    AssertionFailure,
    InvalidVirtualDispatch,
    InlineAssembly,
}

/// Declarative semantics for one opcode.
///
/// Register names refer to fields in [`OpcodeMetadata::operands`]. `args` denotes
/// every register in a variadic operand and `this` denotes implicit register 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct OpcodeSemantics {
    pub reads: &'static [&'static str],
    pub writes: &'static [&'static str],
    pub side_effects: &'static [SideEffect],
    pub control_flow: ControlFlowBehavior,
    pub exceptions: &'static [PossibleException],
}

macro_rules! semantic {
    ($reads:expr, $writes:expr) => {
        OpcodeSemantics {
            reads: $reads,
            writes: $writes,
            side_effects: &[],
            control_flow: ControlFlowBehavior::Fallthrough,
            exceptions: &[],
        }
    };
    ($reads:expr, $writes:expr, $effects:expr, $flow:ident, $exceptions:expr) => {
        OpcodeSemantics {
            reads: $reads,
            writes: $writes,
            side_effects: $effects,
            control_flow: ControlFlowBehavior::$flow,
            exceptions: $exceptions,
        }
    };
}

/// Opcode semantics in the same wire order as [`Opcode::METADATA`].
pub const OPCODE_SEMANTICS: &[OpcodeSemantics] = &[
    semantic!(&["src"], &["dst"]),    // Mov
    semantic!(&[], &["dst"]),         // Int
    semantic!(&[], &["dst"]),         // Float
    semantic!(&[], &["dst"]),         // Bool
    semantic!(&[], &["dst"]),         // Bytes
    semantic!(&[], &["dst"]),         // String
    semantic!(&[], &["dst"]),         // Null
    semantic!(&["a", "b"], &["dst"]), // Add
    semantic!(&["a", "b"], &["dst"]), // Sub
    semantic!(&["a", "b"], &["dst"]), // Mul
    semantic!(
        &["a", "b"],
        &["dst"],
        &[],
        Fallthrough,
        &[PossibleException::Arithmetic]
    ), // SDiv
    semantic!(
        &["a", "b"],
        &["dst"],
        &[],
        Fallthrough,
        &[PossibleException::Arithmetic]
    ), // UDiv
    semantic!(
        &["a", "b"],
        &["dst"],
        &[],
        Fallthrough,
        &[PossibleException::Arithmetic]
    ), // SMod
    semantic!(
        &["a", "b"],
        &["dst"],
        &[],
        Fallthrough,
        &[PossibleException::Arithmetic]
    ), // UMod
    semantic!(&["a", "b"], &["dst"]), // Shl
    semantic!(&["a", "b"], &["dst"]), // SShr
    semantic!(&["a", "b"], &["dst"]), // UShr
    semantic!(&["a", "b"], &["dst"]), // And
    semantic!(&["a", "b"], &["dst"]), // Or
    semantic!(&["a", "b"], &["dst"]), // Xor
    semantic!(&["src"], &["dst"]),    // Neg
    semantic!(&["src"], &["dst"]),    // Not
    semantic!(&["dst"], &["dst"]),    // Incr
    semantic!(&["dst"], &["dst"]),    // Decr
    semantic!(
        &[],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // Call0
    semantic!(
        &["arg0"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // Call1
    semantic!(
        &["arg0", "arg1"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // Call2
    semantic!(
        &["arg0", "arg1", "arg2"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // Call3
    semantic!(
        &["arg0", "arg1", "arg2", "arg3"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // Call4
    semantic!(
        &["args"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee]
    ), // CallN
    semantic!(
        &["args"],
        &["dst"],
        &[SideEffect::Call, SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::Callee, PossibleException::NullReference]
    ), // CallMethod
    semantic!(
        &["this", "args"],
        &["dst"],
        &[SideEffect::Call, SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::Callee, PossibleException::NullReference]
    ), // CallThis
    semantic!(
        &["fun", "args"],
        &["dst"],
        &[SideEffect::Call],
        Fallthrough,
        &[PossibleException::Callee, PossibleException::NullReference]
    ), // CallClosure
    semantic!(&[], &["dst"], &[SideEffect::Allocation], Fallthrough, &[]), // StaticClosure
    semantic!(
        &["obj"],
        &["dst"],
        &[SideEffect::Allocation, SideEffect::ReferenceAlias],
        Fallthrough,
        &[]
    ), // InstanceClosure
    semantic!(
        &["obj"],
        &["dst"],
        &[
            SideEffect::Allocation,
            SideEffect::MemoryRead,
            SideEffect::ReferenceAlias
        ],
        Fallthrough,
        &[
            PossibleException::NullReference,
            PossibleException::InvalidVirtualDispatch
        ]
    ), // VirtualClosure
    semantic!(&[], &["dst"], &[SideEffect::GlobalRead], Fallthrough, &[]), // GetGlobal
    semantic!(&["src"], &[], &[SideEffect::GlobalWrite], Fallthrough, &[]), // SetGlobal
    semantic!(
        &["obj"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // Field
    semantic!(
        &["obj", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // SetField
    semantic!(
        &["this"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // GetThis
    semantic!(
        &["this", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // SetThis
    semantic!(
        &["obj"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[
            PossibleException::DynamicAccess,
            PossibleException::NullReference
        ]
    ), // DynGet
    semantic!(
        &["obj", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[
            PossibleException::DynamicAccess,
            PossibleException::NullReference
        ]
    ), // DynSet
    semantic!(&["cond"], &[], &[], ConditionalBranch, &[]), // JTrue
    semantic!(&["cond"], &[], &[], ConditionalBranch, &[]), // JFalse
    semantic!(&["reg"], &[], &[], ConditionalBranch, &[]), // JNull
    semantic!(&["reg"], &[], &[], ConditionalBranch, &[]), // JNotNull
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JSLt
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JSGte
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JSGt
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JSLte
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JULt
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JUGte
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JNotLt
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JNotGte
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JEq
    semantic!(&["a", "b"], &[], &[], ConditionalBranch, &[]), // JNotEq
    semantic!(&[], &[], &[], UnconditionalBranch, &[]), // JAlways
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::Allocation],
        Fallthrough,
        &[]
    ), // ToDyn
    semantic!(&["src"], &["dst"]),    // ToSFloat
    semantic!(&["src"], &["dst"]),    // ToUFloat
    semantic!(&["src"], &["dst"]),    // ToInt
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::Cast]
    ), // SafeCast
    semantic!(&["src"], &["dst"]),    // UnsafeCast
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::Allocation, SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::Cast]
    ), // ToVirtual
    semantic!(&[], &[], &[], Label, &[]), // Label
    semantic!(&["ret"], &[], &[], Return, &[]), // Ret
    semantic!(
        &["exc"],
        &[],
        &[SideEffect::ExceptionState],
        Throw,
        &[PossibleException::ExplicitThrow]
    ), // Throw
    semantic!(
        &["exc"],
        &[],
        &[SideEffect::ExceptionState],
        Throw,
        &[PossibleException::ExplicitThrow]
    ), // Rethrow
    semantic!(&["reg"], &[], &[], Switch, &[]), // Switch
    semantic!(
        &["reg"],
        &[],
        &[],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // NullCheck
    semantic!(&[], &["exc"], &[SideEffect::ExceptionState], TrapSetup, &[]), // Trap
    semantic!(&[], &[], &[SideEffect::ExceptionState], TrapEnd, &[]), // EndTrap
    semantic!(
        &["bytes", "index"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // GetI8
    semantic!(
        &["bytes", "index"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // GetI16
    semantic!(
        &["bytes", "index"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // GetMem
    semantic!(
        &["array", "index"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference, PossibleException::Bounds]
    ), // GetArray
    semantic!(
        &["bytes", "index", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // SetI8
    semantic!(
        &["bytes", "index", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // SetI16
    semantic!(
        &["bytes", "index", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // SetMem
    semantic!(
        &["array", "index", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::NullReference, PossibleException::Bounds]
    ), // SetArray
    semantic!(&[], &["dst"], &[SideEffect::Allocation], Fallthrough, &[]), // New
    semantic!(
        &["array"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // ArraySize
    semantic!(&[], &["dst"]),         // Type
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[]
    ), // GetType (null maps to Void)
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // GetTID
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::ReferenceAlias],
        Fallthrough,
        &[]
    ), // Ref
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::MemoryRead, SideEffect::ReferenceRead],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // Unref
    semantic!(
        &["dst", "value"],
        &[],
        &[SideEffect::MemoryWrite, SideEffect::ReferenceWrite],
        Fallthrough,
        &[PossibleException::MemoryFault]
    ), // Setref
    semantic!(
        &["args"],
        &["dst"],
        &[SideEffect::Allocation],
        Fallthrough,
        &[]
    ), // MakeEnum
    semantic!(&[], &["dst"], &[SideEffect::Allocation], Fallthrough, &[]), // EnumAlloc
    semantic!(
        &["value"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference]
    ), // EnumIndex
    semantic!(
        &["value"],
        &["dst"],
        &[SideEffect::MemoryRead],
        Fallthrough,
        &[PossibleException::NullReference, PossibleException::Bounds]
    ), // EnumField
    semantic!(
        &["value", "src"],
        &[],
        &[SideEffect::MemoryWrite],
        Fallthrough,
        &[PossibleException::NullReference, PossibleException::Bounds]
    ), // SetEnumField
    semantic!(
        &[],
        &[],
        &[SideEffect::DebugBreak],
        Throw,
        &[PossibleException::AssertionFailure]
    ), // Assert
    semantic!(
        &["src"],
        &["dst"],
        &[SideEffect::ReferenceAlias],
        Fallthrough,
        &[]
    ), // RefData
    semantic!(
        &["reg", "offset"],
        &["dst"],
        &[SideEffect::ReferenceAlias],
        Fallthrough,
        &[]
    ), // RefOffset
    semantic!(&[], &[]),              // Nop
    semantic!(&["value"], &[], &[SideEffect::Prefetch], Fallthrough, &[]), // Prefetch
    semantic!(
        &["reg(mode=2)"],
        &["reg(mode=3)"],
        &[SideEffect::InlineAssembly],
        Unknown,
        &[PossibleException::InlineAssembly]
    ), // Asm
    semantic!(&[], &[], &[SideEffect::ExceptionState], CatchMarker, &[]), // Catch
];

/// A diagnostic-friendly operand value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OpcodeOperand {
    pub name: &'static str,
    pub value: String,
}

/// A concrete register operand extracted from an opcode.
///
/// Variadic register operands produce one entry per register. Implicit
/// registers such as `this` and mode-dependent operands such as [`Opcode::Asm`]
/// are described by [`OpcodeSemantics`] and are intentionally not synthesized
/// here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct OpcodeRegisterOperand {
    pub name: &'static str,
    pub register: Reg,
}

/// Opcodes definitions. The fields are the opcode arguments.
///
/// The methods for this struct are generated through a macro because there is no way I would have written code for 98
/// opcodes. The opcode name is directly derived from the variant name. The opcode description is derived from the doc
/// comment on each variant.
///
/// The order of opcodes here is important as it defines the number used for serialization.
#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, hlbc_derive::OpcodeHelper, Serialize, Deserialize)]
pub enum Opcode {
    /// Copy value from *src* into *dst*
    ///
    /// `dst = src`
    Mov {
        dst: Reg,
        src: Reg,
    },
    /// Get an **i32** from the constant pool
    ///
    /// `dst = @ptr`
    Int {
        dst: Reg,
        ptr: RefInt,
    },
    /// Get a **f64** from the constant pool
    ///
    /// `dst = @ptr`
    Float {
        dst: Reg,
        ptr: RefFloat,
    },
    /// Set a **bool** value
    ///
    /// `dst = <true|false>`
    Bool {
        dst: Reg,
        value: InlineBool,
    },
    /// Get a byte array from the constant pool
    ///
    /// `dst = @ptr`
    Bytes {
        dst: Reg,
        ptr: RefBytes,
    },
    /// Get a **string** from the constant pool
    ///
    /// `dst = @ptr`
    String {
        dst: Reg,
        ptr: RefString,
    },
    /// Nullify a register
    ///
    /// `dst = null`
    Null {
        dst: Reg,
    },
    /// Add two numbers
    ///
    /// `dst = a + b`
    Add {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Subtracts two numbers
    ///
    /// `dst = a - b`
    Sub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Multiply two numbers
    ///
    /// `dst = a * b`
    Mul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed division
    ///
    /// `dst = a / b`
    SDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned division
    ///
    /// `dst = a / b`
    UDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed modulo
    ///
    /// `dst = a % b`
    SMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned modulo
    ///
    /// `dst = a % b`
    UMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Shift bits left
    ///
    /// `dst = a << b`
    Shl {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed shift bits right
    ///
    /// `dst = a >> b`
    SShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned shift bits right
    ///
    /// `dst = a >>> b`
    UShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical and
    ///
    /// `dst = a & b`
    And {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical or
    ///
    /// `dst = a | b`
    Or {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical xor
    ///
    /// `dst = a ^ b`
    Xor {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Negate a number
    ///
    /// `dst = -src`
    Neg {
        dst: Reg,
        src: Reg,
    },
    /// Invert a boolean value
    ///
    /// dst = !src`
    Not {
        dst: Reg,
        src: Reg,
    },
    /// Increment a number
    ///
    /// `dst++`
    Incr {
        dst: Reg,
    },
    /// Decrement a number
    ///
    /// `dst--`
    Decr {
        dst: Reg,
    },
    /// Call a function with no argument
    ///
    /// `dst = fun()`
    Call0 {
        dst: Reg,
        fun: RefFun,
    },
    /// Call a function with one argument
    ///
    /// `dst = fun(arg0)`
    Call1 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
    },
    /// Call a function with two arguments
    ///
    /// `dst = fun(arg0, arg1)`
    Call2 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
    },
    /// Call a function with three arguments
    ///
    /// `dst = fun(arg0, arg1, arg2)`
    Call3 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
        arg2: Reg,
    },
    /// Call a function with four arguments
    ///
    /// `dst = fun(arg0, arg1, arg2, arg3)`
    Call4 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
        arg2: Reg,
        arg3: Reg,
    },
    /// Call a function with N arguments
    ///
    /// `dst = fun(arg0, arg1, ...)`
    CallN {
        dst: Reg,
        fun: RefFun,
        args: Vec<Reg>,
    },
    /// Call a function with N arguments, using the first argument as the receiver
    ///
    /// `dst = arg0.field(arg1, arg2, ...)`
    CallMethod {
        dst: Reg,
        field: RefField,
        // obj is the first arg
        args: Vec<Reg>,
    },
    /// Call a function with N arguments.
    ///
    /// `dst = this.field(arg0, arg1, ...)`
    CallThis {
        dst: Reg,
        field: RefField,
        args: Vec<Reg>,
    },
    /// Call a closure with N arguments. Here *fun* is a register.
    ///
    /// `dst = fun(arg0, arg1, ...)`
    CallClosure {
        dst: Reg,
        fun: Reg,
        args: Vec<Reg>,
    },
    /// Create a closure from a function reference.
    ///
    /// `dst = fun`
    StaticClosure {
        dst: Reg,
        fun: RefFun,
    },
    /// Create a closure from an object method.
    ///
    /// `dst = obj.fun`
    InstanceClosure {
        dst: Reg,
        fun: RefFun,
        obj: Reg,
    },
    /// Create a closure from a virtual method prototype and capture its receiver.
    ///
    /// `dst = closure(obj.method)`
    VirtualClosure {
        dst: Reg,
        obj: Reg,
        /// Prototype index in the receiver's object type.
        field: RefField,
    },
    /// Get a global value.
    ///
    /// `dst = @global`
    GetGlobal {
        dst: Reg,
        global: RefGlobal,
    },
    /// Set a global value.
    ///
    /// `@global = src`
    SetGlobal {
        global: RefGlobal,
        src: Reg,
    },
    /// Access an object field
    ///
    /// `dst = obj.field`
    Field {
        dst: Reg,
        obj: Reg,
        field: RefField,
    },
    /// Set an object field
    ///
    /// `obj.field = src`
    SetField {
        obj: Reg,
        field: RefField,
        src: Reg,
    },
    /// Get a field from the *this* instance.
    /// *this* = *reg0*.
    ///
    /// `dst = this.field`
    GetThis {
        dst: Reg,
        field: RefField,
    },
    /// Set a field from the *this* instance.
    /// *this* = *reg0*.
    ///
    /// `dst = this.field`
    SetThis {
        field: RefField,
        src: Reg,
    },
    /// Access a field of a **dyn** instance by its name.
    ///
    /// `dst = obj[field]`
    DynGet {
        dst: Reg,
        obj: Reg,
        field: RefString,
    },
    /// Set a field of a **dyn** instance by its name.
    ///
    /// `obj[field] = src`
    DynSet {
        obj: Reg,
        field: RefString,
        src: Reg,
    },
    /// Jump by an offset if the condition is true
    ///
    /// `if cond jump by offset`
    JTrue {
        cond: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the condition is false
    ///
    /// `if !cond jump by offset`
    JFalse {
        cond: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the value is null
    ///
    /// `if reg == null jump by offset`
    JNull {
        reg: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the value is not null
    ///
    /// `if reg != null jump by offset`
    JNotNull {
        reg: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed lesser than.
    ///
    /// `if a < b jump by offset`
    JSLt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed greater than or equal
    ///
    /// `if a >= b jump by offset`
    JSGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed greater than
    ///
    /// `if a > b jump by offset`
    JSGt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed lesser than or equal
    ///
    /// `if a < b jump by offset`
    JSLte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if unsigned lesser than
    ///
    /// `if a < b jump by offset`
    JULt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if unsigned greater than or equal
    ///
    /// `if a >= b jump by offset`
    JUGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not lesser than
    ///
    /// `if !(a < b) jump by offset`
    JNotLt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not greater than or equal
    ///
    /// `if !(a >= b) jump by offset`
    JNotGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if equal
    ///
    /// `if a == b jump by offset`
    JEq {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not equal
    ///
    /// `if a != b jump by offset`
    JNotEq {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset unconditionally
    ///
    /// `jump by offset`
    JAlways {
        offset: JumpOffset,
    },
    /// Convert a value to a **dyn** value
    ///
    /// `dst = (dyn) src`
    ToDyn {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to a signed **float**
    ///
    /// `dst = (float) src`
    ToSFloat {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to an unsigned **float**
    ///
    /// `dst = (float) src`
    ToUFloat {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to an **int**
    ///
    /// `dst = (int) src`
    ToInt {
        dst: Reg,
        src: Reg,
    },
    /// Cast a value to another type. Throw an exception if the cast is invalid.
    ///
    /// `dst = (typeof dst) src`
    SafeCast {
        dst: Reg,
        src: Reg,
    },
    /// Cast a value to another type. Will not throw an exception. Might crash the program at a later point.
    ///
    /// `dst = (typeof dst) src`
    UnsafeCast {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to a **virtual** value
    ///
    /// `dst = (virtual) src`
    ToVirtual {
        dst: Reg,
        src: Reg,
    },
    /// No-op, mark a position as being the target of a backward jump. Corresponds to a loop.
    ///
    /// Negative jump offsets must always target a label.
    Label,
    /// Return a value from the current function
    ///
    /// `return ret`
    Ret {
        ret: Reg,
    },
    /// Throw an exception
    Throw {
        exc: Reg,
    },
    /// Rethrow an exception, without touching the exception stack trace.
    Rethrow {
        exc: Reg,
    },
    /// Select a jump offset based on the integer value. The offsets array is no bigger than 255.
    /// Values outside the table fall through to the next opcode. `end` marks the opcode after
    /// the complete switch construct for structural analysis; the runtime does not jump to it.
    ///
    /// `jump by offsets[reg] else fall through`
    Switch {
        reg: Reg,
        offsets: Vec<SwitchOffset>,
        end: SwitchOffset,
    },
    /// Throw an exception if *reg* is null.
    ///
    /// `if reg == null throw exception`
    NullCheck {
        reg: Reg,
    },
    /// Setup a try-catch block. If an exception occurs, store it in the given register and jump by an offset.
    Trap {
        exc: Reg,
        offset: JumpOffset,
    },
    /// End the **latest** trap section. `normal` distinguishes the usual end of the try body
    /// from cleanup emitted before an early return, break, or continue. HashLink's JIT does not
    /// otherwise inspect this marker.
    EndTrap {
        normal: InlineBool,
    },
    /// Read an **i8** from a byte array.
    ///
    /// `dst = bytes[index]`
    GetI8 {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Read an **i16** from a byte array.
    ///
    /// `dst = bytes[index]`
    GetI16 {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Read memory directly.
    ///
    /// `dst = bytes[index]`
    GetMem {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Get the value of an array at an index.
    ///
    /// `dst = array[index]`
    GetArray {
        dst: Reg,
        array: Reg,
        index: Reg,
    },
    /// Write an **i8** to a byte array.
    ///
    /// `bytes[index] = src`
    SetI8 {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write an **i16** to a byte array.
    ///
    /// `bytes[index] = src`
    SetI16 {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write to memory directly.
    ///
    /// `bytes[index] = src`
    SetMem {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write a value in an array.
    ///
    /// `array[index] = src`
    SetArray {
        array: Reg,
        index: Reg,
        src: Reg,
    },
    /// Allocate an object.
    ///
    /// `dst = new (typeof dst)`
    New {
        dst: Reg,
    },
    /// Get the length of an array.
    ///
    /// `dst = len(array)`
    ArraySize {
        dst: Reg,
        array: Reg,
    },
    /// Get the type object from its identifier.
    ///
    /// `dst = type ty`
    Type {
        dst: Reg,
        ty: RefType,
    },
    /// Get the type object of a value.
    ///
    /// `dst = typeof src`
    GetType {
        dst: Reg,
        src: Reg,
    },
    /// Get the runtime type kind identifier of a value. Useful for switch statements on types.
    ///
    /// `dst = (typeof src).kind`
    GetTID {
        dst: Reg,
        src: Reg,
    },
    /// Get a reference to a value.
    ///
    /// `dst = &src`
    Ref {
        dst: Reg,
        src: Reg,
    },
    /// Read a reference value.
    ///
    /// `dst = *src`
    Unref {
        dst: Reg,
        src: Reg,
    },
    /// Write into a reference value.
    ///
    /// `*dst = src`
    Setref {
        dst: Reg,
        value: Reg,
    },
    /// Create an enum variant.
    ///
    /// `dst = construct(args...)`
    MakeEnum {
        dst: Reg,
        construct: RefEnumConstruct,
        args: Vec<Reg>,
    },
    /// Create an enum variant using the default values.
    ///
    /// `dst = construct()`
    EnumAlloc {
        dst: Reg,
        construct: RefEnumConstruct,
    },
    /// Get the enum value variant index (the enum tag). Useful for switch statements.
    ///
    /// `dst = variantof value`
    EnumIndex {
        dst: Reg,
        value: Reg,
    },
    /// Access a field of an enum.
    ///
    /// `dst = (value as construct).field`
    EnumField {
        dst: Reg,
        value: Reg,
        construct: RefEnumConstruct,
        field: RefField,
    },
    /// Set a field of an enum. Uses the first enum variant.
    ///
    /// `value.field = src`
    SetEnumField {
        value: Reg,
        field: RefField,
        src: Reg,
    },
    /// Debug break, calls `hl_assert()` under the hood.
    Assert,
    /// Get a reference to the first element of an array's data storage.
    ///
    /// `dst = &src[0]`
    RefData {
        dst: Reg,
        src: Reg,
    },
    /// Offset a reference by `offset` elements of the referenced type.
    ///
    /// `dst = reg + offset * sizeof(*reg)`
    RefOffset {
        dst: Reg,
        reg: Reg,
        offset: Reg,
    },
    /// No-op, useful to mark removed opcodes without breaking jump offsets.
    Nop,
    /// x86 prefetch. Move data closer to the processor using hints.
    Prefetch {
        /// Value to prefetch
        value: Reg,
        /// Encoded field selector: zero prefetches the value, otherwise this is field index + 1.
        field: RefField,
        /// https://github.com/HaxeFoundation/hashlink/blob/733b6a14a0a7e7cfba6c21cdf0ee03595cafafb4/src/jit.c#L4310
        /// https://www.felixcloutier.com/x86/prefetchh
        /// https://www.felixcloutier.com/x86/prefetchw
        mode: InlineInt,
    },
    /// Inline x86 assembly
    Asm {
        /// https://github.com/HaxeFoundation/hashlink/blob/733b6a14a0a7e7cfba6c21cdf0ee03595cafafb4/src/jit.c#L4334
        mode: InlineInt,
        value: InlineInt,
        /// Warning ! Only non-zero values indicates valid reg. Register index is reg-1.
        reg: Reg,
    },
    /// Type metadata marker emitted immediately after [`Opcode::Trap`].
    /// HashLink declares its wire operand with the `J` encoding, but the value
    /// is a global type-object index rather than a CFG branch displacement.
    Catch {
        global: RefGlobal,
    },
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::opcodes::{Opcode, OPCODE_SEMANTICS};
    use crate::types::{RefEnumConstruct, Reg};
    use crate::{AdjustReferences, IndexMapping};

    #[test]
    fn test_doc() {
        assert_eq!(
            "Copy value from *src* into *dst*\n`dst = src`",
            Opcode::Mov {
                dst: Reg(0),
                src: Reg(0),
            }
            .description()
        );
        assert_eq!(
            "Nullify a register\n`dst = null`",
            Opcode::Null { dst: Reg(0) }.description()
        );
    }

    #[test]
    fn every_hashlink_opcode_has_metadata_and_round_trips() -> crate::Result<()> {
        let expected = [
            "Mov",
            "Int",
            "Float",
            "Bool",
            "Bytes",
            "String",
            "Null",
            "Add",
            "Sub",
            "Mul",
            "SDiv",
            "UDiv",
            "SMod",
            "UMod",
            "Shl",
            "SShr",
            "UShr",
            "And",
            "Or",
            "Xor",
            "Neg",
            "Not",
            "Incr",
            "Decr",
            "Call0",
            "Call1",
            "Call2",
            "Call3",
            "Call4",
            "CallN",
            "CallMethod",
            "CallThis",
            "CallClosure",
            "StaticClosure",
            "InstanceClosure",
            "VirtualClosure",
            "GetGlobal",
            "SetGlobal",
            "Field",
            "SetField",
            "GetThis",
            "SetThis",
            "DynGet",
            "DynSet",
            "JTrue",
            "JFalse",
            "JNull",
            "JNotNull",
            "JSLt",
            "JSGte",
            "JSGt",
            "JSLte",
            "JULt",
            "JUGte",
            "JNotLt",
            "JNotGte",
            "JEq",
            "JNotEq",
            "JAlways",
            "ToDyn",
            "ToSFloat",
            "ToUFloat",
            "ToInt",
            "SafeCast",
            "UnsafeCast",
            "ToVirtual",
            "Label",
            "Ret",
            "Throw",
            "Rethrow",
            "Switch",
            "NullCheck",
            "Trap",
            "EndTrap",
            "GetI8",
            "GetI16",
            "GetMem",
            "GetArray",
            "SetI8",
            "SetI16",
            "SetMem",
            "SetArray",
            "New",
            "ArraySize",
            "Type",
            "GetType",
            "GetTID",
            "Ref",
            "Unref",
            "Setref",
            "MakeEnum",
            "EnumAlloc",
            "EnumIndex",
            "EnumField",
            "SetEnumField",
            "Assert",
            "RefData",
            "RefOffset",
            "Nop",
            "Prefetch",
            "Asm",
            "Catch",
        ];

        assert_eq!(Opcode::COUNT, expected.len());
        assert_eq!(Opcode::COUNT, OPCODE_SEMANTICS.len());
        assert_eq!(
            Opcode::METADATA.iter().map(|m| m.name).collect::<Vec<_>>(),
            expected
        );

        for opcode in Opcode::all_defaults() {
            let mut bytes = Vec::new();
            opcode.write(&mut bytes)?;
            let decoded = Opcode::read(&mut Cursor::new(bytes))?;
            assert_eq!(decoded, opcode, "{} did not round-trip", opcode.name());
            assert_eq!(opcode.metadata().name, opcode.name());
            assert_eq!(opcode.metadata().operands.len(), opcode.operands().len());
            assert_eq!(
                opcode.metadata().semantics,
                &OPCODE_SEMANTICS[opcode.code() as usize]
            );
        }
        Ok(())
    }

    #[test]
    fn enum_construct_references_are_type_local_during_merge() {
        let mut opcode = Opcode::MakeEnum {
            dst: Reg(0),
            construct: RefEnumConstruct(2),
            args: vec![],
        };
        opcode.adjust_references(&IndexMapping {
            type_offset: 17,
            ..IndexMapping::default()
        });
        assert!(matches!(
            opcode,
            Opcode::MakeEnum {
                construct: RefEnumConstruct(2),
                ..
            }
        ));
    }

    #[test]
    fn end_trap_operand_is_a_boolean_marker_not_a_register() -> crate::Result<()> {
        for normal in [false, true] {
            let opcode = Opcode::EndTrap { normal };
            let mut bytes = Vec::new();
            opcode.write(&mut bytes)?;
            assert_eq!(bytes.last(), Some(&(normal as u8)));
            assert_eq!(Opcode::read(&mut Cursor::new(bytes))?, opcode);
            assert!(opcode.metadata().semantics.reads.is_empty());
            assert!(opcode.metadata().semantics.writes.is_empty());
        }
        Ok(())
    }
}

impl AdjustReferences for Opcode {
    fn adjust_references(&mut self, mapping: &IndexMapping) {
        match self {
            // Constant pool references
            Opcode::Int { ptr, .. } => ptr.adjust(mapping.int_offset),
            Opcode::Float { ptr, .. } => ptr.adjust(mapping.float_offset),
            Opcode::String { ptr, .. } => mapping.apply_string(ptr),
            Opcode::Bytes { ptr, .. } => ptr.adjust(mapping.bytes_offset),

            // Function calls
            Opcode::Call0 { fun, .. }
            | Opcode::Call1 { fun, .. }
            | Opcode::Call2 { fun, .. }
            | Opcode::Call3 { fun, .. }
            | Opcode::Call4 { fun, .. }
            | Opcode::CallN { fun, .. } => {
                fun.adjust(mapping.function_offset);
            }

            // Closures
            Opcode::StaticClosure { fun, .. } | Opcode::InstanceClosure { fun, .. } => {
                fun.adjust(mapping.function_offset);
            }

            // Global access
            Opcode::GetGlobal { global, .. }
            | Opcode::SetGlobal { global, .. }
            | Opcode::Catch { global } => {
                global.adjust(mapping.global_offset);
            }

            // Type operations
            Opcode::Type { ty, .. } => ty.adjust(mapping.type_offset),

            // Field operations - need field mapping
            Opcode::Field { field, .. }
            | Opcode::SetField { field, .. }
            | Opcode::GetThis { field, .. }
            | Opcode::SetThis { field, .. }
            | Opcode::CallMethod { field, .. }
            | Opcode::CallThis { field, .. } => {
                if let Some(&mapped_field) = mapping.field_mappings.get(field) {
                    *field = mapped_field;
                }
            }

            // Dynamic field access
            Opcode::DynGet { field, .. } | Opcode::DynSet { field, .. } => {
                mapping.apply_string(field);
            }

            Opcode::SetEnumField { field, .. } => {
                if let Some(&mapped_field) = mapping.field_mappings.get(field) {
                    *field = mapped_field;
                }
            }

            // Prefetch with field reference
            Opcode::Prefetch { field, .. } if field.0 > 0 => {
                let decoded = RefField(field.0 - 1);
                if let Some(&mapped_field) = mapping.field_mappings.get(&decoded) {
                    field.0 = mapped_field.0 + 1;
                }
            }

            // All other opcodes don't have references to adjust
            _ => {}
        }
    }
}

//! Typed, effect-aware SSA IR between HashLink bytecode and the Haxe AST.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt;

use hlbc::opcodes::{ControlFlowBehavior, Opcode, PossibleException, SideEffect};
use hlbc::types::{Function, RefField, RefGlobal, RefString, RefType, Reg, Type};
use hlbc::Bytecode;
use serde::Serialize;

use crate::cfg::{ControlFlowGraph, DominatorInfo, NodeId};
use crate::diagnostics::{DecompileError, Decompiled, Diagnostic, DiagnosticSeverity};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct ValueId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct OperationId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct LocalId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct OpcodeRange {
    pub start: usize,
    pub end: usize,
}

impl OpcodeRange {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn len(self) -> usize {
        self.end.saturating_sub(self.start)
    }

    pub const fn is_empty(self) -> bool {
        self.start >= self.end
    }
}

/// Exact bytecode origins for a real or synthetic IR node.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IrProvenance {
    pub function_index: usize,
    pub opcode_ranges: Vec<OpcodeRange>,
    pub synthetic: bool,
}

impl IrProvenance {
    pub fn opcode(function_index: usize, opcode: usize) -> Self {
        Self::new(
            function_index,
            vec![OpcodeRange::new(opcode, opcode + 1)],
            false,
        )
    }

    pub fn range(function_index: usize, start: usize, end: usize, synthetic: bool) -> Self {
        Self::new(
            function_index,
            vec![OpcodeRange::new(start, end)],
            synthetic,
        )
    }

    pub fn boundary(function_index: usize, opcode: usize) -> Self {
        Self::range(function_index, opcode, opcode, true)
    }

    pub fn new(function_index: usize, opcode_ranges: Vec<OpcodeRange>, synthetic: bool) -> Self {
        Self {
            function_index,
            opcode_ranges: normalize_ranges(opcode_ranges),
            synthetic,
        }
    }

    fn union<'a>(
        function_index: usize,
        provenances: impl IntoIterator<Item = &'a IrProvenance>,
    ) -> Self {
        let ranges = provenances
            .into_iter()
            .flat_map(|provenance| provenance.opcode_ranges.iter().copied())
            .collect();
        Self::new(function_index, ranges, true)
    }
}

fn normalize_ranges(mut ranges: Vec<OpcodeRange>) -> Vec<OpcodeRange> {
    ranges.sort_by_key(|range| (range.start, range.end));
    let mut normalized: Vec<OpcodeRange> = Vec::new();
    for range in ranges {
        if let Some(previous) = normalized.last_mut() {
            if range.start <= previous.end {
                previous.end = previous.end.max(range.end);
                continue;
            }
        }
        normalized.push(range);
    }
    normalized
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum IrType {
    HashLink(RefType),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum AccessMode {
    Read,
    Write,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MemoryWidth {
    I8,
    I16,
    Native,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum FieldSelector {
    Indexed(RefField),
    Dynamic(RefString),
    This(RefField),
}

/// IR-level effects. These refine the lower-level opcode side-effect table.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum IrEffect {
    PureValue,
    Call,
    Allocation,
    RawMemory {
        access: AccessMode,
        width: MemoryWidth,
    },
    Global {
        access: AccessMode,
        global: RefGlobal,
    },
    Reference {
        access: AccessMode,
    },
    ReferenceAlias,
    ObjectField {
        access: AccessMode,
        field: FieldSelector,
    },
    ArrayElement {
        access: AccessMode,
    },
    ArrayMetadataRead,
    EnumField {
        access: AccessMode,
        field: Option<RefField>,
    },
    RuntimeMetadataRead,
    ExceptionState,
    ControlFlow,
    DebugBreak,
    Prefetch,
    InlineAssembly,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum IrOperationKind {
    PureValue,
    Call,
    Allocation,
    Global,
    ObjectField,
    ArrayElement,
    RawMemory,
    Reference,
    Enum,
    RuntimeMetadata,
    RuntimeCheck,
    ControlFlow,
    Prefetch,
    Marker,
    Unsupported { reason: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ValueDefinition {
    Parameter {
        index: usize,
    },
    Operation {
        block: usize,
        operation: OperationId,
        result_index: usize,
    },
    Phi {
        block: usize,
        phi_index: usize,
    },
    /// A recovery definition paired with a construction diagnostic.
    Undefined {
        block: Option<usize>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum UseSite {
    Operation {
        block: usize,
        operation: OperationId,
        input_index: usize,
    },
    Phi {
        block: usize,
        phi_index: usize,
        input_index: usize,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct IrValue {
    pub id: ValueId,
    pub register: Reg,
    pub version: u32,
    pub ty: IrType,
    pub definition: ValueDefinition,
    pub uses: Vec<UseSite>,
    pub debug_name: Option<String>,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrUse {
    pub role: String,
    pub register: Reg,
    pub value: ValueId,
    pub ty: IrType,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct PhiInput {
    pub predecessor: NodeId,
    pub value: ValueId,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrPhi {
    pub register: Reg,
    pub result: ValueId,
    pub inputs: Vec<PhiInput>,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrOperation {
    pub id: OperationId,
    pub opcode: Opcode,
    pub kind: IrOperationKind,
    pub inputs: Vec<IrUse>,
    pub results: Vec<ValueId>,
    pub effects: Vec<IrEffect>,
    pub semantic_side_effects: Vec<SideEffect>,
    pub exceptions: Vec<PossibleException>,
    pub control_flow: ControlFlowBehavior,
    /// Position in the source-order chain of effectful or throwing operations.
    pub effect_order: Option<usize>,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrBlock {
    pub id: usize,
    pub predecessors: BTreeSet<NodeId>,
    pub successors: BTreeSet<NodeId>,
    pub phis: Vec<IrPhi>,
    pub operations: Vec<IrOperation>,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum IrRegionKind {
    Function,
    BasicBlock { block: usize },
    NaturalLoop { header: NodeId, latch: NodeId },
    Exception { handler_opcode: usize },
    Irreducible,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrRegion {
    pub kind: IrRegionKind,
    pub blocks: BTreeSet<usize>,
    pub provenance: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct IrLocal {
    pub id: LocalId,
    pub value: ValueId,
    pub register: Reg,
    pub version: u32,
    pub ty: IrType,
    pub debug_name: Option<String>,
    pub lifetime: IrProvenance,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedIr {
    pub function_index: usize,
    pub opcode_count: usize,
    pub register_types: Vec<IrType>,
    pub cfg: ControlFlowGraph,
    pub blocks: Vec<IrBlock>,
    pub values: Vec<IrValue>,
    pub locals: Vec<IrLocal>,
    pub regions: Vec<IrRegion>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IrVerificationError {
    pub provenance: IrProvenance,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, Serialize)]
#[error("IR verification failed with {} error(s)", .errors.len())]
pub struct IrVerificationErrors {
    pub errors: Vec<IrVerificationError>,
}

impl TypedIr {
    /// Construct and verify typed SSA IR for a bytecode function.
    pub fn build(code: &Bytecode, function: &Function) -> Result<Decompiled<Self>, DecompileError> {
        let function_index = function.findex.0;
        let cfg =
            ControlFlowGraph::build_with_index(function, function_index).map_err(|error| {
                DecompileError::new(vec![Diagnostic::fatal(function_index, error.to_string())])
            })?;
        Ok(Self::build_with_cfg(code, function, cfg))
    }

    pub(crate) fn build_with_cfg(
        code: &Bytecode,
        function: &Function,
        cfg: ControlFlowGraph,
    ) -> Decompiled<Self> {
        Builder::new(code, function, cfg).build()
    }

    pub fn operation(&self, id: OperationId) -> Option<&IrOperation> {
        self.blocks
            .iter()
            .flat_map(|block| block.operations.iter())
            .find(|operation| operation.id == id)
    }

    /// Return the original opcode stream as preserved by typed IR operations.
    /// This is the compatibility boundary used while AST lowering migrates one
    /// opcode family at a time.
    pub fn bytecode_compatibility_stream(&self) -> Vec<Opcode> {
        let mut operations = self
            .blocks
            .iter()
            .flat_map(|block| block.operations.iter())
            .collect::<Vec<_>>();
        operations.sort_by_key(|operation| operation.id);
        operations
            .into_iter()
            .map(|operation| operation.opcode.clone())
            .collect()
    }

    pub fn verify(&self) -> Result<(), IrVerificationErrors> {
        let mut verifier = Verifier {
            ir: self,
            errors: Vec::new(),
        };
        verifier.verify();
        if verifier.errors.is_empty() {
            Ok(())
        } else {
            Err(IrVerificationErrors {
                errors: verifier.errors,
            })
        }
    }

    pub(crate) fn value_dominates_operation(
        &self,
        value: ValueId,
        use_block: usize,
        use_operation: OperationId,
        dominators: &DominatorInfo,
    ) -> bool {
        let Some(value) = self.values.get(value.0) else {
            return false;
        };
        match value.definition {
            ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None } => {
                dominators
                    .sets
                    .get(&NodeId::Block(use_block))
                    .map_or(false, |set| set.contains(&NodeId::Entry))
            }
            ValueDefinition::Phi { block, .. }
            | ValueDefinition::Undefined { block: Some(block) } => {
                node_dominates(block, use_block, dominators)
            }
            ValueDefinition::Operation {
                block, operation, ..
            } => {
                node_dominates(block, use_block, dominators)
                    && (block != use_block || operation.0 < use_operation.0)
            }
        }
    }

    pub(crate) fn value_dominates_edge(
        &self,
        value: ValueId,
        predecessor: NodeId,
        dominators: &DominatorInfo,
    ) -> bool {
        let Some(value) = self.values.get(value.0) else {
            return false;
        };
        match predecessor {
            NodeId::Entry => matches!(
                value.definition,
                ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None }
            ),
            NodeId::Block(predecessor_block) => match value.definition {
                ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None } => {
                    dominators
                        .sets
                        .get(&predecessor)
                        .map_or(false, |set| set.contains(&NodeId::Entry))
                }
                ValueDefinition::Phi { block, .. }
                | ValueDefinition::Undefined { block: Some(block) } => {
                    node_dominates(block, predecessor_block, dominators)
                }
                ValueDefinition::Operation { block, .. } => {
                    node_dominates(block, predecessor_block, dominators)
                }
            },
            NodeId::Exit => false,
        }
    }
}

#[derive(Debug, Clone)]
struct NamedRegister {
    role: String,
    register: Reg,
}

fn semantic_registers(opcode: &Opcode, roles: &[&str]) -> Vec<NamedRegister> {
    let concrete = opcode.register_operands();
    let mut result = Vec::new();
    for &role in roles {
        match role {
            "this" => result.push(NamedRegister {
                role: role.to_owned(),
                register: Reg(0),
            }),
            "args" => result.extend(
                concrete
                    .iter()
                    .filter(|operand| operand.name == "args")
                    .map(|operand| NamedRegister {
                        role: role.to_owned(),
                        register: operand.register,
                    }),
            ),
            _ if role.starts_with("reg(mode=") => {
                if let Opcode::Asm { mode, reg, .. } = opcode {
                    let required_mode = role
                        .strip_prefix("reg(mode=")
                        .and_then(|value| value.strip_suffix(')'))
                        .and_then(|value| value.parse::<i32>().ok());
                    if required_mode == Some(*mode) {
                        if let Some(register) = reg.0.checked_sub(1) {
                            result.push(NamedRegister {
                                role: role.to_owned(),
                                register: Reg(register),
                            });
                        }
                    }
                }
            }
            _ => result.extend(concrete.iter().filter(|operand| operand.name == role).map(
                |operand| NamedRegister {
                    role: role.to_owned(),
                    register: operand.register,
                },
            )),
        }
    }
    result
}

fn operation_kind(opcode: &Opcode) -> IrOperationKind {
    match opcode {
        Opcode::Mov { .. }
        | Opcode::Int { .. }
        | Opcode::Float { .. }
        | Opcode::Bool { .. }
        | Opcode::Bytes { .. }
        | Opcode::String { .. }
        | Opcode::Null { .. }
        | Opcode::Add { .. }
        | Opcode::Sub { .. }
        | Opcode::Mul { .. }
        | Opcode::SDiv { .. }
        | Opcode::UDiv { .. }
        | Opcode::SMod { .. }
        | Opcode::UMod { .. }
        | Opcode::Shl { .. }
        | Opcode::SShr { .. }
        | Opcode::UShr { .. }
        | Opcode::And { .. }
        | Opcode::Or { .. }
        | Opcode::Xor { .. }
        | Opcode::Neg { .. }
        | Opcode::Not { .. }
        | Opcode::Incr { .. }
        | Opcode::Decr { .. }
        | Opcode::ToSFloat { .. }
        | Opcode::ToUFloat { .. }
        | Opcode::ToInt { .. }
        | Opcode::UnsafeCast { .. } => IrOperationKind::PureValue,
        Opcode::Call0 { .. }
        | Opcode::Call1 { .. }
        | Opcode::Call2 { .. }
        | Opcode::Call3 { .. }
        | Opcode::Call4 { .. }
        | Opcode::CallN { .. }
        | Opcode::CallMethod { .. }
        | Opcode::CallThis { .. }
        | Opcode::CallClosure { .. } => IrOperationKind::Call,
        Opcode::StaticClosure { .. }
        | Opcode::InstanceClosure { .. }
        | Opcode::VirtualClosure { .. }
        | Opcode::ToDyn { .. }
        | Opcode::ToVirtual { .. }
        | Opcode::New { .. } => IrOperationKind::Allocation,
        Opcode::GetGlobal { .. } | Opcode::SetGlobal { .. } => IrOperationKind::Global,
        Opcode::Field { .. }
        | Opcode::SetField { .. }
        | Opcode::GetThis { .. }
        | Opcode::SetThis { .. }
        | Opcode::DynGet { .. }
        | Opcode::DynSet { .. } => IrOperationKind::ObjectField,
        Opcode::JTrue { .. }
        | Opcode::JFalse { .. }
        | Opcode::JNull { .. }
        | Opcode::JNotNull { .. }
        | Opcode::JSLt { .. }
        | Opcode::JSGte { .. }
        | Opcode::JSGt { .. }
        | Opcode::JSLte { .. }
        | Opcode::JULt { .. }
        | Opcode::JUGte { .. }
        | Opcode::JNotLt { .. }
        | Opcode::JNotGte { .. }
        | Opcode::JEq { .. }
        | Opcode::JNotEq { .. }
        | Opcode::JAlways { .. }
        | Opcode::Ret { .. }
        | Opcode::Throw { .. }
        | Opcode::Rethrow { .. }
        | Opcode::Switch { .. }
        | Opcode::Trap { .. }
        | Opcode::EndTrap { .. } => IrOperationKind::ControlFlow,
        Opcode::Label | Opcode::Catch { .. } | Opcode::Nop => IrOperationKind::Marker,
        Opcode::SafeCast { .. } | Opcode::NullCheck { .. } | Opcode::Assert => {
            IrOperationKind::RuntimeCheck
        }
        Opcode::GetI8 { .. }
        | Opcode::GetI16 { .. }
        | Opcode::GetMem { .. }
        | Opcode::SetI8 { .. }
        | Opcode::SetI16 { .. }
        | Opcode::SetMem { .. } => IrOperationKind::RawMemory,
        Opcode::GetArray { .. } | Opcode::SetArray { .. } | Opcode::ArraySize { .. } => {
            IrOperationKind::ArrayElement
        }
        Opcode::Type { .. } | Opcode::GetType { .. } | Opcode::GetTID { .. } => {
            IrOperationKind::RuntimeMetadata
        }
        Opcode::Ref { .. }
        | Opcode::Unref { .. }
        | Opcode::Setref { .. }
        | Opcode::RefData { .. }
        | Opcode::RefOffset { .. } => IrOperationKind::Reference,
        Opcode::MakeEnum { .. }
        | Opcode::EnumAlloc { .. }
        | Opcode::EnumIndex { .. }
        | Opcode::EnumField { .. }
        | Opcode::SetEnumField { .. } => IrOperationKind::Enum,
        Opcode::Prefetch { .. } => IrOperationKind::Prefetch,
        Opcode::Asm { .. } => IrOperationKind::Unsupported {
            reason: "inline assembly has target-specific register semantics".to_owned(),
        },
    }
}

fn operation_effects(opcode: &Opcode) -> Vec<IrEffect> {
    use AccessMode::{Read, Write};
    match opcode {
        Opcode::CallMethod { field, .. } | Opcode::CallThis { field, .. } => vec![
            IrEffect::ObjectField {
                access: Read,
                field: FieldSelector::Indexed(*field),
            },
            IrEffect::Call,
        ],
        Opcode::Call0 { .. }
        | Opcode::Call1 { .. }
        | Opcode::Call2 { .. }
        | Opcode::Call3 { .. }
        | Opcode::Call4 { .. }
        | Opcode::CallN { .. }
        | Opcode::CallClosure { .. } => vec![IrEffect::Call],
        Opcode::StaticClosure { .. } | Opcode::ToDyn { .. } | Opcode::ToVirtual { .. } => {
            vec![IrEffect::Allocation]
        }
        Opcode::InstanceClosure { .. } => {
            vec![IrEffect::Allocation, IrEffect::ReferenceAlias]
        }
        Opcode::VirtualClosure { field, .. } => vec![
            IrEffect::Allocation,
            IrEffect::ObjectField {
                access: Read,
                field: FieldSelector::Indexed(*field),
            },
            IrEffect::ReferenceAlias,
        ],
        Opcode::GetGlobal { global, .. } => vec![IrEffect::Global {
            access: Read,
            global: *global,
        }],
        Opcode::SetGlobal { global, .. } => vec![IrEffect::Global {
            access: Write,
            global: *global,
        }],
        Opcode::Field { field, .. } => vec![IrEffect::ObjectField {
            access: Read,
            field: FieldSelector::Indexed(*field),
        }],
        Opcode::SetField { field, .. } => vec![IrEffect::ObjectField {
            access: Write,
            field: FieldSelector::Indexed(*field),
        }],
        Opcode::GetThis { field, .. } => vec![IrEffect::ObjectField {
            access: Read,
            field: FieldSelector::This(*field),
        }],
        Opcode::SetThis { field, .. } => vec![IrEffect::ObjectField {
            access: Write,
            field: FieldSelector::This(*field),
        }],
        Opcode::DynGet { field, .. } => vec![IrEffect::ObjectField {
            access: Read,
            field: FieldSelector::Dynamic(*field),
        }],
        Opcode::DynSet { field, .. } => vec![IrEffect::ObjectField {
            access: Write,
            field: FieldSelector::Dynamic(*field),
        }],
        Opcode::GetI8 { .. } => vec![IrEffect::RawMemory {
            access: Read,
            width: MemoryWidth::I8,
        }],
        Opcode::GetI16 { .. } => vec![IrEffect::RawMemory {
            access: Read,
            width: MemoryWidth::I16,
        }],
        Opcode::GetMem { .. } => vec![IrEffect::RawMemory {
            access: Read,
            width: MemoryWidth::Native,
        }],
        Opcode::SetI8 { .. } => vec![IrEffect::RawMemory {
            access: Write,
            width: MemoryWidth::I8,
        }],
        Opcode::SetI16 { .. } => vec![IrEffect::RawMemory {
            access: Write,
            width: MemoryWidth::I16,
        }],
        Opcode::SetMem { .. } => vec![IrEffect::RawMemory {
            access: Write,
            width: MemoryWidth::Native,
        }],
        Opcode::GetArray { .. } => vec![IrEffect::ArrayElement { access: Read }],
        Opcode::SetArray { .. } => vec![IrEffect::ArrayElement { access: Write }],
        Opcode::ArraySize { .. } => vec![IrEffect::ArrayMetadataRead],
        Opcode::Ref { .. } | Opcode::RefData { .. } | Opcode::RefOffset { .. } => {
            vec![IrEffect::ReferenceAlias]
        }
        Opcode::Unref { .. } => vec![IrEffect::Reference { access: Read }],
        Opcode::Setref { .. } => vec![IrEffect::Reference { access: Write }],
        Opcode::MakeEnum { .. } | Opcode::EnumAlloc { .. } | Opcode::New { .. } => {
            vec![IrEffect::Allocation]
        }
        Opcode::EnumIndex { .. } => vec![IrEffect::EnumField {
            access: Read,
            field: None,
        }],
        Opcode::EnumField { field, .. } => vec![IrEffect::EnumField {
            access: Read,
            field: Some(*field),
        }],
        Opcode::SetEnumField { field, .. } => vec![IrEffect::EnumField {
            access: Write,
            field: Some(*field),
        }],
        Opcode::Type { .. } | Opcode::GetType { .. } | Opcode::GetTID { .. } => {
            vec![IrEffect::RuntimeMetadataRead]
        }
        Opcode::Throw { .. }
        | Opcode::Rethrow { .. }
        | Opcode::Trap { .. }
        | Opcode::EndTrap { .. }
        | Opcode::Catch { .. } => vec![IrEffect::ExceptionState, IrEffect::ControlFlow],
        Opcode::JTrue { .. }
        | Opcode::JFalse { .. }
        | Opcode::JNull { .. }
        | Opcode::JNotNull { .. }
        | Opcode::JSLt { .. }
        | Opcode::JSGte { .. }
        | Opcode::JSGt { .. }
        | Opcode::JSLte { .. }
        | Opcode::JULt { .. }
        | Opcode::JUGte { .. }
        | Opcode::JNotLt { .. }
        | Opcode::JNotGte { .. }
        | Opcode::JEq { .. }
        | Opcode::JNotEq { .. }
        | Opcode::JAlways { .. }
        | Opcode::Ret { .. }
        | Opcode::Switch { .. }
        | Opcode::Label => vec![IrEffect::ControlFlow],
        Opcode::Assert => vec![IrEffect::DebugBreak, IrEffect::ControlFlow],
        Opcode::Prefetch { .. } => vec![IrEffect::Prefetch],
        Opcode::Asm { .. } => vec![IrEffect::InlineAssembly],
        Opcode::SafeCast { .. } | Opcode::NullCheck { .. } => vec![IrEffect::PureValue],
        Opcode::Mov { .. }
        | Opcode::Int { .. }
        | Opcode::Float { .. }
        | Opcode::Bool { .. }
        | Opcode::Bytes { .. }
        | Opcode::String { .. }
        | Opcode::Null { .. }
        | Opcode::Add { .. }
        | Opcode::Sub { .. }
        | Opcode::Mul { .. }
        | Opcode::SDiv { .. }
        | Opcode::UDiv { .. }
        | Opcode::SMod { .. }
        | Opcode::UMod { .. }
        | Opcode::Shl { .. }
        | Opcode::SShr { .. }
        | Opcode::UShr { .. }
        | Opcode::And { .. }
        | Opcode::Or { .. }
        | Opcode::Xor { .. }
        | Opcode::Neg { .. }
        | Opcode::Not { .. }
        | Opcode::Incr { .. }
        | Opcode::Decr { .. }
        | Opcode::ToSFloat { .. }
        | Opcode::ToUFloat { .. }
        | Opcode::ToInt { .. }
        | Opcode::UnsafeCast { .. }
        | Opcode::Nop => vec![IrEffect::PureValue],
    }
}

fn needs_effect_order(opcode: &Opcode) -> bool {
    let semantics = opcode.metadata().semantics;
    !semantics.side_effects.is_empty()
        || !semantics.exceptions.is_empty()
        || semantics.control_flow != ControlFlowBehavior::Fallthrough
}

struct Builder<'a> {
    code: &'a Bytecode,
    function: &'a Function,
    cfg: ControlFlowGraph,
    diagnostics: Vec<Diagnostic>,
    values: Vec<IrValue>,
    versions: HashMap<Reg, u32>,
    diagnosed_unknown_types: HashSet<(usize, Reg)>,
}

impl<'a> Builder<'a> {
    fn new(code: &'a Bytecode, function: &'a Function, cfg: ControlFlowGraph) -> Self {
        Self {
            code,
            function,
            cfg,
            diagnostics: Vec::new(),
            values: Vec::new(),
            versions: HashMap::new(),
            diagnosed_unknown_types: HashSet::new(),
        }
    }

    fn build(mut self) -> Decompiled<TypedIr> {
        let function_index = self.function.findex.0;
        let register_types = self
            .function
            .regs
            .iter()
            .map(|ty| {
                if ty.0 < self.code.types.len() {
                    IrType::HashLink(*ty)
                } else {
                    IrType::Unknown
                }
            })
            .collect::<Vec<_>>();

        let parameter_count = self
            .code
            .types
            .get(self.function.t.0)
            .and_then(Type::get_type_fun)
            .map_or(0, |function_type| function_type.args.len())
            .min(self.function.regs.len());
        let parameter_names = parameter_names(self.code, self.function, parameter_count);

        let mut stacks: HashMap<Reg, Vec<ValueId>> = HashMap::new();
        for index in 0..parameter_count {
            let register = Reg(index as u32);
            let value = self.new_value(
                register,
                ValueDefinition::Parameter { index },
                parameter_names.get(&register).cloned().flatten(),
                IrProvenance::boundary(function_index, 0),
            );
            stacks.entry(register).or_default().push(value);
        }

        let phi_registers = place_phis(self.function, &self.cfg, parameter_count);
        let mut blocks = self
            .cfg
            .blocks
            .iter()
            .map(|block| IrBlock {
                id: block.id,
                predecessors: block.predecessors.clone(),
                successors: block.successors.clone(),
                phis: Vec::new(),
                operations: Vec::new(),
                provenance: IrProvenance::range(function_index, block.start, block.end, true),
            })
            .collect::<Vec<_>>();

        for (block_id, registers) in phi_registers.iter().enumerate() {
            for (phi_index, &register_index) in registers.iter().enumerate() {
                let register = Reg(register_index);
                let provenance =
                    IrProvenance::boundary(function_index, self.cfg.blocks[block_id].start);
                let result = self.new_value(
                    register,
                    ValueDefinition::Phi {
                        block: block_id,
                        phi_index,
                    },
                    None,
                    provenance.clone(),
                );
                blocks[block_id].phis.push(IrPhi {
                    register,
                    result,
                    inputs: Vec::new(),
                    provenance,
                });
            }
        }

        let dominators = self.cfg.dominators();
        let mut dom_children: BTreeMap<NodeId, Vec<NodeId>> = BTreeMap::new();
        for (&node, &parent) in &dominators.immediate {
            if let Some(parent) = parent {
                dom_children.entry(parent).or_default().push(node);
            }
        }
        for children in dom_children.values_mut() {
            children.sort();
        }

        let mut effect_orders = vec![None; self.function.ops.len()];
        let mut next_effect_order = 0;
        for (index, opcode) in self.function.ops.iter().enumerate() {
            if needs_effect_order(opcode) {
                effect_orders[index] = Some(next_effect_order);
                next_effect_order += 1;
            }
        }

        let mut renamer = Renamer {
            builder: &mut self,
            blocks: &mut blocks,
            stacks,
            dom_children,
            visited: BTreeSet::new(),
            effect_orders,
        };
        renamer.add_successor_phi_inputs(NodeId::Entry, None, None);
        renamer.visit_children(NodeId::Entry);

        // Disconnected bytecode is preserved. It is renamed as an independent
        // root so no reachable definition is accidentally allowed to dominate it.
        for block_id in 0..renamer.blocks.len() {
            if !renamer.visited.contains(&block_id) {
                renamer.stacks.clear();
                renamer.visit_block(block_id);
            }
        }

        drop(renamer);
        finalize_phi_provenance(function_index, &mut blocks, &mut self.values);
        rebuild_uses(&blocks, &mut self.values);
        recover_phi_names(&blocks, &mut self.values);
        let locals = recover_locals(function_index, &blocks, &self.values);
        let regions = build_regions(function_index, &self.cfg);

        let ir = TypedIr {
            function_index,
            opcode_count: self.function.ops.len(),
            register_types,
            cfg: self.cfg,
            blocks,
            values: self.values,
            locals,
            regions,
        };
        if let Err(errors) = ir.verify() {
            for error in errors.errors {
                let opcode_index = error
                    .provenance
                    .opcode_ranges
                    .first()
                    .map_or(0, |range| range.start)
                    .min(self.function.ops.len().saturating_sub(1));
                if let Some(opcode) = self.function.ops.get(opcode_index) {
                    self.diagnostics.push(Diagnostic::for_opcode(
                        DiagnosticSeverity::Fatal,
                        self.code,
                        function_index,
                        self.function,
                        opcode_index,
                        opcode,
                        format!("IR verifier: {}", error.message),
                    ));
                } else {
                    self.diagnostics.push(Diagnostic::fatal(
                        function_index,
                        format!("IR verifier: {}", error.message),
                    ));
                }
            }
        }
        Decompiled::new(ir, self.diagnostics)
    }

    fn new_value(
        &mut self,
        register: Reg,
        definition: ValueDefinition,
        debug_name: Option<String>,
        provenance: IrProvenance,
    ) -> ValueId {
        let version = self.versions.entry(register).or_insert(0);
        let id = ValueId(self.values.len());
        let ty = self
            .function
            .regs
            .get(register.0 as usize)
            .copied()
            .filter(|ty| ty.0 < self.code.types.len())
            .map_or(IrType::Unknown, IrType::HashLink);
        self.values.push(IrValue {
            id,
            register,
            version: *version,
            ty,
            definition,
            uses: Vec::new(),
            debug_name,
            provenance,
        });
        *version += 1;
        id
    }

    fn diagnose_unknown_type(&mut self, opcode_index: usize, opcode: &Opcode, register: Reg) {
        if self
            .diagnosed_unknown_types
            .insert((opcode_index, register))
        {
            self.diagnostics.push(Diagnostic::for_opcode(
                DiagnosticSeverity::Fatal,
                self.code,
                self.function.findex.0,
                self.function,
                opcode_index,
                opcode,
                format!(
                    "register r{} has no valid declared HashLink type",
                    register.0
                ),
            ));
        }
    }
}

fn parameter_names(
    code: &Bytecode,
    function: &Function,
    parameter_count: usize,
) -> HashMap<Reg, Option<String>> {
    let mut result = HashMap::new();
    let is_this = function.is_method()
        || code
            .strings
            .get(function.name.0)
            .map_or(false, |name| name.as_ref() == "__constructor__");
    let first_named = usize::from(is_this);
    if is_this && parameter_count > 0 {
        result.insert(Reg(0), Some("this".to_owned()));
    }
    let names = function
        .assigns
        .iter()
        .flatten()
        .filter(|(_, position)| *position == 0)
        .filter_map(|(name, _)| code.strings.get(name.0).map(ToString::to_string));
    for (register, name) in (first_named..parameter_count).zip(names) {
        result.insert(Reg(register as u32), Some(name));
    }
    result
}

fn debug_name_at(code: &Bytecode, function: &Function, opcode_index: usize) -> Option<String> {
    function
        .assigns
        .as_ref()?
        .iter()
        .find_map(|(name, position)| {
            (*position == opcode_index + 1)
                .then(|| code.strings.get(name.0).map(ToString::to_string))
                .flatten()
        })
}

fn place_phis(
    function: &Function,
    cfg: &ControlFlowGraph,
    parameter_count: usize,
) -> Vec<BTreeSet<u32>> {
    let dominance_frontiers = dominance_frontiers(cfg);
    let mut definitions: BTreeMap<u32, BTreeSet<NodeId>> = BTreeMap::new();
    for register in 0..parameter_count {
        definitions
            .entry(register as u32)
            .or_default()
            .insert(NodeId::Entry);
    }
    for block in &cfg.blocks {
        for opcode in &function.ops[block.range()] {
            for register in semantic_registers(opcode, opcode.metadata().semantics.writes) {
                definitions
                    .entry(register.register.0)
                    .or_default()
                    .insert(NodeId::Block(block.id));
            }
        }
    }

    let mut result = vec![BTreeSet::new(); cfg.blocks.len()];
    for (register, definition_nodes) in definitions {
        let mut worklist = VecDeque::from_iter(definition_nodes.iter().copied());
        let mut queued = definition_nodes;
        while let Some(node) = worklist.pop_front() {
            for frontier in dominance_frontiers.get(&node).into_iter().flatten() {
                let NodeId::Block(block_id) = frontier else {
                    continue;
                };
                if result[*block_id].insert(register) && queued.insert(*frontier) {
                    worklist.push_back(*frontier);
                }
            }
        }
    }
    result
}

fn dominance_frontiers(cfg: &ControlFlowGraph) -> BTreeMap<NodeId, BTreeSet<NodeId>> {
    let dominators = cfg.dominators();
    let mut result: BTreeMap<NodeId, BTreeSet<NodeId>> = BTreeMap::new();
    for block in &cfg.blocks {
        let node = NodeId::Block(block.id);
        if block.predecessors.len() < 2 {
            continue;
        }
        let stop = dominators.immediate.get(&node).copied().flatten();
        for &predecessor in &block.predecessors {
            let mut runner = Some(predecessor);
            let mut seen = BTreeSet::new();
            while runner != stop {
                let Some(current) = runner else {
                    break;
                };
                if !seen.insert(current) {
                    break;
                }
                result.entry(current).or_default().insert(node);
                runner = dominators.immediate.get(&current).copied().flatten();
            }
        }
    }
    result
}

struct Renamer<'builder, 'input> {
    builder: &'builder mut Builder<'input>,
    blocks: &'builder mut Vec<IrBlock>,
    stacks: HashMap<Reg, Vec<ValueId>>,
    dom_children: BTreeMap<NodeId, Vec<NodeId>>,
    visited: BTreeSet<usize>,
    effect_orders: Vec<Option<usize>>,
}

impl Renamer<'_, '_> {
    fn visit_children(&mut self, node: NodeId) {
        let children = self.dom_children.get(&node).cloned().unwrap_or_default();
        for child in children {
            if let NodeId::Block(block_id) = child {
                self.visit_block(block_id);
            }
        }
    }

    fn visit_block(&mut self, block_id: usize) {
        if !self.visited.insert(block_id) {
            return;
        }
        let mut pushed = Vec::new();
        let phi_values = self.blocks[block_id]
            .phis
            .iter()
            .map(|phi| (phi.register, phi.result))
            .collect::<Vec<_>>();
        for (register, value) in phi_values {
            self.stacks.entry(register).or_default().push(value);
            pushed.push(register);
        }

        let range = self.builder.cfg.blocks[block_id].range();
        for opcode_index in range {
            self.lower_operation(block_id, opcode_index, &mut pushed);
        }

        self.add_successor_phi_inputs(NodeId::Block(block_id), Some(block_id), Some(&mut pushed));
        self.visit_children(NodeId::Block(block_id));
        for register in pushed.into_iter().rev() {
            if let Some(stack) = self.stacks.get_mut(&register) {
                stack.pop();
            }
        }
    }

    fn lower_operation(&mut self, block_id: usize, opcode_index: usize, pushed: &mut Vec<Reg>) {
        let opcode = self.builder.function.ops[opcode_index].clone();
        let semantics = opcode.metadata().semantics;
        let reads = semantic_registers(&opcode, semantics.reads);
        let writes = semantic_registers(&opcode, semantics.writes);
        let mut inputs = Vec::new();
        for read in reads {
            let (value, recovery_definition) = self.current_value(
                read.register,
                NodeId::Block(block_id),
                opcode_index,
                &opcode,
                &read.role,
            );
            if recovery_definition {
                pushed.push(read.register);
            }
            let ty = self.builder.values[value.0].ty;
            inputs.push(IrUse {
                role: read.role,
                register: read.register,
                value,
                ty,
                provenance: IrProvenance::opcode(self.builder.function.findex.0, opcode_index),
            });
        }

        let mut results = Vec::new();
        for (result_index, write) in writes.into_iter().enumerate() {
            let provenance = IrProvenance::opcode(self.builder.function.findex.0, opcode_index);
            let value = self.builder.new_value(
                write.register,
                ValueDefinition::Operation {
                    block: block_id,
                    operation: OperationId(opcode_index),
                    result_index,
                },
                debug_name_at(self.builder.code, self.builder.function, opcode_index),
                provenance,
            );
            if self.builder.values[value.0].ty == IrType::Unknown {
                self.builder
                    .diagnose_unknown_type(opcode_index, &opcode, write.register);
            }
            self.stacks.entry(write.register).or_default().push(value);
            pushed.push(write.register);
            results.push(value);
        }

        if matches!(operation_kind(&opcode), IrOperationKind::Unsupported { .. }) {
            self.builder.diagnostics.push(Diagnostic::for_opcode(
                DiagnosticSeverity::Unsupported,
                self.builder.code,
                self.builder.function.findex.0,
                self.builder.function,
                opcode_index,
                &opcode,
                "opcode is represented by an explicit unsupported typed IR node",
            ));
        }
        self.blocks[block_id].operations.push(IrOperation {
            id: OperationId(opcode_index),
            opcode: opcode.clone(),
            kind: operation_kind(&opcode),
            inputs,
            results,
            effects: operation_effects(&opcode),
            semantic_side_effects: semantics.side_effects.to_vec(),
            exceptions: semantics.exceptions.to_vec(),
            control_flow: semantics.control_flow,
            effect_order: self.effect_orders[opcode_index],
            provenance: IrProvenance::opcode(self.builder.function.findex.0, opcode_index),
        });
    }

    fn current_value(
        &mut self,
        register: Reg,
        node: NodeId,
        opcode_index: usize,
        opcode: &Opcode,
        _role: &str,
    ) -> (ValueId, bool) {
        if let Some(value) = self
            .stacks
            .get(&register)
            .and_then(|stack| stack.last())
            .copied()
        {
            if self.builder.values[value.0].ty == IrType::Unknown {
                self.builder
                    .diagnose_unknown_type(opcode_index, opcode, register);
            }
            return (value, false);
        }

        let block = match node {
            NodeId::Block(block) => Some(block),
            NodeId::Entry | NodeId::Exit => None,
        };
        let provenance = block.map_or_else(
            || IrProvenance::boundary(self.builder.function.findex.0, 0),
            |block| {
                IrProvenance::boundary(
                    self.builder.function.findex.0,
                    self.builder.cfg.blocks[block].start,
                )
            },
        );
        let value = self.builder.new_value(
            register,
            ValueDefinition::Undefined { block },
            None,
            provenance,
        );
        self.stacks.entry(register).or_default().push(value);
        if self.builder.values[value.0].ty == IrType::Unknown {
            self.builder
                .diagnose_unknown_type(opcode_index, opcode, register);
        }
        (value, true)
    }

    fn add_successor_phi_inputs(
        &mut self,
        predecessor: NodeId,
        block_id: Option<usize>,
        mut pushed: Option<&mut Vec<Reg>>,
    ) {
        let successors: Vec<_> = self
            .builder
            .cfg
            .edges
            .iter()
            .filter(|edge| edge.from == predecessor)
            .map(|edge| edge.to)
            .collect();
        for successor in successors {
            let NodeId::Block(successor_id) = successor else {
                continue;
            };
            let phi_registers = self.blocks[successor_id]
                .phis
                .iter()
                .map(|phi| phi.register)
                .collect::<Vec<_>>();
            for (phi_index, register) in phi_registers.into_iter().enumerate() {
                let opcode_index = block_id
                    .map(|id| self.builder.cfg.blocks[id].end - 1)
                    .unwrap_or(0);
                let opcode = self
                    .builder
                    .function
                    .ops
                    .get(opcode_index)
                    .cloned()
                    .unwrap_or(Opcode::Nop);
                let (value, recovery_definition) =
                    self.current_value(register, predecessor, opcode_index, &opcode, "phi input");
                if recovery_definition {
                    if let Some(pushed) = pushed.as_mut() {
                        pushed.push(register);
                    }
                }
                let provenance = edge_provenance(
                    self.builder.function.findex.0,
                    &self.builder.cfg,
                    predecessor,
                );
                self.blocks[successor_id].phis[phi_index]
                    .inputs
                    .push(PhiInput {
                        predecessor,
                        value,
                        provenance,
                    });
            }
        }
    }
}

fn edge_provenance(
    function_index: usize,
    cfg: &ControlFlowGraph,
    predecessor: NodeId,
) -> IrProvenance {
    match predecessor {
        NodeId::Entry => IrProvenance::boundary(function_index, 0),
        NodeId::Block(block) => IrProvenance::opcode(function_index, cfg.blocks[block].end - 1),
        NodeId::Exit => IrProvenance::boundary(function_index, cfg.instruction_count),
    }
}

fn finalize_phi_provenance(function_index: usize, blocks: &mut [IrBlock], values: &mut [IrValue]) {
    for block in blocks {
        for phi in &mut block.phis {
            phi.inputs.sort_by_key(|input| input.predecessor);
            let provenance = if phi.inputs.is_empty() {
                phi.provenance.clone()
            } else {
                IrProvenance::union(
                    function_index,
                    phi.inputs.iter().map(|input| &input.provenance),
                )
            };
            phi.provenance = provenance.clone();
            values[phi.result.0].provenance = provenance;
        }
    }
}

fn rebuild_uses(blocks: &[IrBlock], values: &mut [IrValue]) {
    for value in values.iter_mut() {
        value.uses.clear();
    }
    for block in blocks {
        for (phi_index, phi) in block.phis.iter().enumerate() {
            for (input_index, input) in phi.inputs.iter().enumerate() {
                if let Some(value) = values.get_mut(input.value.0) {
                    value.uses.push(UseSite::Phi {
                        block: block.id,
                        phi_index,
                        input_index,
                    });
                }
            }
        }
        for operation in &block.operations {
            for (input_index, input) in operation.inputs.iter().enumerate() {
                if let Some(value) = values.get_mut(input.value.0) {
                    value.uses.push(UseSite::Operation {
                        block: block.id,
                        operation: operation.id,
                        input_index,
                    });
                }
            }
        }
    }
    for value in values {
        value.uses.sort();
    }
}

fn recover_phi_names(blocks: &[IrBlock], values: &mut [IrValue]) {
    let mut changed = true;
    while changed {
        changed = false;
        for block in blocks {
            for phi in &block.phis {
                if values[phi.result.0].debug_name.is_some() {
                    continue;
                }
                let mut names = phi
                    .inputs
                    .iter()
                    .filter_map(|input| values[input.value.0].debug_name.clone());
                let Some(first) = names.next() else {
                    continue;
                };
                if names.all(|name| name == first) {
                    values[phi.result.0].debug_name = Some(first);
                    changed = true;
                }
            }
        }
    }
}

fn recover_locals(function_index: usize, blocks: &[IrBlock], values: &[IrValue]) -> Vec<IrLocal> {
    values
        .iter()
        .filter(|value| !matches!(value.definition, ValueDefinition::Undefined { .. }))
        .enumerate()
        .map(|(index, value)| {
            let mut ranges = value.provenance.opcode_ranges.clone();
            for use_site in &value.uses {
                match *use_site {
                    UseSite::Operation {
                        block, operation, ..
                    } => {
                        if let Some(operation) = blocks[block]
                            .operations
                            .iter()
                            .find(|candidate| candidate.id == operation)
                        {
                            ranges.extend(operation.provenance.opcode_ranges.iter().copied());
                        }
                    }
                    UseSite::Phi {
                        block,
                        phi_index,
                        input_index,
                    } => {
                        if let Some(input) = blocks[block]
                            .phis
                            .get(phi_index)
                            .and_then(|phi| phi.inputs.get(input_index))
                        {
                            ranges.extend(input.provenance.opcode_ranges.iter().copied());
                        }
                    }
                }
            }
            let start = ranges.iter().map(|range| range.start).min().unwrap_or(0);
            let end = ranges.iter().map(|range| range.end).max().unwrap_or(start);
            IrLocal {
                id: LocalId(index),
                value: value.id,
                register: value.register,
                version: value.version,
                ty: value.ty,
                debug_name: value.debug_name.clone(),
                lifetime: IrProvenance::range(function_index, start, end, true),
            }
        })
        .collect()
}

fn blocks_provenance(
    function_index: usize,
    cfg: &ControlFlowGraph,
    nodes: impl IntoIterator<Item = NodeId>,
) -> (BTreeSet<usize>, IrProvenance) {
    let blocks = nodes
        .into_iter()
        .filter_map(|node| match node {
            NodeId::Block(block) => Some(block),
            NodeId::Entry | NodeId::Exit => None,
        })
        .collect::<BTreeSet<_>>();
    let ranges = blocks
        .iter()
        .map(|block| {
            let block = &cfg.blocks[*block];
            OpcodeRange::new(block.start, block.end)
        })
        .collect();
    (blocks, IrProvenance::new(function_index, ranges, true))
}

fn build_regions(function_index: usize, cfg: &ControlFlowGraph) -> Vec<IrRegion> {
    let mut regions = vec![IrRegion {
        kind: IrRegionKind::Function,
        blocks: cfg.blocks.iter().map(|block| block.id).collect(),
        provenance: IrProvenance::range(function_index, 0, cfg.instruction_count, true),
    }];
    regions.extend(cfg.blocks.iter().map(|block| IrRegion {
        kind: IrRegionKind::BasicBlock { block: block.id },
        blocks: BTreeSet::from([block.id]),
        provenance: IrProvenance::range(function_index, block.start, block.end, true),
    }));
    for natural_loop in cfg.natural_loops() {
        let (blocks, provenance) =
            blocks_provenance(function_index, cfg, natural_loop.nodes.iter().copied());
        regions.push(IrRegion {
            kind: IrRegionKind::NaturalLoop {
                header: natural_loop.header,
                latch: natural_loop.latch,
            },
            blocks,
            provenance,
        });
    }
    for exception in &cfg.exception_regions {
        let mut blocks = cfg
            .blocks
            .iter()
            .filter(|block| block.start < exception.end && exception.start < block.end)
            .map(|block| block.id)
            .collect::<BTreeSet<_>>();
        let mut ranges = vec![
            OpcodeRange::new(exception.trap_opcode, exception.trap_opcode + 1),
            OpcodeRange::new(exception.start, exception.end),
        ];
        if let Some(handler) = cfg
            .blocks
            .iter()
            .find(|block| block.start == exception.handler)
        {
            blocks.insert(handler.id);
            ranges.push(OpcodeRange::new(handler.start, handler.end));
        }
        ranges.extend(
            exception
                .catch_opcodes
                .iter()
                .chain(exception.end_trap_opcodes.iter())
                .map(|opcode| OpcodeRange::new(*opcode, *opcode + 1)),
        );
        regions.push(IrRegion {
            kind: IrRegionKind::Exception {
                handler_opcode: exception.handler,
            },
            blocks,
            provenance: IrProvenance::new(function_index, ranges, true),
        });
    }
    for irreducible in &cfg.irreducible_regions {
        let (blocks, provenance) =
            blocks_provenance(function_index, cfg, irreducible.nodes.iter().copied());
        regions.push(IrRegion {
            kind: IrRegionKind::Irreducible,
            blocks,
            provenance,
        });
    }
    regions
}

struct Verifier<'a> {
    ir: &'a TypedIr,
    errors: Vec<IrVerificationError>,
}

impl Verifier<'_> {
    fn verify(&mut self) {
        self.verify_provenance();
        self.verify_regions();
        self.verify_blocks_and_operations();
        self.verify_values_and_uses();
        self.verify_dominance();
        self.verify_effect_ordering();
    }

    fn error(&mut self, provenance: IrProvenance, message: impl Into<String>) {
        self.errors.push(IrVerificationError {
            provenance,
            message: message.into(),
        });
    }

    fn verify_provenance(&mut self) {
        let mut provenances = Vec::new();
        provenances.extend(self.blocks_provenances());
        provenances.extend(self.ir.values.iter().map(|value| value.provenance.clone()));
        provenances.extend(self.ir.locals.iter().map(|local| local.lifetime.clone()));
        provenances.extend(
            self.ir
                .regions
                .iter()
                .map(|region| region.provenance.clone()),
        );
        for provenance in provenances {
            if provenance.function_index != self.ir.function_index {
                self.error(
                    provenance.clone(),
                    "node provenance belongs to a different function",
                );
                continue;
            }
            let normalized = normalize_ranges(provenance.opcode_ranges.clone());
            if normalized != provenance.opcode_ranges
                || provenance
                    .opcode_ranges
                    .iter()
                    .any(|range| range.start > range.end || range.end > self.ir.opcode_count)
            {
                self.error(provenance.clone(), "invalid or non-canonical opcode ranges");
            }
        }
    }

    fn blocks_provenances(&self) -> Vec<IrProvenance> {
        let mut result = Vec::new();
        for block in &self.ir.blocks {
            result.push(block.provenance.clone());
            result.extend(block.phis.iter().map(|phi| phi.provenance.clone()));
            result.extend(
                block
                    .phis
                    .iter()
                    .flat_map(|phi| phi.inputs.iter().map(|input| input.provenance.clone())),
            );
            result.extend(
                block
                    .operations
                    .iter()
                    .map(|operation| operation.provenance.clone()),
            );
            result.extend(block.operations.iter().flat_map(|operation| {
                operation
                    .inputs
                    .iter()
                    .map(|input| input.provenance.clone())
            }));
        }
        result
    }

    fn verify_regions(&mut self) {
        for region in &self.ir.regions {
            if region
                .blocks
                .iter()
                .any(|block| *block >= self.ir.blocks.len())
            {
                self.error(region.provenance.clone(), "region owns a missing IR block");
            }
            if let IrRegionKind::BasicBlock { block } = &region.kind {
                let expected_blocks = BTreeSet::from([*block]);
                if region.blocks != expected_blocks {
                    self.error(
                        region.provenance.clone(),
                        "basic-block region has inconsistent block ownership",
                    );
                }
            }
        }
        let function_regions = self
            .ir
            .regions
            .iter()
            .filter(|region| region.kind == IrRegionKind::Function)
            .collect::<Vec<_>>();
        let all_blocks = self
            .ir
            .blocks
            .iter()
            .map(|block| block.id)
            .collect::<BTreeSet<_>>();
        if function_regions.len() != 1 || function_regions[0].blocks != all_blocks {
            self.error(
                IrProvenance::range(self.ir.function_index, 0, self.ir.opcode_count, true),
                "function region does not own every IR block exactly once",
            );
        }
    }

    fn verify_blocks_and_operations(&mut self) {
        if self.ir.blocks.len() != self.ir.cfg.blocks.len() {
            self.error(
                IrProvenance::range(self.ir.function_index, 0, self.ir.opcode_count, true),
                "IR block count does not match CFG",
            );
            return;
        }
        let mut operation_owners = vec![None; self.ir.opcode_count];
        for (expected_id, block) in self.ir.blocks.iter().enumerate() {
            let cfg_block = &self.ir.cfg.blocks[expected_id];
            if block.id != expected_id
                || block.predecessors != cfg_block.predecessors
                || block.successors != cfg_block.successors
            {
                self.error(
                    block.provenance.clone(),
                    "IR block ownership differs from CFG",
                );
            }
            let expected_range =
                IrProvenance::range(self.ir.function_index, cfg_block.start, cfg_block.end, true);
            if block.provenance != expected_range {
                self.error(
                    block.provenance.clone(),
                    "IR block does not preserve its exact CFG opcode range",
                );
            }
            let mut previous = None;
            for operation in &block.operations {
                let index = operation.id.0;
                if index < cfg_block.start || index >= cfg_block.end {
                    self.error(
                        operation.provenance.clone(),
                        "operation is outside its owning block",
                    );
                    continue;
                }
                if previous.map_or(false, |previous| index <= previous) {
                    self.error(
                        operation.provenance.clone(),
                        "operations are not in source/effect order",
                    );
                }
                previous = Some(index);
                if operation_owners[index].replace(block.id).is_some() {
                    self.error(
                        operation.provenance.clone(),
                        "opcode was lowered into more than one IR operation",
                    );
                }
                self.verify_operation(block, operation);
            }
        }
        if let Some(index) = operation_owners.iter().position(Option::is_none) {
            self.error(
                IrProvenance::opcode(self.ir.function_index, index),
                "opcode has no typed or unsupported IR operation",
            );
        }
    }

    fn verify_operation(&mut self, block: &IrBlock, operation: &IrOperation) {
        let expected_provenance = IrProvenance::opcode(self.ir.function_index, operation.id.0);
        if operation.provenance != expected_provenance {
            self.error(
                operation.provenance.clone(),
                "operation does not preserve its exact opcode range",
            );
        }
        let semantics = operation.opcode.metadata().semantics;
        if operation.semantic_side_effects != semantics.side_effects
            || operation.exceptions != semantics.exceptions
            || operation.control_flow != semantics.control_flow
            || operation.effects != operation_effects(&operation.opcode)
            || operation.kind != operation_kind(&operation.opcode)
        {
            self.error(
                operation.provenance.clone(),
                "operation semantic/effect metadata does not match its opcode",
            );
        }
        if operation.effects.is_empty() {
            self.error(
                operation.provenance.clone(),
                "operation has no explicit IR effect classification",
            );
        }

        let reads = semantic_registers(&operation.opcode, semantics.reads);
        let writes = semantic_registers(&operation.opcode, semantics.writes);
        if operation.inputs.len() != reads.len() || operation.results.len() != writes.len() {
            self.error(
                operation.provenance.clone(),
                "operation register arity does not match opcode semantics",
            );
        }
        for (input, expected) in operation.inputs.iter().zip(reads) {
            if input.role != expected.role || input.register != expected.register {
                self.error(
                    operation.provenance.clone(),
                    "operation input role/register does not match opcode semantics",
                );
            }
            if input.provenance != operation.provenance {
                self.error(
                    input.provenance.clone(),
                    "operation input does not preserve its exact opcode range",
                );
            }
            let Some(value) = self.ir.values.get(input.value.0) else {
                self.error(
                    operation.provenance.clone(),
                    format!("operation input references missing value {}", input.value.0),
                );
                continue;
            };
            if input.ty != value.ty
                || input.register != value.register
                || input.ty != self.register_type(input.register)
            {
                self.error(
                    operation.provenance.clone(),
                    "operation input has an inconsistent type or register",
                );
            }
        }
        for (result_index, (&result, expected)) in operation.results.iter().zip(writes).enumerate()
        {
            let Some(value) = self.ir.values.get(result.0) else {
                self.error(
                    operation.provenance.clone(),
                    format!("operation result references missing value {}", result.0),
                );
                continue;
            };
            if value.register != expected.register
                || value.ty != self.register_type(expected.register)
                || value.definition
                    != (ValueDefinition::Operation {
                        block: block.id,
                        operation: operation.id,
                        result_index,
                    })
            {
                self.error(
                    operation.provenance.clone(),
                    "operation result definition, register, or type is inconsistent",
                );
            }
        }
    }

    fn register_type(&self, register: Reg) -> IrType {
        self.ir
            .register_types
            .get(register.0 as usize)
            .copied()
            .unwrap_or(IrType::Unknown)
    }

    fn verify_values_and_uses(&mut self) {
        let mut expected_uses = vec![Vec::new(); self.ir.values.len()];
        let mut definition_owners = vec![0usize; self.ir.values.len()];
        for block in &self.ir.blocks {
            let expected_predecessors = &block.predecessors;
            for (phi_index, phi) in block.phis.iter().enumerate() {
                if phi.result.0 >= self.ir.values.len() {
                    self.error(
                        phi.provenance.clone(),
                        "phi references a missing result value",
                    );
                    continue;
                }
                let value = &self.ir.values[phi.result.0];
                definition_owners[phi.result.0] += 1;
                if value.register != phi.register
                    || value.ty != self.register_type(phi.register)
                    || value.definition
                        != (ValueDefinition::Phi {
                            block: block.id,
                            phi_index,
                        })
                {
                    self.error(
                        phi.provenance.clone(),
                        "phi result definition, register, or type is inconsistent",
                    );
                }
                let actual_predecessors = phi
                    .inputs
                    .iter()
                    .map(|input| input.predecessor)
                    .collect::<BTreeSet<_>>();
                if &actual_predecessors != expected_predecessors
                    || actual_predecessors.len() != phi.inputs.len()
                {
                    self.error(
                        phi.provenance.clone(),
                        "phi inputs do not correspond one-to-one with block predecessors",
                    );
                }
                for (input_index, input) in phi.inputs.iter().enumerate() {
                    let Some(input_value) = self.ir.values.get(input.value.0) else {
                        self.error(
                            input.provenance.clone(),
                            "phi input references a missing value",
                        );
                        continue;
                    };
                    if input_value.register != phi.register || input_value.ty != value.ty {
                        self.error(
                            input.provenance.clone(),
                            "phi input type or register differs from its result",
                        );
                    }
                    expected_uses[input.value.0].push(UseSite::Phi {
                        block: block.id,
                        phi_index,
                        input_index,
                    });
                }
            }
            for operation in &block.operations {
                for result in &operation.results {
                    if let Some(owner_count) = definition_owners.get_mut(result.0) {
                        *owner_count += 1;
                    }
                }
                for (input_index, input) in operation.inputs.iter().enumerate() {
                    if input.value.0 < expected_uses.len() {
                        expected_uses[input.value.0].push(UseSite::Operation {
                            block: block.id,
                            operation: operation.id,
                            input_index,
                        });
                    }
                }
            }
        }
        for uses in &mut expected_uses {
            uses.sort();
        }
        let mut versions = HashSet::new();
        for (index, value) in self.ir.values.iter().enumerate() {
            if value.id != ValueId(index) {
                self.error(
                    value.provenance.clone(),
                    "value id does not match value table index",
                );
            }
            if value.ty != self.register_type(value.register) {
                self.error(
                    value.provenance.clone(),
                    "value type differs from its bytecode register type",
                );
            }
            if value.uses != expected_uses[index] {
                self.error(
                    value.provenance.clone(),
                    "value use-definition links are not reciprocal",
                );
            }
            if !versions.insert((value.register, value.version)) {
                self.error(
                    value.provenance.clone(),
                    "SSA register version is not unique",
                );
            }
            match value.definition {
                ValueDefinition::Parameter { index: parameter } => {
                    if value.register != Reg(parameter as u32) || definition_owners[index] != 0 {
                        self.error(
                            value.provenance.clone(),
                            "parameter definition has inconsistent register or ownership",
                        );
                    }
                }
                ValueDefinition::Undefined { block } => {
                    if block.map_or(false, |block| block >= self.ir.blocks.len())
                        || definition_owners[index] != 0
                    {
                        self.error(
                            value.provenance.clone(),
                            "undefined recovery definition has inconsistent ownership",
                        );
                    }
                }
                ValueDefinition::Operation { .. } | ValueDefinition::Phi { .. } => {
                    if definition_owners[index] != 1 {
                        self.error(
                            value.provenance.clone(),
                            "SSA value does not have exactly one owning definition",
                        );
                    }
                }
            }
        }

        let expected_locals = self
            .ir
            .values
            .iter()
            .filter(|value| !matches!(value.definition, ValueDefinition::Undefined { .. }))
            .map(|value| value.id)
            .collect::<Vec<_>>();
        let actual_locals = self
            .ir
            .locals
            .iter()
            .map(|local| local.value)
            .collect::<Vec<_>>();
        if expected_locals != actual_locals {
            self.error(
                IrProvenance::range(self.ir.function_index, 0, self.ir.opcode_count, true),
                "recovered locals do not correspond one-to-one with SSA definitions",
            );
        }
        for (index, local) in self.ir.locals.iter().enumerate() {
            if local.id != LocalId(index) {
                self.error(
                    local.lifetime.clone(),
                    "local id does not match local table index",
                );
            }
            if let Some(value) = self.ir.values.get(local.value.0) {
                if local.register != value.register
                    || local.version != value.version
                    || local.ty != value.ty
                    || local.debug_name != value.debug_name
                {
                    self.error(
                        local.lifetime.clone(),
                        "recovered local metadata differs from its SSA value",
                    );
                }
            }
        }
    }

    fn verify_dominance(&mut self) {
        let dominators = self.ir.cfg.dominators();
        let reachable = self.ir.cfg.reachable();
        for block in &self.ir.blocks {
            if !reachable.contains(&NodeId::Block(block.id)) {
                continue;
            }
            for operation in &block.operations {
                for input in &operation.inputs {
                    if let Some(value) = self.ir.values.get(input.value.0) {
                        if !self.value_dominates_operation(
                            value,
                            block.id,
                            operation.id,
                            &dominators,
                        ) {
                            self.error(
                                operation.provenance.clone(),
                                format!(
                                    "value {} does not dominate operation {}",
                                    value.id.0, operation.id.0
                                ),
                            );
                        }
                    }
                }
            }
            for phi in &block.phis {
                for input in &phi.inputs {
                    if let Some(value) = self.ir.values.get(input.value.0) {
                        if !self.value_dominates_edge(value, input.predecessor, &dominators) {
                            self.error(
                                input.provenance.clone(),
                                format!(
                                    "value {} does not dominate phi predecessor {:?}",
                                    value.id.0, input.predecessor
                                ),
                            );
                        }
                    }
                }
            }
        }
    }

    fn value_dominates_operation(
        &self,
        value: &IrValue,
        use_block: usize,
        use_operation: OperationId,
        dominators: &crate::cfg::DominatorInfo,
    ) -> bool {
        match value.definition {
            ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None } => {
                dominators
                    .sets
                    .get(&NodeId::Block(use_block))
                    .map_or(false, |set| set.contains(&NodeId::Entry))
            }
            ValueDefinition::Phi { block, .. }
            | ValueDefinition::Undefined { block: Some(block) } => {
                node_dominates(block, use_block, dominators)
            }
            ValueDefinition::Operation {
                block, operation, ..
            } => {
                node_dominates(block, use_block, dominators)
                    && (block != use_block || operation.0 < use_operation.0)
            }
        }
    }

    fn value_dominates_edge(
        &self,
        value: &IrValue,
        predecessor: NodeId,
        dominators: &crate::cfg::DominatorInfo,
    ) -> bool {
        match predecessor {
            NodeId::Entry => matches!(
                value.definition,
                ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None }
            ),
            NodeId::Block(predecessor_block) => match value.definition {
                ValueDefinition::Parameter { .. } | ValueDefinition::Undefined { block: None } => {
                    dominators
                        .sets
                        .get(&predecessor)
                        .map_or(false, |set| set.contains(&NodeId::Entry))
                }
                ValueDefinition::Phi { block, .. }
                | ValueDefinition::Undefined { block: Some(block) } => {
                    node_dominates(block, predecessor_block, dominators)
                }
                ValueDefinition::Operation { block, .. } => {
                    node_dominates(block, predecessor_block, dominators)
                }
            },
            NodeId::Exit => false,
        }
    }

    fn verify_effect_ordering(&mut self) {
        let mut operations = self
            .ir
            .blocks
            .iter()
            .flat_map(|block| block.operations.iter())
            .collect::<Vec<_>>();
        operations.sort_by_key(|operation| operation.id);
        let mut next = 0;
        for operation in operations {
            let expected = if needs_effect_order(&operation.opcode) {
                let current = Some(next);
                next += 1;
                current
            } else {
                None
            };
            if operation.effect_order != expected {
                self.error(
                    operation.provenance.clone(),
                    "effect ordering token is missing or out of source order",
                );
            }
        }
    }
}

fn node_dominates(
    definition_block: usize,
    use_block: usize,
    dominators: &crate::cfg::DominatorInfo,
) -> bool {
    dominators
        .sets
        .get(&NodeId::Block(use_block))
        .map_or(false, |set| set.contains(&NodeId::Block(definition_block)))
}

impl fmt::Display for IrProvenance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function {}", self.function_index)?;
        for range in &self.opcode_ranges {
            write!(f, " {}..{}", range.start, range.end)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hlbc::types::{RefFun, RefString};
    use proptest::prelude::*;
    use std::panic::{catch_unwind, AssertUnwindSafe};

    fn function(ops: Vec<Opcode>, register_count: usize) -> Function {
        Function {
            t: RefType(0),
            findex: RefFun(7),
            regs: vec![RefType(3); register_count],
            ops,
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        }
    }

    fn minimal_code() -> Bytecode {
        let mut code = Bytecode::default();
        code.types = vec![
            Type::Fun(hlbc::types::TypeFun {
                args: vec![RefType(3)],
                ret: RefType(3),
            }),
            Type::UI8,
            Type::UI16,
            Type::I32,
        ];
        code.strings.push("test".into());
        code
    }

    #[test]
    fn diamond_inserts_typed_phi_and_keeps_def_use_links() {
        let code = minimal_code();
        let function = function(
            vec![
                Opcode::JFalse {
                    cond: Reg(0),
                    offset: 2,
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: Default::default(),
                },
                Opcode::JAlways { offset: 1 },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            2,
        );
        let built = TypedIr::build(&code, &function).unwrap();
        built.value.verify().unwrap();
        let join = built
            .value
            .blocks
            .iter()
            .find(|block| block.operations.iter().any(|op| op.id == OperationId(4)))
            .unwrap();
        let phi = join.phis.iter().find(|phi| phi.register == Reg(1)).unwrap();
        assert_eq!(phi.inputs.len(), 2);
        assert_eq!(
            built.value.values[phi.result.0].ty,
            IrType::HashLink(RefType(3))
        );
        assert!(built.value.values[phi.result.0]
            .uses
            .iter()
            .any(|use_site| matches!(
                use_site,
                UseSite::Operation {
                    operation: OperationId(4),
                    ..
                }
            )));
    }

    #[test]
    fn loop_header_phi_receives_entry_and_back_edge_values() {
        let code = minimal_code();
        let function = function(
            vec![
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Label,
                Opcode::JFalse {
                    cond: Reg(0),
                    offset: 2,
                },
                Opcode::Incr { dst: Reg(1) },
                Opcode::JAlways { offset: -4 },
                Opcode::Ret { ret: Reg(1) },
            ],
            2,
        );
        let built = TypedIr::build(&code, &function).unwrap();
        built.value.verify().unwrap();
        let header = built
            .value
            .blocks
            .iter()
            .find(|block| block.operations.iter().any(|op| op.id == OperationId(1)))
            .unwrap();
        let phi = header
            .phis
            .iter()
            .find(|phi| phi.register == Reg(1))
            .unwrap();
        assert_eq!(phi.inputs.len(), 2);
        assert!(built
            .value
            .regions
            .iter()
            .any(|region| matches!(region.kind, IrRegionKind::NaturalLoop { .. })));
    }

    #[test]
    fn recovery_definitions_do_not_leak_between_dominator_siblings() {
        let code = minimal_code();
        let function = function(
            vec![
                Opcode::JFalse {
                    cond: Reg(0),
                    offset: 2,
                },
                Opcode::NullCheck { reg: Reg(1) },
                Opcode::Ret { ret: Reg(0) },
                Opcode::NullCheck { reg: Reg(1) },
                Opcode::Ret { ret: Reg(0) },
            ],
            2,
        );
        let ir = TypedIr::build(&code, &function).unwrap().value;
        ir.verify().unwrap();
        let recovery_blocks = ir
            .values
            .iter()
            .filter_map(|value| match value.definition {
                ValueDefinition::Undefined { block: Some(block) } if value.register == Reg(1) => {
                    Some(block)
                }
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        assert_eq!(recovery_blocks.len(), 2);
    }

    #[test]
    fn malformed_register_use_gets_precise_diagnostic_and_recovery_definition() {
        let code = minimal_code();
        let function = function(vec![Opcode::Ret { ret: Reg(9) }], 2);
        let built = TypedIr::build(&code, &function).unwrap();
        assert!(built.diagnostics.iter().any(|diagnostic| {
            diagnostic.opcode_index == 0
                && diagnostic.message.contains("r9")
                && diagnostic
                    .message
                    .contains("no valid declared HashLink type")
        }));
        assert!(built
            .value
            .values
            .iter()
            .any(|value| matches!(value.definition, ValueDefinition::Undefined { .. })));
        built.value.verify().unwrap();
    }

    #[test]
    fn malformed_control_flow_returns_a_typed_build_error() {
        let code = minimal_code();
        let function = function(vec![Opcode::JAlways { offset: i32::MAX }], 1);
        let error = TypedIr::build(&code, &function).unwrap_err();
        assert!(error
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("outside 0..=1")));
    }

    #[test]
    fn verifier_rejects_corrupt_type_phi_and_effect_order() {
        let code = minimal_code();
        let function = function(
            vec![
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            2,
        );
        let mut built = TypedIr::build(&code, &function).unwrap().value;
        built.blocks[0].operations[0].effect_order = Some(99);
        built.blocks[0].operations[1].inputs[0].ty = IrType::Unknown;
        let errors = built.verify().unwrap_err();
        assert!(errors
            .errors
            .iter()
            .any(|error| error.message.contains("effect ordering")));
        assert!(errors
            .errors
            .iter()
            .any(|error| error.message.contains("inconsistent type")));
    }

    #[test]
    fn verifier_rejects_corrupt_phi_ownership_and_use_links() {
        let code = minimal_code();
        let function = function(
            vec![
                Opcode::JFalse {
                    cond: Reg(0),
                    offset: 2,
                },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::JAlways { offset: 1 },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            2,
        );
        let mut ir = TypedIr::build(&code, &function).unwrap().value;
        let join = ir
            .blocks
            .iter()
            .position(|block| block.operations.iter().any(|op| op.id == OperationId(4)))
            .unwrap();
        ir.blocks[join].phis[0].inputs.pop();
        ir.blocks[join].id = usize::MAX;
        ir.values[0].uses.clear();
        let errors = ir.verify().unwrap_err();
        assert!(errors
            .errors
            .iter()
            .any(|error| error.message.contains("phi inputs")));
        assert!(errors
            .errors
            .iter()
            .any(|error| error.message.contains("ownership")));
        assert!(errors
            .errors
            .iter()
            .any(|error| error.message.contains("use-definition")));
    }

    #[test]
    fn reused_register_debug_assignments_create_distinct_locals() {
        let mut code = minimal_code();
        code.strings.push("first".into());
        code.strings.push("second".into());
        let mut function = function(
            vec![
                Opcode::Int {
                    dst: Reg(1),
                    ptr: Default::default(),
                },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            2,
        );
        function.assigns = Some(vec![(RefString(1), 1), (RefString(2), 2)]);
        let ir = TypedIr::build(&code, &function).unwrap().value;
        let locals = ir
            .locals
            .iter()
            .filter(|local| local.register == Reg(1))
            .collect::<Vec<_>>();
        assert_eq!(locals.len(), 2);
        assert_ne!(locals[0].value, locals[1].value);
        assert_ne!(locals[0].version, locals[1].version);
        assert_eq!(locals[0].debug_name.as_deref(), Some("first"));
        assert_eq!(locals[1].debug_name.as_deref(), Some("second"));
        assert!(locals[0].lifetime.opcode_ranges[0].end <= 1);
        assert_eq!(locals[1].lifetime.opcode_ranges[0], OpcodeRange::new(1, 3));
    }

    #[test]
    fn every_opcode_has_an_effect_aware_ir_kind() {
        let code = minimal_code();
        for opcode in Opcode::all_defaults() {
            assert!(!operation_effects(&opcode).is_empty(), "{}", opcode.name());
            assert_eq!(
                matches!(operation_kind(&opcode), IrOperationKind::Unsupported { .. }),
                matches!(opcode, Opcode::Asm { .. }),
                "{}",
                opcode.name()
            );
            let function = function(vec![opcode.clone()], 4);
            let built = TypedIr::build(&code, &function).unwrap_or_else(|error| {
                panic!("{} failed IR construction: {error}", opcode.name())
            });
            assert_eq!(
                built.value.bytecode_compatibility_stream(),
                [opcode.clone()]
            );
            built
                .value
                .verify()
                .unwrap_or_else(|error| panic!("{} failed verification: {error}", opcode.name()));
            let operation = &built.value.blocks[0].operations[0];
            assert_eq!(
                matches!(operation.kind, IrOperationKind::Unsupported { .. }),
                matches!(opcode, Opcode::Asm { .. })
            );
        }
    }

    #[test]
    fn storage_domains_have_distinct_effects() {
        assert!(matches!(
            operation_effects(&Opcode::GetMem {
                dst: Reg(0),
                bytes: Reg(1),
                index: Reg(2),
            })[0],
            IrEffect::RawMemory {
                access: AccessMode::Read,
                ..
            }
        ));
        assert!(matches!(
            operation_effects(&Opcode::GetGlobal {
                dst: Reg(0),
                global: RefGlobal(1),
            })[0],
            IrEffect::Global { .. }
        ));
        assert!(matches!(
            operation_effects(&Opcode::Unref {
                dst: Reg(0),
                src: Reg(1),
            })[0],
            IrEffect::Reference { .. }
        ));
        assert!(matches!(
            operation_effects(&Opcode::Field {
                dst: Reg(0),
                obj: Reg(1),
                field: RefField(2),
            })[0],
            IrEffect::ObjectField { .. }
        ));
        assert!(matches!(
            operation_effects(&Opcode::GetArray {
                dst: Reg(0),
                array: Reg(1),
                index: Reg(2),
            })[0],
            IrEffect::ArrayElement { .. }
        ));
        let division = Opcode::SDiv {
            dst: Reg(0),
            a: Reg(1),
            b: Reg(2),
        };
        assert_eq!(
            division.metadata().semantics.exceptions,
            &[PossibleException::Arithmetic]
        );
        assert!(needs_effect_order(&division));
    }

    proptest! {
        #[test]
        fn malformed_random_ir_inputs_never_panic(
            operands in prop::collection::vec((any::<i16>(), any::<u8>()), 0..48)
        ) {
            let code = minimal_code();
            let ops = operands
                .into_iter()
                .enumerate()
                .map(|(index, (offset, register))| match index % 5 {
                    0 => Opcode::Mov {
                        dst: Reg(register as u32),
                        src: Reg(register.wrapping_add(1) as u32),
                    },
                    1 => Opcode::JFalse {
                        cond: Reg(register as u32),
                        offset: offset as i32,
                    },
                    2 => Opcode::Trap {
                        exc: Reg(register as u32),
                        offset: offset as i32,
                    },
                    3 => Opcode::NullCheck {
                        reg: Reg(register as u32),
                    },
                    _ => Opcode::Ret {
                        ret: Reg(register as u32),
                    },
                })
                .collect();
            let function = function(ops, 4);
            let result = catch_unwind(AssertUnwindSafe(|| TypedIr::build(&code, &function)));
            prop_assert!(result.is_ok());
            if let Ok(Ok(built)) = result {
                prop_assert!(built.value.verify().is_ok());
            }
        }
    }
}

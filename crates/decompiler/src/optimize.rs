//! Transactional, verified optimization overlays for typed SSA IR.
//!
//! The bytecode-exact [`crate::ir::TypedIr`] remains immutable because CFG structuring
//! depends on original opcode offsets. Optimization decisions are represented
//! explicitly as SSA aliases, inline candidates, and eliminated operations.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt;

use hlbc::opcodes::{ControlFlowBehavior, Opcode};
use serde::Serialize;

use crate::ir::{
    AccessMode, IrEffect, IrOperation, IrProvenance, OperationId, TypedIr, UseSite,
    ValueDefinition, ValueId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum OptimizationProfile {
    Fidelity,
    #[default]
    Balanced,
    Readability,
}

impl OptimizationProfile {
    pub const fn pipeline(self) -> &'static [OptimizationPassKind] {
        match self {
            Self::Fidelity => &[OptimizationPassKind::VerifyIr],
            Self::Balanced => &[
                OptimizationPassKind::VerifyIr,
                OptimizationPassKind::ConstantAndCopyPropagation,
                OptimizationPassKind::EffectAwareTemporaryInlining,
            ],
            Self::Readability => &[
                OptimizationPassKind::VerifyIr,
                OptimizationPassKind::ConstantAndCopyPropagation,
                OptimizationPassKind::EffectAwareTemporaryInlining,
                OptimizationPassKind::DeadStoreAndCodeElimination,
            ],
        }
    }
}

impl fmt::Display for OptimizationProfile {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        out.write_str(match self {
            Self::Fidelity => "fidelity",
            Self::Balanced => "balanced",
            Self::Readability => "readability",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum OptimizationPassKind {
    VerifyIr,
    ConstantAndCopyPropagation,
    EffectAwareTemporaryInlining,
    DeadStoreAndCodeElimination,
}

impl OptimizationPassKind {
    pub const fn name(self) -> &'static str {
        match self {
            Self::VerifyIr => "verify-ir",
            Self::ConstantAndCopyPropagation => "constant-copy-propagation",
            Self::EffectAwareTemporaryInlining => "effect-aware-temporary-inlining",
            Self::DeadStoreAndCodeElimination => "dead-store-code-elimination",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EliminationReason {
    PropagatedCopy,
    DeadCode,
    DeadStore,
}

#[derive(Debug, Clone, Serialize)]
pub struct OptimizedIr {
    pub ir: TypedIr,
    /// Canonical replacement for every SSA value. Identity means unchanged.
    pub aliases: Vec<ValueId>,
    /// Canonical SSA values defined by HashLink literal opcodes.
    pub constant_values: BTreeSet<ValueId>,
    /// Pure, single-use SSA values which may be inlined without reordering.
    pub inline_values: BTreeSet<ValueId>,
    /// Operations retained in the source IR but omitted by optimized consumers.
    pub eliminated_operations: BTreeMap<OperationId, EliminationReason>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OptimizationVerificationError {
    pub provenance: IrProvenance,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, Serialize)]
#[error("optimized IR verification failed with {} error(s)", .errors.len())]
pub struct OptimizationVerificationErrors {
    pub errors: Vec<OptimizationVerificationError>,
}

impl OptimizedIr {
    pub fn new(ir: TypedIr) -> Self {
        let aliases = (0..ir.values.len()).map(ValueId).collect();
        let constant_values = ir
            .blocks
            .iter()
            .flat_map(|block| block.operations.iter())
            .filter(|operation| is_constant_definition(operation))
            .flat_map(|operation| operation.results.iter().copied())
            .collect();
        Self {
            ir,
            aliases,
            constant_values,
            inline_values: BTreeSet::new(),
            eliminated_operations: BTreeMap::new(),
        }
    }

    pub fn resolve(&self, value: ValueId) -> Option<ValueId> {
        let mut current = value;
        let mut seen = HashSet::new();
        loop {
            if !seen.insert(current) {
                return None;
            }
            let next = *self.aliases.get(current.0)?;
            if next == current {
                return Some(current);
            }
            current = next;
        }
    }

    pub fn operation_is_active(&self, operation: OperationId) -> bool {
        !self.eliminated_operations.contains_key(&operation)
    }

    pub fn verify(&self) -> Result<(), OptimizationVerificationErrors> {
        let mut errors = Vec::new();
        if let Err(source_errors) = self.ir.verify() {
            errors.extend(source_errors.errors.into_iter().map(|error| {
                OptimizationVerificationError {
                    provenance: error.provenance,
                    message: format!("source IR: {}", error.message),
                }
            }));
        }
        let function_provenance =
            IrProvenance::range(self.ir.function_index, 0, self.ir.opcode_count, true);
        if self.aliases.len() != self.ir.values.len() {
            errors.push(OptimizationVerificationError {
                provenance: function_provenance.clone(),
                message: "SSA alias table length differs from the value table".to_owned(),
            });
        }

        for value in &self.ir.values {
            let Some(resolved) = self.resolve(value.id) else {
                errors.push(OptimizationVerificationError {
                    provenance: value.provenance.clone(),
                    message: format!("SSA alias for value {} is missing or cyclic", value.id.0),
                });
                continue;
            };
            let Some(target) = self.ir.values.get(resolved.0) else {
                errors.push(OptimizationVerificationError {
                    provenance: value.provenance.clone(),
                    message: format!("SSA alias for value {} is out of bounds", value.id.0),
                });
                continue;
            };
            if value.ty != target.ty {
                errors.push(OptimizationVerificationError {
                    provenance: value.provenance.clone(),
                    message: format!(
                        "SSA alias {} -> {} changes the HashLink value type",
                        value.id.0, resolved.0
                    ),
                });
            }
        }
        for &value in &self.constant_values {
            let valid = self
                .ir
                .values
                .get(value.0)
                .and_then(|value| match value.definition {
                    ValueDefinition::Operation { operation, .. } => self.ir.operation(operation),
                    _ => None,
                })
                .map_or(false, is_constant_definition);
            if !valid {
                errors.push(OptimizationVerificationError {
                    provenance: self
                        .ir
                        .values
                        .get(value.0)
                        .map(|value| value.provenance.clone())
                        .unwrap_or_else(|| function_provenance.clone()),
                    message: format!("constant value {} has no literal definition", value.0),
                });
            }
        }

        let dominators = self.ir.cfg.dominators();
        for block in &self.ir.blocks {
            for operation in &block.operations {
                if !self.operation_is_active(operation.id) {
                    continue;
                }
                for input in &operation.inputs {
                    let Some(resolved) = self.resolve(input.value) else {
                        continue;
                    };
                    if !self.ir.value_dominates_operation(
                        resolved,
                        block.id,
                        operation.id,
                        &dominators,
                    ) {
                        errors.push(OptimizationVerificationError {
                            provenance: operation.provenance.clone(),
                            message: format!(
                                "propagated value {} does not dominate operation {}",
                                resolved.0, operation.id.0
                            ),
                        });
                    }
                }
            }
            for phi in &block.phis {
                for input in &phi.inputs {
                    let Some(resolved) = self.resolve(input.value) else {
                        continue;
                    };
                    if !self
                        .ir
                        .value_dominates_edge(resolved, input.predecessor, &dominators)
                    {
                        errors.push(OptimizationVerificationError {
                            provenance: input.provenance.clone(),
                            message: format!(
                                "propagated value {} does not dominate its phi edge",
                                resolved.0
                            ),
                        });
                    }
                }
            }
        }

        for (&operation_id, &reason) in &self.eliminated_operations {
            let Some(operation) = self.ir.operation(operation_id) else {
                errors.push(OptimizationVerificationError {
                    provenance: function_provenance.clone(),
                    message: format!(
                        "elimination references missing operation {}",
                        operation_id.0
                    ),
                });
                continue;
            };
            let valid = match reason {
                EliminationReason::PropagatedCopy => {
                    matches!(operation.opcode, Opcode::Mov { .. })
                        && operation.results.iter().all(|result| {
                            self.resolve(*result)
                                .map_or(false, |resolved| resolved != *result)
                        })
                }
                EliminationReason::DeadCode => is_pure_removable(operation),
                EliminationReason::DeadStore => is_safe_store(operation),
            };
            if !valid {
                errors.push(OptimizationVerificationError {
                    provenance: operation.provenance.clone(),
                    message: format!(
                        "operation {} cannot be eliminated as {:?}",
                        operation_id.0, reason
                    ),
                });
            }
        }

        let uses = effective_uses(self);
        for &value in &self.inline_values {
            let Some(resolved) = self.resolve(value) else {
                continue;
            };
            let Some(ir_value) = self.ir.values.get(resolved.0) else {
                errors.push(OptimizationVerificationError {
                    provenance: function_provenance.clone(),
                    message: format!("inline candidate {} is out of bounds", value.0),
                });
                continue;
            };
            let valid_definition = match ir_value.definition {
                ValueDefinition::Operation { operation, .. } => {
                    self.ir.operation(operation).map_or(false, |operation| {
                        self.operation_is_active(operation.id)
                            && is_safe_inline_definition(operation)
                    })
                }
                _ => false,
            };
            if resolved != value || uses.get(&value).map_or(0, Vec::len) != 1 || !valid_definition {
                errors.push(OptimizationVerificationError {
                    provenance: ir_value.provenance.clone(),
                    message: format!(
                        "value {} is not a pure, active, single-use inline candidate",
                        value.0
                    ),
                });
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(OptimizationVerificationErrors { errors })
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct OptimizationPassDiagnostic {
    pub pass: OptimizationPassKind,
    pub provenance: Option<IrProvenance>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct OptimizationPassTrace {
    pub pass: OptimizationPassKind,
    pub applied: bool,
    pub before_valid: bool,
    pub after_valid: bool,
    pub changed_operations: Vec<OperationId>,
    pub provenance: Vec<IrProvenance>,
    pub diagnostics: Vec<OptimizationPassDiagnostic>,
    pub before_snapshot: String,
    pub after_snapshot: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct OptimizationTrace {
    pub function_index: usize,
    pub profile: OptimizationProfile,
    pub passes: Vec<OptimizationPassTrace>,
}

#[derive(Debug, Clone)]
pub struct OptimizationResult {
    pub value: OptimizedIr,
    pub diagnostics: Vec<OptimizationPassDiagnostic>,
    pub trace: Option<OptimizationTrace>,
}

struct TransactionOutcome {
    candidate: OptimizedIr,
    applied: bool,
    before_valid: bool,
    after_valid: bool,
    diagnostics: Vec<OptimizationPassDiagnostic>,
}

fn verified_transaction(
    current: &mut OptimizedIr,
    pass: OptimizationPassKind,
    transform: impl FnOnce(&mut OptimizedIr),
) -> TransactionOutcome {
    let before_valid = current.verify().is_ok();
    let mut candidate = current.clone();
    transform(&mut candidate);
    let verification = candidate.verify();
    let after_valid = verification.is_ok();
    let applied = before_valid && after_valid;
    let mut diagnostics = Vec::new();
    if applied {
        *current = candidate.clone();
    } else {
        let errors = verification
            .err()
            .map(|errors| errors.errors)
            .unwrap_or_default();
        if errors.is_empty() {
            diagnostics.push(OptimizationPassDiagnostic {
                pass,
                provenance: None,
                message: "optimization refused because its input IR was invalid".to_owned(),
            });
        } else {
            diagnostics.extend(errors.into_iter().map(|error| OptimizationPassDiagnostic {
                pass,
                provenance: Some(error.provenance),
                message: format!("optimization rolled back: {}", error.message),
            }));
        }
    }
    TransactionOutcome {
        candidate,
        applied,
        before_valid,
        after_valid,
        diagnostics,
    }
}

pub fn optimize(
    ir: &TypedIr,
    profile: OptimizationProfile,
    retain_trace: bool,
) -> OptimizationResult {
    let mut current = OptimizedIr::new(ir.clone());
    let mut diagnostics = Vec::new();
    let mut traces = Vec::new();

    for &pass in profile.pipeline() {
        let before = current.clone();
        let before_snapshot = if retain_trace {
            snapshot(&current)
        } else {
            String::new()
        };
        let transaction = verified_transaction(&mut current, pass, |candidate| {
            apply_pass(pass, candidate);
        });
        let before_operations = before
            .eliminated_operations
            .keys()
            .copied()
            .collect::<BTreeSet<_>>();
        let mut changed_operations = transaction
            .candidate
            .eliminated_operations
            .keys()
            .filter(|operation| !before_operations.contains(operation))
            .copied()
            .collect::<Vec<_>>();
        changed_operations.extend(changed_alias_operations(&before, &transaction.candidate));
        changed_operations.sort();
        changed_operations.dedup();
        diagnostics.extend(transaction.diagnostics.clone());

        if retain_trace {
            let provenance = changed_operations
                .iter()
                .filter_map(|operation| ir.operation(*operation))
                .map(|operation| operation.provenance.clone())
                .collect();
            traces.push(OptimizationPassTrace {
                pass,
                applied: transaction.applied,
                before_valid: transaction.before_valid,
                after_valid: transaction.after_valid,
                changed_operations,
                provenance,
                diagnostics: transaction.diagnostics,
                before_snapshot,
                after_snapshot: snapshot(&current),
            });
        }
    }

    OptimizationResult {
        value: current,
        diagnostics,
        trace: retain_trace.then_some(OptimizationTrace {
            function_index: ir.function_index,
            profile,
            passes: traces,
        }),
    }
}

fn apply_pass(pass: OptimizationPassKind, ir: &mut OptimizedIr) {
    match pass {
        OptimizationPassKind::VerifyIr => {}
        OptimizationPassKind::ConstantAndCopyPropagation => propagate_constants_and_copies(ir),
        OptimizationPassKind::EffectAwareTemporaryInlining => inline_safe_temporaries(ir),
        OptimizationPassKind::DeadStoreAndCodeElimination => eliminate_dead_stores_and_code(ir),
    }
}

fn propagate_constants_and_copies(ir: &mut OptimizedIr) {
    loop {
        let mut changed = false;
        for block in &ir.ir.blocks {
            for operation in &block.operations {
                let Opcode::Mov { .. } = operation.opcode else {
                    continue;
                };
                let (Some(input), Some(result)) =
                    (operation.inputs.first(), operation.results.first())
                else {
                    continue;
                };
                let Some(source) = ir.resolve(input.value) else {
                    continue;
                };
                if ir.ir.values[source.0].ty != ir.ir.values[result.0].ty {
                    continue;
                }
                if ir.aliases[result.0] != source {
                    ir.aliases[result.0] = source;
                    changed = true;
                }
                ir.eliminated_operations
                    .insert(operation.id, EliminationReason::PropagatedCopy);
            }
            for phi in &block.phis {
                let mut inputs = phi
                    .inputs
                    .iter()
                    .filter_map(|input| ir.resolve(input.value));
                let Some(first) = inputs.next() else {
                    continue;
                };
                if inputs.all(|input| input == first)
                    && ir.ir.values[first.0].ty == ir.ir.values[phi.result.0].ty
                    && ir.aliases[phi.result.0] != first
                {
                    ir.aliases[phi.result.0] = first;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
}

fn inline_safe_temporaries(ir: &mut OptimizedIr) {
    ir.inline_values.clear();
    let uses = effective_uses(ir);
    for value in &ir.ir.values {
        if ir.resolve(value.id) != Some(value.id) {
            continue;
        }
        let Some(sites) = uses.get(&value.id) else {
            continue;
        };
        if sites.len() != 1 {
            continue;
        }
        let ValueDefinition::Operation {
            block, operation, ..
        } = value.definition
        else {
            continue;
        };
        let Some(definition) = ir.ir.operation(operation) else {
            continue;
        };
        if !ir.operation_is_active(operation) || !is_safe_inline_definition(definition) {
            continue;
        }
        let UseSite::Operation {
            block: use_block,
            operation: use_operation,
            ..
        } = sites[0]
        else {
            continue;
        };
        if block != use_block || operation.0 >= use_operation.0 {
            continue;
        }
        let crosses_ordered_operation = ir.ir.blocks[block]
            .operations
            .iter()
            .filter(|candidate| operation.0 < candidate.id.0 && candidate.id.0 < use_operation.0)
            .any(|candidate| {
                ir.operation_is_active(candidate.id) && is_ordering_barrier(candidate)
            });
        if !crosses_ordered_operation {
            ir.inline_values.insert(value.id);
        }
    }
}

fn eliminate_dead_stores_and_code(ir: &mut OptimizedIr) {
    eliminate_dead_global_stores(ir);
    loop {
        let uses = effective_uses(ir);
        let mut eliminated = Vec::new();
        for block in &ir.ir.blocks {
            for operation in block.operations.iter().rev() {
                if !ir.operation_is_active(operation.id) || !is_pure_removable(operation) {
                    continue;
                }
                if operation.results.iter().all(|result| {
                    ir.resolve(*result).map_or(true, |resolved| {
                        uses.get(&resolved).map_or(true, Vec::is_empty)
                    })
                }) {
                    eliminated.push(operation.id);
                }
            }
        }
        if eliminated.is_empty() {
            break;
        }
        for operation in eliminated {
            ir.eliminated_operations
                .insert(operation, EliminationReason::DeadCode);
        }
    }
    ir.inline_values = ir
        .inline_values
        .iter()
        .copied()
        .filter(|value| {
            ir.ir.values.get(value.0).map_or(false, |value| {
                matches!(
                    value.definition,
                    ValueDefinition::Operation { operation, .. }
                        if ir.operation_is_active(operation)
                )
            })
        })
        .collect();
}

fn eliminate_dead_global_stores(ir: &mut OptimizedIr) {
    let mut eliminate = Vec::new();
    for block in &ir.ir.blocks {
        let mut overwritten = BTreeSet::new();
        for operation in block.operations.iter().rev() {
            if !ir.operation_is_active(operation.id) {
                continue;
            }
            if is_global_barrier(operation) {
                overwritten.clear();
                continue;
            }
            let global_effect = operation.effects.iter().find_map(|effect| match effect {
                IrEffect::Global { access, global } => Some((*access, global.0)),
                _ => None,
            });
            match global_effect {
                Some((AccessMode::Read, global)) => {
                    overwritten.remove(&global);
                }
                Some((AccessMode::Write, global)) if is_safe_store(operation) => {
                    if overwritten.contains(&global) {
                        eliminate.push(operation.id);
                    } else {
                        overwritten.insert(global);
                    }
                }
                Some((AccessMode::Write, _)) => overwritten.clear(),
                None => {}
            }
        }
    }
    for operation in eliminate {
        ir.eliminated_operations
            .insert(operation, EliminationReason::DeadStore);
    }
}

fn is_global_barrier(operation: &IrOperation) -> bool {
    !operation.exceptions.is_empty()
        || operation.control_flow != ControlFlowBehavior::Fallthrough
        || operation.effects.iter().any(|effect| {
            matches!(
                effect,
                IrEffect::Call
                    | IrEffect::Allocation
                    | IrEffect::InlineAssembly
                    | IrEffect::ReferenceAlias
                    | IrEffect::RawMemory { .. }
            )
        })
        || matches!(
            operation.kind,
            crate::ir::IrOperationKind::Unsupported { .. }
        )
}

fn is_ordering_barrier(operation: &IrOperation) -> bool {
    !operation.semantic_side_effects.is_empty()
        || !operation.exceptions.is_empty()
        || operation.effect_order.is_some()
        || operation
            .effects
            .iter()
            .any(|effect| *effect != IrEffect::PureValue)
}

fn is_pure_removable(operation: &IrOperation) -> bool {
    operation
        .effects
        .iter()
        .all(|effect| *effect == IrEffect::PureValue)
        && operation.semantic_side_effects.is_empty()
        && operation.exceptions.is_empty()
        && operation.control_flow == ControlFlowBehavior::Fallthrough
}

fn is_safe_inline_definition(operation: &IrOperation) -> bool {
    is_pure_removable(operation) && !operation.results.is_empty()
}

fn is_safe_store(operation: &IrOperation) -> bool {
    operation.effects.iter().any(|effect| {
        matches!(
            effect,
            IrEffect::Global {
                access: AccessMode::Write,
                ..
            }
        )
    }) && operation.exceptions.is_empty()
        && operation.control_flow == ControlFlowBehavior::Fallthrough
        && !operation.effects.iter().any(|effect| {
            matches!(
                effect,
                IrEffect::Call
                    | IrEffect::Allocation
                    | IrEffect::InlineAssembly
                    | IrEffect::ReferenceAlias
                    | IrEffect::RawMemory { .. }
            )
        })
}

fn effective_uses(ir: &OptimizedIr) -> HashMap<ValueId, Vec<UseSite>> {
    let mut uses: HashMap<ValueId, Vec<UseSite>> = HashMap::new();
    for block in &ir.ir.blocks {
        for operation in &block.operations {
            if !ir.operation_is_active(operation.id) {
                continue;
            }
            for (input_index, input) in operation.inputs.iter().enumerate() {
                if let Some(value) = ir.resolve(input.value) {
                    uses.entry(value).or_default().push(UseSite::Operation {
                        block: block.id,
                        operation: operation.id,
                        input_index,
                    });
                }
            }
        }
        for (phi_index, phi) in block.phis.iter().enumerate() {
            for (input_index, input) in phi.inputs.iter().enumerate() {
                if let Some(value) = ir.resolve(input.value) {
                    uses.entry(value).or_default().push(UseSite::Phi {
                        block: block.id,
                        phi_index,
                        input_index,
                    });
                }
            }
        }
    }
    for sites in uses.values_mut() {
        sites.sort();
    }
    uses
}

fn changed_alias_operations(before: &OptimizedIr, after: &OptimizedIr) -> Vec<OperationId> {
    before
        .aliases
        .iter()
        .zip(&after.aliases)
        .enumerate()
        .filter(|(_, (before, after))| before != after)
        .filter_map(|(index, _)| match after.ir.values[index].definition {
            ValueDefinition::Operation { operation, .. } => Some(operation),
            _ => None,
        })
        .collect()
}

fn snapshot(ir: &OptimizedIr) -> String {
    serde_json::to_string_pretty(ir)
        .unwrap_or_else(|error| format!("{{\"snapshot_error\":\"{error}\"}}"))
}

fn is_constant_definition(operation: &IrOperation) -> bool {
    matches!(
        operation.opcode,
        Opcode::Int { .. }
            | Opcode::Float { .. }
            | Opcode::Bool { .. }
            | Opcode::Bytes { .. }
            | Opcode::String { .. }
            | Opcode::Null { .. }
            | Opcode::Type { .. }
    )
}

#[cfg(test)]
mod tests {
    use hlbc::opcodes::Opcode;
    use hlbc::types::{RefInt, RefType, Reg};
    use hlbc::Bytecode;

    use super::*;
    use crate::ir::TypedIr;

    fn synthetic_ir(ops: Vec<Opcode>, regs: Vec<RefType>) -> TypedIr {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let mut function = code.function_by_name("main").unwrap().clone();
        function.ops = ops;
        function.regs = regs;
        function.debug_info = None;
        function.assigns = None;
        TypedIr::build(&code, &function).unwrap().value
    }

    #[test]
    fn constant_and_copy_propagation_is_verified_before_and_after() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Mov {
                    dst: Reg(2),
                    src: Reg(1),
                },
                Opcode::Ret { ret: Reg(2) },
            ],
            vec![RefType(3), RefType(3), RefType(3)],
        );
        let result = optimize(&ir, OptimizationProfile::Balanced, true);
        assert!(result.value.verify().is_ok());
        assert_eq!(result.value.resolve(ValueId(2)), Some(ValueId(0)));
        assert_eq!(
            result.value.eliminated_operations.get(&OperationId(1)),
            Some(&EliminationReason::PropagatedCopy)
        );
        let trace = result.trace.unwrap();
        assert!(trace
            .passes
            .iter()
            .all(|pass| pass.before_valid && pass.after_valid && pass.applied));
    }

    #[test]
    fn inlining_requires_one_use_and_no_effect_or_exception_crossing() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: RefInt(1),
                },
                Opcode::Add {
                    dst: Reg(2),
                    a: Reg(0),
                    b: Reg(1),
                },
                Opcode::Mov {
                    dst: Reg(3),
                    src: Reg(2),
                },
                Opcode::Ret { ret: Reg(3) },
            ],
            vec![RefType(3), RefType(3), RefType(3), RefType(3)],
        );
        let result = optimize(&ir, OptimizationProfile::Balanced, false);
        assert!(result.value.verify().is_ok());
        assert!(result.value.inline_values.contains(&ValueId(2)));
    }

    #[test]
    fn inlining_refuses_exception_crossing_and_multiple_evaluation() {
        let crossing = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: RefInt(1),
                },
                Opcode::Add {
                    dst: Reg(2),
                    a: Reg(0),
                    b: Reg(1),
                },
                Opcode::NullCheck { reg: Reg(0) },
                Opcode::Add {
                    dst: Reg(3),
                    a: Reg(2),
                    b: Reg(0),
                },
                Opcode::Ret { ret: Reg(3) },
            ],
            vec![RefType(3), RefType(3), RefType(3), RefType(3)],
        );
        let crossing = optimize(&crossing, OptimizationProfile::Balanced, false);
        assert!(!crossing.value.inline_values.contains(&ValueId(2)));

        let multiple = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: RefInt(1),
                },
                Opcode::Add {
                    dst: Reg(2),
                    a: Reg(0),
                    b: Reg(1),
                },
                Opcode::Add {
                    dst: Reg(3),
                    a: Reg(2),
                    b: Reg(0),
                },
                Opcode::Add {
                    dst: Reg(4),
                    a: Reg(2),
                    b: Reg(1),
                },
                Opcode::Ret { ret: Reg(4) },
            ],
            vec![RefType(3), RefType(3), RefType(3), RefType(3), RefType(3)],
        );
        let multiple = optimize(&multiple, OptimizationProfile::Balanced, false);
        assert!(!multiple.value.inline_values.contains(&ValueId(2)));
    }

    #[test]
    fn dead_code_and_overwritten_global_stores_use_effect_liveness() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::SetGlobal {
                    global: hlbc::types::RefGlobal(0),
                    src: Reg(0),
                },
                Opcode::SetGlobal {
                    global: hlbc::types::RefGlobal(0),
                    src: Reg(0),
                },
                Opcode::Int {
                    dst: Reg(1),
                    ptr: RefInt(1),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![RefType(3), RefType(3)],
        );
        let result = optimize(&ir, OptimizationProfile::Readability, false);
        assert!(result.value.verify().is_ok());
        assert_eq!(
            result.value.eliminated_operations.get(&OperationId(1)),
            Some(&EliminationReason::DeadStore)
        );
        assert_eq!(
            result.value.eliminated_operations.get(&OperationId(3)),
            Some(&EliminationReason::DeadCode)
        );
    }

    #[test]
    fn dead_store_elimination_stops_at_exception_barriers() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::SetGlobal {
                    global: hlbc::types::RefGlobal(0),
                    src: Reg(0),
                },
                Opcode::NullCheck { reg: Reg(0) },
                Opcode::SetGlobal {
                    global: hlbc::types::RefGlobal(0),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![RefType(3)],
        );
        let result = optimize(&ir, OptimizationProfile::Readability, false);
        assert!(result.value.verify().is_ok());
        assert_ne!(
            result.value.eliminated_operations.get(&OperationId(1)),
            Some(&EliminationReason::DeadStore)
        );
    }

    #[test]
    fn invalid_candidate_is_rejected_by_overlay_verifier() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![RefType(3)],
        );
        let mut optimized = OptimizedIr::new(ir);
        optimized.aliases[0] = ValueId(99);
        assert!(optimized.verify().is_err());
    }

    #[test]
    fn invalid_transformation_is_rolled_back_with_diagnostics() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![RefType(3)],
        );
        let mut optimized = OptimizedIr::new(ir);
        let before = snapshot(&optimized);
        let transaction = verified_transaction(
            &mut optimized,
            OptimizationPassKind::ConstantAndCopyPropagation,
            |candidate| candidate.aliases[0] = ValueId(99),
        );
        assert!(!transaction.applied);
        assert!(transaction.before_valid);
        assert!(!transaction.after_valid);
        assert_eq!(snapshot(&optimized), before);
        assert!(transaction
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("rolled back")));
    }

    #[test]
    fn profiles_have_documented_deterministic_pipelines() {
        assert_eq!(
            OptimizationProfile::Fidelity.pipeline(),
            &[OptimizationPassKind::VerifyIr]
        );
        assert_eq!(
            OptimizationProfile::Balanced.pipeline(),
            &[
                OptimizationPassKind::VerifyIr,
                OptimizationPassKind::ConstantAndCopyPropagation,
                OptimizationPassKind::EffectAwareTemporaryInlining,
            ]
        );
        assert_eq!(
            OptimizationProfile::Readability.pipeline().last(),
            Some(&OptimizationPassKind::DeadStoreAndCodeElimination)
        );
    }

    #[test]
    fn retained_trace_is_deterministic_and_contains_snapshots_and_provenance() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Mov {
                    dst: Reg(1),
                    src: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            vec![RefType(3), RefType(3)],
        );
        let first = optimize(&ir, OptimizationProfile::Readability, true)
            .trace
            .unwrap();
        let second = optimize(&ir, OptimizationProfile::Readability, true)
            .trace
            .unwrap();
        assert_eq!(
            serde_json::to_string(&first).unwrap(),
            serde_json::to_string(&second).unwrap()
        );
        assert!(first.passes.iter().all(|pass| {
            !pass.before_snapshot.is_empty()
                && !pass.after_snapshot.is_empty()
                && pass.before_valid
                && pass.after_valid
        }));
        assert!(first.passes.iter().any(|pass| !pass.provenance.is_empty()));
    }

    #[test]
    fn constant_classifier_covers_hashlink_literal_definitions() {
        let ir = synthetic_ir(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![RefType(3)],
        );
        assert!(is_constant_definition(
            ir.operation(OperationId(0)).unwrap()
        ));
    }
}

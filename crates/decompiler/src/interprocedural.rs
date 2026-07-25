//! Conservative interprocedural recovery of types, constants, closures, and
//! likely call targets.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};

use hlbc::opcodes::Opcode;
use hlbc::types::{Function, RefFun, RefType, Reg};
use hlbc::Bytecode;
use serde::Serialize;

use crate::cache::{AnalysisCache, CacheStats, Fingerprint, FunctionCacheKey};
use crate::diagnostics::{
    Approximation, Confidence, Provenance, RecoveredConstruct, RecoveryAnnotation,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AbstractConstant {
    Int(i32),
    FloatBits(u64),
    Bool(bool),
    String(String),
    Null,
    Type(RecoveredType),
}

/// Orderable public wrapper for a HashLink type-pool reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct RecoveredType(pub RefType);

impl PartialOrd for RecoveredType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RecoveredType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0 .0.cmp(&other.0 .0)
    }
}

impl Hash for RecoveredType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0 .0.hash(state);
    }
}

/// A finite set plus a completeness bit. `complete = false` means other values
/// may exist and is the conservative top state for this domain.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FactSet<T: Ord> {
    pub values: BTreeSet<T>,
    pub complete: bool,
}

impl<T: Ord> FactSet<T> {
    pub fn unknown() -> Self {
        Self {
            values: BTreeSet::new(),
            complete: false,
        }
    }

    pub fn exact(value: T) -> Self {
        Self {
            values: BTreeSet::from([value]),
            complete: true,
        }
    }

    fn empty_exact() -> Self {
        Self {
            values: BTreeSet::new(),
            complete: true,
        }
    }

    fn merge(&mut self, other: &Self, limit: usize)
    where
        T: Clone,
    {
        self.complete &= other.complete;
        self.values.extend(other.values.iter().cloned());
        if self.values.len() > limit {
            self.values = self.values.iter().take(limit).cloned().collect();
            self.complete = false;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AbstractValue {
    pub types: FactSet<RecoveredType>,
    pub constants: FactSet<AbstractConstant>,
    pub closures: FactSet<RefFun>,
}

impl AbstractValue {
    pub fn unknown() -> Self {
        Self {
            types: FactSet::unknown(),
            constants: FactSet::unknown(),
            closures: FactSet::unknown(),
        }
    }

    pub fn declared_type(ty: RefType) -> Self {
        Self {
            types: FactSet::exact(RecoveredType(ty)),
            constants: FactSet::unknown(),
            closures: FactSet::unknown(),
        }
    }

    fn literal(ty: RefType, constant: AbstractConstant) -> Self {
        Self {
            types: FactSet::exact(RecoveredType(ty)),
            constants: FactSet::exact(constant),
            closures: FactSet::empty_exact(),
        }
    }

    fn closure(ty: RefType, target: RefFun) -> Self {
        Self {
            types: FactSet::exact(RecoveredType(ty)),
            constants: FactSet::empty_exact(),
            closures: FactSet::exact(target),
        }
    }

    fn merge(&mut self, other: &Self, limit: usize) {
        self.types.merge(&other.types, limit);
        self.constants.merge(&other.constants, limit);
        self.closures.merge(&other.closures, limit);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallTargetSet {
    pub targets: BTreeSet<RefFun>,
    pub complete: bool,
    pub confidence: Confidence,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallSiteSummary {
    pub provenance: Provenance,
    pub targets: CallTargetSet,
    pub arguments: Vec<AbstractValue>,
    pub dynamic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InvalidationReason {
    DynamicCall,
    EscapedClosure,
    GlobalWrite,
    FactLimit,
    IterationLimit,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConservativeInvalidation {
    pub provenance: Provenance,
    pub reason: InvalidationReason,
    pub detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FunctionSummary {
    pub function_index: usize,
    pub parameters: Vec<AbstractValue>,
    pub return_value: AbstractValue,
    pub call_sites: Vec<CallSiteSummary>,
    pub direct_dependencies: BTreeSet<usize>,
    pub conservative_invalidations: Vec<ConservativeInvalidation>,
    pub bytecode_hash: Fingerprint,
    pub dependency_fingerprint: Fingerprint,
}

impl FunctionSummary {
    pub fn annotations(&self) -> Vec<RecoveryAnnotation> {
        let mut annotations = Vec::new();
        for call in &self.call_sites {
            let approximation = if call.dynamic {
                Some(Approximation::DynamicDispatch)
            } else if !call.targets.complete || call.targets.targets.len() != 1 {
                Some(Approximation::AmbiguousCallTarget)
            } else {
                None
            };
            annotations.push(match approximation {
                Some(reason) => RecoveryAnnotation::approximate(
                    RecoveredConstruct::CallTarget,
                    call.provenance,
                    call.targets.confidence,
                    reason,
                    "interprocedural",
                ),
                None => RecoveryAnnotation::exact(
                    RecoveredConstruct::CallTarget,
                    call.provenance,
                    "interprocedural",
                ),
            });
        }
        if !self.return_value.constants.values.is_empty() {
            annotations.push(fact_annotation(
                self.function_index,
                RecoveredConstruct::ConstantFact,
                self.return_value.constants.complete,
            ));
        }
        if !self.return_value.types.values.is_empty() {
            annotations.push(fact_annotation(
                self.function_index,
                RecoveredConstruct::TypeFact,
                self.return_value.types.complete,
            ));
        }
        if !self.return_value.closures.values.is_empty() {
            annotations.push(fact_annotation(
                self.function_index,
                RecoveredConstruct::ClosureFact,
                self.return_value.closures.complete,
            ));
        }
        annotations
    }
}

fn fact_annotation(
    function_index: usize,
    construct: RecoveredConstruct,
    complete: bool,
) -> RecoveryAnnotation {
    let provenance = Provenance::new(function_index, 0, usize::MAX);
    if complete {
        RecoveryAnnotation::exact(construct, provenance, "interprocedural")
    } else {
        RecoveryAnnotation::approximate(
            construct,
            provenance,
            Confidence::MEDIUM,
            Approximation::Other("fact set was conservatively widened".to_owned()),
            "interprocedural",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct AnalysisConfig {
    pub max_iterations: usize,
    pub max_fact_values: usize,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        Self {
            max_iterations: 64,
            max_fact_values: 32,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ProgramAnalysis {
    pub functions: BTreeMap<usize, FunctionSummary>,
    pub dependency_order: Vec<usize>,
    pub annotations: Vec<RecoveryAnnotation>,
    pub cache: CacheStats,
    pub converged: bool,
}

impl ProgramAnalysis {
    pub fn function(&self, function_index: usize) -> Option<&FunctionSummary> {
        self.functions.get(&function_index)
    }

    pub fn likely_call_targets(
        &self,
        function_index: usize,
        opcode_index: usize,
    ) -> Option<&CallTargetSet> {
        self.function(function_index)?
            .call_sites
            .iter()
            .find(|call| call.provenance.opcode_start == opcode_index)
            .map(|call| &call.targets)
    }
}

pub fn analyze_program(code: &Bytecode, config: AnalysisConfig) -> ProgramAnalysis {
    let mut cache = AnalysisCache::new();
    analyze_program_with_cache(code, config, &mut cache)
}

pub fn analyze_program_with_cache(
    code: &Bytecode,
    config: AnalysisConfig,
    cache: &mut AnalysisCache,
) -> ProgramAnalysis {
    let config_hash = Fingerprint::serializable(&config);
    let context_hash = bytecode_context_hash(code);
    let mut locals = BTreeMap::new();
    let mut bytecode_hashes = BTreeMap::new();
    for native in &code.natives {
        let native_bytes = serde_json::to_vec(native).unwrap_or_default();
        bytecode_hashes.insert(
            native.findex.0,
            Fingerprint::bytes([context_hash.0.to_le_bytes().as_slice(), &native_bytes]),
        );
    }
    for function in sorted_functions(code) {
        let hash = function_hash(function, context_hash);
        bytecode_hashes.insert(function.findex.0, hash);
        locals.insert(
            function.findex.0,
            analyze_local(code, function, config, hash),
        );
    }
    let dependency_order = stable_dependency_order(&locals);
    let dependency_hashes = dependency_fingerprints(&locals, &bytecode_hashes);

    let mut summaries = BTreeMap::new();
    let mut misses = false;
    let mut keys = BTreeMap::new();
    for (&function_index, local) in &locals {
        let key = FunctionCacheKey {
            function_index,
            bytecode_hash: local.summary.bytecode_hash,
            configuration: config_hash,
            dependencies: dependency_hashes
                .get(&function_index)
                .copied()
                .unwrap_or_default(),
        };
        keys.insert(function_index, key);
        match cache.get(key) {
            Some(summary) => {
                summaries.insert(function_index, summary);
            }
            None => {
                misses = true;
                summaries.insert(function_index, local.summary.clone());
            }
        }
    }

    let mut converged = true;
    if misses {
        summaries = locals
            .iter()
            .map(|(&index, local)| (index, local.summary.clone()))
            .collect();
        for summary in summaries.values_mut() {
            summary.dependency_fingerprint = dependency_hashes
                .get(&summary.function_index)
                .copied()
                .unwrap_or_default();
        }
        converged = solve_fixed_point(&locals, &mut summaries, config);
        if !converged {
            for summary in summaries.values_mut() {
                summary
                    .conservative_invalidations
                    .push(ConservativeInvalidation {
                        provenance: Provenance::new(summary.function_index, 0, 0),
                        reason: InvalidationReason::IterationLimit,
                        detail: format!(
                            "analysis did not converge in {} iterations",
                            config.max_iterations
                        ),
                    });
                summary.return_value.constants.complete = false;
                summary.return_value.closures.complete = false;
            }
        }
        for (&function_index, summary) in &summaries {
            if let Some(key) = keys.get(&function_index) {
                cache.insert(*key, summary.clone());
            }
        }
    }

    let mut annotations: Vec<_> = summaries
        .values()
        .flat_map(FunctionSummary::annotations)
        .collect();
    annotations.sort_by(|left, right| {
        (
            left.provenance.function_index,
            left.provenance.opcode_start,
            left.construct,
        )
            .cmp(&(
                right.provenance.function_index,
                right.provenance.opcode_start,
                right.construct,
            ))
    });
    ProgramAnalysis {
        functions: summaries,
        dependency_order,
        annotations,
        cache: cache.stats(),
        converged,
    }
}

#[derive(Clone)]
struct SymbolicValue {
    value: AbstractValue,
    return_targets: BTreeSet<usize>,
}

impl SymbolicValue {
    fn declared(ty: RefType) -> Self {
        Self {
            value: AbstractValue::declared_type(ty),
            return_targets: BTreeSet::new(),
        }
    }
}

#[derive(Clone)]
struct LocalSummary {
    summary: FunctionSummary,
    returned_targets: BTreeSet<usize>,
}

fn analyze_local(
    code: &Bytecode,
    function: &Function,
    config: AnalysisConfig,
    bytecode_hash: Fingerprint,
) -> LocalSummary {
    let mut registers: Vec<_> = function
        .regs
        .iter()
        .copied()
        .map(SymbolicValue::declared)
        .collect();
    let mut call_sites = Vec::new();
    let mut dependencies = BTreeSet::new();
    let mut invalidations = Vec::new();
    let mut returned = AbstractValue {
        types: FactSet::empty_exact(),
        constants: FactSet::empty_exact(),
        closures: FactSet::empty_exact(),
    };
    let mut returned_targets = BTreeSet::new();

    for (opcode_index, opcode) in function.ops.iter().enumerate() {
        let provenance = Provenance::opcode(function.findex.0, opcode_index);
        match opcode {
            Opcode::Mov { dst, src } => copy_register(&mut registers, *dst, *src),
            Opcode::Int { dst, ptr } => {
                let constant = code.ints.get(ptr.0).copied().map(AbstractConstant::Int);
                set_literal(&mut registers, function, *dst, constant);
            }
            Opcode::Float { dst, ptr } => {
                let constant = code
                    .floats
                    .get(ptr.0)
                    .copied()
                    .map(|value| AbstractConstant::FloatBits(value.to_bits()));
                set_literal(&mut registers, function, *dst, constant);
            }
            Opcode::Bool { dst, value } => set_register(
                &mut registers,
                *dst,
                SymbolicValue {
                    value: AbstractValue::literal(
                        function.regtype(*dst),
                        AbstractConstant::Bool(*value),
                    ),
                    return_targets: BTreeSet::new(),
                },
            ),
            Opcode::String { dst, ptr } => {
                let constant = code
                    .strings
                    .get(ptr.0)
                    .map(|value| AbstractConstant::String(value.to_string()));
                set_literal(&mut registers, function, *dst, constant);
            }
            Opcode::Null { dst } => {
                set_literal(&mut registers, function, *dst, Some(AbstractConstant::Null))
            }
            Opcode::Type { dst, ty } => set_literal(
                &mut registers,
                function,
                *dst,
                Some(AbstractConstant::Type(RecoveredType(*ty))),
            ),
            Opcode::StaticClosure { dst, fun } | Opcode::InstanceClosure { dst, fun, .. } => {
                set_register(
                    &mut registers,
                    *dst,
                    SymbolicValue {
                        value: AbstractValue::closure(function.regtype(*dst), *fun),
                        return_targets: BTreeSet::new(),
                    },
                )
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                let targets = function
                    .regs
                    .get(obj.0 as usize)
                    .and_then(|ty| ty.method(field.0, code))
                    .map(|method| BTreeSet::from([method.findex]))
                    .unwrap_or_default();
                let complete = !targets.is_empty();
                let value = AbstractValue {
                    types: FactSet::exact(RecoveredType(function.regtype(*dst))),
                    constants: FactSet::empty_exact(),
                    closures: FactSet {
                        values: targets,
                        complete,
                    },
                };
                set_register(
                    &mut registers,
                    *dst,
                    SymbolicValue {
                        value,
                        return_targets: BTreeSet::new(),
                    },
                );
            }
            Opcode::Call0 { dst, fun } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                &[],
            ),
            Opcode::Call1 { dst, fun, arg0, .. } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                &[*arg0],
            ),
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                &[*arg0, *arg1],
            ),
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                &[*arg0, *arg1, *arg2],
            ),
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                &[*arg0, *arg1, *arg2, *arg3],
            ),
            Opcode::CallN { dst, fun, args, .. } => direct_call(
                function,
                &mut registers,
                &mut call_sites,
                &mut dependencies,
                provenance,
                *dst,
                *fun,
                args,
            ),
            Opcode::CallClosure { dst, fun, args, .. } => {
                let value = register(&registers, *fun);
                let targets = value
                    .map(|value| value.value.closures.values.clone())
                    .unwrap_or_default();
                let complete = value.map_or(false, |value| value.value.closures.complete)
                    && !targets.is_empty();
                for target in &targets {
                    dependencies.insert(target.0);
                }
                let confidence = if complete && targets.len() == 1 {
                    Confidence::CERTAIN
                } else if !targets.is_empty() {
                    Confidence::MEDIUM
                } else {
                    Confidence::NONE
                };
                call_sites.push(CallSiteSummary {
                    provenance,
                    targets: CallTargetSet {
                        targets: targets.clone(),
                        complete,
                        confidence,
                    },
                    arguments: argument_values(&registers, args),
                    dynamic: !complete,
                });
                if !complete {
                    invalidations.push(ConservativeInvalidation {
                        provenance,
                        reason: InvalidationReason::DynamicCall,
                        detail: "closure target set is incomplete".to_owned(),
                    });
                }
                set_call_result(&mut registers, function, *dst, targets);
            }
            Opcode::CallMethod {
                dst, field, args, ..
            } => {
                let targets = args
                    .first()
                    .and_then(|receiver| function.regs.get(receiver.0 as usize))
                    .and_then(|ty| ty.method(field.0, code))
                    .map(|method| BTreeSet::from([method.findex]))
                    .unwrap_or_default();
                method_call(
                    function,
                    &mut registers,
                    &mut call_sites,
                    &mut dependencies,
                    &mut invalidations,
                    provenance,
                    *dst,
                    targets,
                    args,
                );
            }
            Opcode::CallThis {
                dst, field, args, ..
            } => {
                let targets = function
                    .regs
                    .first()
                    .and_then(|ty| ty.method(field.0, code))
                    .map(|method| BTreeSet::from([method.findex]))
                    .unwrap_or_default();
                method_call(
                    function,
                    &mut registers,
                    &mut call_sites,
                    &mut dependencies,
                    &mut invalidations,
                    provenance,
                    *dst,
                    targets,
                    args,
                );
            }
            Opcode::Ret { ret } => {
                if let Some(value) = register(&registers, *ret) {
                    returned.merge(&value.value, config.max_fact_values);
                    returned_targets.extend(&value.return_targets);
                } else {
                    returned = AbstractValue::unknown();
                }
            }
            Opcode::SetGlobal { src, .. } => {
                let escaped = register(&registers, *src)
                    .map_or(false, |value| !value.value.closures.values.is_empty());
                invalidations.push(ConservativeInvalidation {
                    provenance,
                    reason: if escaped {
                        InvalidationReason::EscapedClosure
                    } else {
                        InvalidationReason::GlobalWrite
                    },
                    detail: if escaped {
                        "closure escaped through global storage".to_owned()
                    } else {
                        "global state changed".to_owned()
                    },
                });
            }
            _ => {
                if let Some(dst) = opcode
                    .operands()
                    .into_iter()
                    .find(|operand| operand.name == "dst")
                    .and_then(|operand| operand.value.strip_prefix('r').map(str::to_owned))
                    .and_then(|value| value.parse::<u32>().ok())
                    .map(Reg)
                {
                    set_register(
                        &mut registers,
                        dst,
                        SymbolicValue::declared(function.regtype(dst)),
                    );
                }
            }
        }
    }

    call_sites.sort_by_key(|call| call.provenance.opcode_start);
    invalidations.sort_by_key(|invalidation| {
        (
            invalidation.provenance.opcode_start,
            invalidation.provenance.opcode_end,
        )
    });
    let parameters = function
        .ty(code)
        .args
        .iter()
        .copied()
        .map(AbstractValue::declared_type)
        .collect();
    LocalSummary {
        summary: FunctionSummary {
            function_index: function.findex.0,
            parameters,
            return_value: returned,
            call_sites,
            direct_dependencies: dependencies,
            conservative_invalidations: invalidations,
            bytecode_hash,
            dependency_fingerprint: Fingerprint::default(),
        },
        returned_targets,
    }
}

#[allow(clippy::too_many_arguments)]
fn direct_call(
    function: &Function,
    registers: &mut [SymbolicValue],
    call_sites: &mut Vec<CallSiteSummary>,
    dependencies: &mut BTreeSet<usize>,
    provenance: Provenance,
    dst: Reg,
    target: RefFun,
    args: &[Reg],
) {
    dependencies.insert(target.0);
    call_sites.push(CallSiteSummary {
        provenance,
        targets: CallTargetSet {
            targets: BTreeSet::from([target]),
            complete: true,
            confidence: Confidence::CERTAIN,
        },
        arguments: argument_values(registers, args),
        dynamic: false,
    });
    set_call_result(registers, function, dst, BTreeSet::from([target]));
}

#[allow(clippy::too_many_arguments)]
fn method_call(
    function: &Function,
    registers: &mut [SymbolicValue],
    call_sites: &mut Vec<CallSiteSummary>,
    dependencies: &mut BTreeSet<usize>,
    invalidations: &mut Vec<ConservativeInvalidation>,
    provenance: Provenance,
    dst: Reg,
    targets: BTreeSet<RefFun>,
    args: &[Reg],
) {
    let complete = !targets.is_empty();
    for target in &targets {
        dependencies.insert(target.0);
    }
    call_sites.push(CallSiteSummary {
        provenance,
        targets: CallTargetSet {
            targets: targets.clone(),
            complete,
            confidence: if complete {
                Confidence::HIGH
            } else {
                Confidence::NONE
            },
        },
        arguments: argument_values(registers, args),
        dynamic: !complete,
    });
    if !complete {
        invalidations.push(ConservativeInvalidation {
            provenance,
            reason: InvalidationReason::DynamicCall,
            detail: "method dispatch could not be resolved from the declared receiver type"
                .to_owned(),
        });
    }
    set_call_result(registers, function, dst, targets);
}

fn set_call_result(
    registers: &mut [SymbolicValue],
    function: &Function,
    dst: Reg,
    targets: BTreeSet<RefFun>,
) {
    set_register(
        registers,
        dst,
        SymbolicValue {
            value: AbstractValue::declared_type(function.regtype(dst)),
            return_targets: targets.iter().map(|target| target.0).collect(),
        },
    );
}

fn set_literal(
    registers: &mut [SymbolicValue],
    function: &Function,
    dst: Reg,
    constant: Option<AbstractConstant>,
) {
    let value = constant.map_or_else(
        || AbstractValue::declared_type(function.regtype(dst)),
        |constant| AbstractValue::literal(function.regtype(dst), constant),
    );
    set_register(
        registers,
        dst,
        SymbolicValue {
            value,
            return_targets: BTreeSet::new(),
        },
    );
}

fn copy_register(registers: &mut [SymbolicValue], dst: Reg, src: Reg) {
    if let Some(value) = register(registers, src).cloned() {
        set_register(registers, dst, value);
    }
}

fn register(registers: &[SymbolicValue], reg: Reg) -> Option<&SymbolicValue> {
    registers.get(reg.0 as usize)
}

fn set_register(registers: &mut [SymbolicValue], reg: Reg, value: SymbolicValue) {
    if let Some(slot) = registers.get_mut(reg.0 as usize) {
        *slot = value;
    }
}

fn argument_values(registers: &[SymbolicValue], args: &[Reg]) -> Vec<AbstractValue> {
    args.iter()
        .map(|argument| {
            register(registers, *argument)
                .map(|value| value.value.clone())
                .unwrap_or_else(AbstractValue::unknown)
        })
        .collect()
}

fn solve_fixed_point(
    locals: &BTreeMap<usize, LocalSummary>,
    summaries: &mut BTreeMap<usize, FunctionSummary>,
    config: AnalysisConfig,
) -> bool {
    for _ in 0..config.max_iterations {
        let previous = summaries.clone();
        for (&function_index, local) in locals {
            let mut return_value = local.summary.return_value.clone();
            for target in &local.returned_targets {
                if let Some(callee) = previous.get(target) {
                    return_value.merge(&callee.return_value, config.max_fact_values);
                } else {
                    return_value.constants.complete = false;
                    return_value.closures.complete = false;
                }
            }
            if let Some(summary) = summaries.get_mut(&function_index) {
                summary.return_value = return_value;
            }
        }
        for caller in previous.values() {
            for call in &caller.call_sites {
                for target in &call.targets.targets {
                    let Some(callee) = summaries.get_mut(&target.0) else {
                        continue;
                    };
                    for (parameter, argument) in callee.parameters.iter_mut().zip(&call.arguments) {
                        parameter.merge(argument, config.max_fact_values);
                    }
                }
            }
        }
        if *summaries == previous {
            return true;
        }
    }
    false
}

fn sorted_functions(code: &Bytecode) -> Vec<&Function> {
    let mut functions: Vec<_> = code.functions.iter().collect();
    functions.sort_by_key(|function| function.findex.0);
    functions
}

fn bytecode_context_hash(code: &Bytecode) -> Fingerprint {
    let version = [code.version];
    let ints = serde_json::to_vec(&code.ints).unwrap_or_default();
    let float_bits: Vec<_> = code.floats.iter().map(|value| value.to_bits()).collect();
    let floats = serde_json::to_vec(&float_bits).unwrap_or_default();
    let strings: Vec<_> = code.strings.iter().map(ToString::to_string).collect();
    let strings = serde_json::to_vec(&strings).unwrap_or_default();
    let types = serde_json::to_vec(&code.types).unwrap_or_default();
    Fingerprint::bytes([version.as_slice(), &ints, &floats, &strings, &types])
}

fn function_hash(function: &Function, context: Fingerprint) -> Fingerprint {
    let function = serde_json::to_vec(function).unwrap_or_default();
    Fingerprint::bytes([context.0.to_le_bytes().as_slice(), &function])
}

fn dependency_fingerprints(
    locals: &BTreeMap<usize, LocalSummary>,
    hashes: &BTreeMap<usize, Fingerprint>,
) -> BTreeMap<usize, Fingerprint> {
    locals
        .keys()
        .map(|&root| {
            let mut reachable = BTreeSet::new();
            let mut pending = vec![root];
            while let Some(function) = pending.pop() {
                let Some(local) = locals.get(&function) else {
                    continue;
                };
                for dependency in &local.summary.direct_dependencies {
                    if *dependency != root && reachable.insert(*dependency) {
                        pending.push(*dependency);
                    }
                }
            }
            let bytes: Vec<_> = reachable
                .into_iter()
                .flat_map(|dependency| {
                    let hash = hashes.get(&dependency).copied().unwrap_or_default();
                    [
                        (dependency as u64).to_le_bytes().to_vec(),
                        hash.0.to_le_bytes().to_vec(),
                    ]
                })
                .collect();
            (root, Fingerprint::bytes(bytes))
        })
        .collect()
}

fn stable_dependency_order(locals: &BTreeMap<usize, LocalSummary>) -> Vec<usize> {
    fn visit(
        function: usize,
        locals: &BTreeMap<usize, LocalSummary>,
        visiting: &mut BTreeSet<usize>,
        visited: &mut BTreeSet<usize>,
        order: &mut Vec<usize>,
    ) {
        if visited.contains(&function) || !visiting.insert(function) {
            return;
        }
        if let Some(local) = locals.get(&function) {
            for dependency in &local.summary.direct_dependencies {
                if locals.contains_key(dependency) {
                    visit(*dependency, locals, visiting, visited, order);
                }
            }
        }
        visiting.remove(&function);
        if visited.insert(function) {
            order.push(function);
        }
    }

    let mut visiting = BTreeSet::new();
    let mut visited = BTreeSet::new();
    let mut order = Vec::new();
    for &function in locals.keys() {
        visit(function, locals, &mut visiting, &mut visited, &mut order);
    }
    order
}

#[cfg(test)]
mod tests {
    use hlbc::opcodes::Opcode;
    use hlbc::types::{RefFun, Reg};
    use hlbc::Bytecode;

    use crate::cache::AnalysisCache;

    use super::{analyze_program_with_cache, AbstractConstant, AnalysisConfig};

    #[test]
    fn constants_and_closures_flow_across_direct_calls() {
        let mut code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let main_index = code.function_by_name("main").unwrap().findex.0;
        let target = code
            .functions
            .iter()
            .find(|function| function.findex.0 != main_index && function.regs.len() >= 2);
        let Some(target) = target.cloned() else {
            return;
        };
        let target_index = target.findex.0;
        let caller = code
            .functions
            .iter_mut()
            .find(|function| function.findex.0 == main_index)
            .unwrap();
        if caller.regs.len() < 2 {
            return;
        }
        caller.ops = vec![
            Opcode::Call0 {
                dst: Reg(0),
                fun: RefFun(target_index),
            },
            Opcode::Ret { ret: Reg(0) },
        ];
        let callee = code
            .functions
            .iter_mut()
            .find(|function| function.findex.0 == target_index)
            .unwrap();
        callee.ops = vec![
            Opcode::Bool {
                dst: Reg(0),
                value: true,
            },
            Opcode::Ret { ret: Reg(0) },
        ];

        let mut cache = AnalysisCache::new();
        let analysis = analyze_program_with_cache(&code, AnalysisConfig::default(), &mut cache);
        let returned = &analysis
            .function(main_index)
            .unwrap()
            .return_value
            .constants;
        assert!(returned.values.contains(&AbstractConstant::Bool(true)));
    }

    #[test]
    fn dependency_change_invalidates_callers_but_not_unrelated_functions() {
        let mut code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let caller_position = code
            .functions
            .iter()
            .position(|function| !function.regs.is_empty())
            .unwrap();
        let target_position = (0..code.functions.len())
            .find(|position| *position != caller_position)
            .unwrap();
        let caller_index = code.functions[caller_position].findex.0;
        let target_index = code.functions[target_position].findex.0;
        code.functions[caller_position].ops.insert(
            0,
            Opcode::Call0 {
                dst: Reg(0),
                fun: RefFun(target_index),
            },
        );
        let config = AnalysisConfig::default();
        let mut cache = AnalysisCache::new();
        let first = analyze_program_with_cache(&code, config, &mut cache);
        assert!(!first.functions.is_empty());
        cache.reset_stats();
        let second = analyze_program_with_cache(&code, config, &mut cache);
        assert_eq!(second.cache.misses, 0);
        assert_eq!(second.cache.hits, code.functions.len());

        code.functions[target_position].ops.push(Opcode::Nop);
        cache.reset_stats();
        let third = analyze_program_with_cache(&code, config, &mut cache);
        assert!(third.cache.invalidations >= 2);
        assert!(third.cache.hits <= code.functions.len().saturating_sub(2));
        assert!(third.function(target_index).is_some());
        assert!(third.function(caller_index).is_some());
    }

    #[test]
    fn configuration_change_invalidates_every_entry() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let mut cache = AnalysisCache::new();
        analyze_program_with_cache(&code, AnalysisConfig::default(), &mut cache);
        cache.reset_stats();
        analyze_program_with_cache(
            &code,
            AnalysisConfig {
                max_fact_values: 1,
                ..AnalysisConfig::default()
            },
            &mut cache,
        );
        assert_eq!(cache.stats().invalidations, code.functions.len());
    }
}

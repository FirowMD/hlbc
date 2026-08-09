//! Stable extension API.
//!
//! Extensions should import from [`crate::api::v1`] rather than relying on crate-internal
//! module layout. Additive fields may still be introduced, but existing v1
//! names and behavioral contracts are retained for the 0.9 release line.

use serde::Serialize;

use crate::diagnostics::DiagnosticSeverity;
use crate::ir::{IrProvenance, TypedIr};
use crate::optimize::OptimizedIr;

pub const API_VERSION: u32 = 1;

#[derive(Debug, Clone, Serialize)]
pub struct ExtensionDiagnostic {
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub provenance: Option<IrProvenance>,
}

impl ExtensionDiagnostic {
    pub fn new(
        code: impl Into<String>,
        severity: DiagnosticSeverity,
        message: impl Into<String>,
        provenance: Option<IrProvenance>,
    ) -> Self {
        Self {
            code: code.into(),
            severity,
            message: message.into(),
            provenance,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PassContext<'a> {
    pub source_ir: &'a TypedIr,
}

#[derive(Debug, Clone)]
pub struct ExtensionPassOutput {
    pub value: OptimizedIr,
    pub diagnostics: Vec<ExtensionDiagnostic>,
    pub changed_provenance: Vec<IrProvenance>,
}

impl ExtensionPassOutput {
    pub fn unchanged(value: OptimizedIr) -> Self {
        Self {
            value,
            diagnostics: Vec::new(),
            changed_provenance: Vec::new(),
        }
    }
}

pub trait ExtensionPass: Send + Sync {
    /// Stable, report-friendly pass name.
    fn name(&self) -> &'static str;

    /// Produce a candidate overlay. The runner verifies it before publication.
    fn run(
        &self,
        context: &PassContext<'_>,
        input: &OptimizedIr,
    ) -> Result<ExtensionPassOutput, ExtensionPassError>;
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, Serialize)]
pub enum ExtensionPassError {
    #[error("extension pass {pass} rejected invalid input: {message}")]
    InvalidInput { pass: String, message: String },
    #[error("extension pass {pass} produced invalid output: {message}")]
    InvalidOutput { pass: String, message: String },
    #[error("extension pass {pass} failed: {message}")]
    Failed { pass: String, message: String },
}

/// Run one extension pass transactionally.
///
/// Invalid input is rejected before the extension executes. Invalid candidate
/// output is discarded and returned as an error, leaving `input` untouched.
pub fn run_verified_pass(
    pass: &dyn ExtensionPass,
    input: &OptimizedIr,
) -> Result<ExtensionPassOutput, ExtensionPassError> {
    input
        .verify()
        .map_err(|error| ExtensionPassError::InvalidInput {
            pass: pass.name().to_owned(),
            message: error.to_string(),
        })?;
    let context = PassContext {
        source_ir: &input.ir,
    };
    let mut output = pass.run(&context, input)?;
    output
        .value
        .verify()
        .map_err(|error| ExtensionPassError::InvalidOutput {
            pass: pass.name().to_owned(),
            message: error.to_string(),
        })?;
    output.changed_provenance.sort_by(|left, right| {
        (left.function_index, &left.opcode_ranges, left.synthetic).cmp(&(
            right.function_index,
            &right.opcode_ranges,
            right.synthetic,
        ))
    });
    output.changed_provenance.dedup();
    output.diagnostics.sort_by(|left, right| {
        (
            left.provenance
                .as_ref()
                .map(|provenance| provenance.function_index),
            &left.code,
            &left.message,
        )
            .cmp(&(
                right
                    .provenance
                    .as_ref()
                    .map(|provenance| provenance.function_index),
                &right.code,
                &right.message,
            ))
    });
    Ok(output)
}

/// Versioned imports for extension crates.
pub mod v1 {
    pub use super::{
        run_verified_pass, ExtensionDiagnostic, ExtensionPass, ExtensionPassError,
        ExtensionPassOutput, PassContext, API_VERSION,
    };
    pub use crate::cache::{AnalysisCache, CacheStats, Fingerprint, FunctionCacheKey};
    pub use crate::cfg::{BasicBlock, CfgError, ControlFlowGraph, Edge, EdgeKind, NodeId};
    pub use crate::diagnostics::{
        Approximation, Confidence, DecompileError, DecompileMode, DecompileOptions, Decompiled,
        Diagnostic, DiagnosticSeverity, Provenance, RecoveredConstruct, RecoveryAnnotation,
        SourceRange,
    };
    pub use crate::divergence::{
        AstRegion, DivergenceKind, DivergenceRegion, OpcodeDivergence, OpcodeSnapshot,
    };
    pub use crate::interprocedural::{
        AbstractConstant, AbstractValue, AnalysisConfig, CallSiteSummary, CallTargetSet,
        ConservativeInvalidation, FactSet, FunctionSummary, InvalidationReason, ProgramAnalysis,
        RecoveredType,
    };
    pub use crate::ir::{
        IrBlock, IrEffect, IrLocal, IrOperation, IrOperationKind, IrPhi, IrProvenance, IrRegion,
        IrRegionKind, IrType, IrUse, IrValue, IrVerificationError, IrVerificationErrors, LocalId,
        OpcodeRange, OperationId, TypedIr, UseSite, ValueDefinition, ValueId,
    };
    pub use crate::optimize::{
        optimize, EliminationReason, OptimizationPassDiagnostic, OptimizationPassKind,
        OptimizationPassTrace, OptimizationProfile, OptimizationResult, OptimizationTrace,
        OptimizedIr,
    };
    pub use crate::patterns::{
        OpcodePredicate, PatternAtom, PatternConstraint, PatternMatch, PatternRegistry,
        PatternSpec, PatternValidation,
    };
    pub use crate::project::{
        decompile_project, decompile_project_with_cache, discover_project, DeclarationKind,
        GeneratedFile, ProjectError, ProjectGraph, ProjectOptions, ProjectOutput, ProjectUnit,
        PROJECT_SCHEMA_VERSION,
    };
    pub use crate::views::{
        decompile_synchronized_views, HaxeView, OpcodeViewLink, SourceSpan, SynchronizedViews,
        ViewKind, ViewNode,
    };
}

#[cfg(test)]
mod tests {
    use hlbc::Bytecode;

    use super::{
        run_verified_pass, ExtensionPass, ExtensionPassError, ExtensionPassOutput, PassContext,
    };
    use crate::ir::TypedIr;
    use crate::optimize::OptimizedIr;

    struct Noop;

    impl ExtensionPass for Noop {
        fn name(&self) -> &'static str {
            "test-noop"
        }

        fn run(
            &self,
            _context: &PassContext<'_>,
            input: &OptimizedIr,
        ) -> Result<ExtensionPassOutput, ExtensionPassError> {
            Ok(ExtensionPassOutput::unchanged(input.clone()))
        }
    }

    struct CorruptAliases;

    impl ExtensionPass for CorruptAliases {
        fn name(&self) -> &'static str {
            "test-corrupt-aliases"
        }

        fn run(
            &self,
            _context: &PassContext<'_>,
            input: &OptimizedIr,
        ) -> Result<ExtensionPassOutput, ExtensionPassError> {
            let mut candidate = input.clone();
            candidate.aliases.pop();
            Ok(ExtensionPassOutput::unchanged(candidate))
        }
    }

    #[test]
    fn v1_facade_and_verified_pass_are_usable_outside_internal_modules() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let function = code.function_by_name("main").unwrap();
        let ir = TypedIr::build(&code, function).unwrap().value;
        let input = OptimizedIr::new(ir);
        let output = run_verified_pass(&Noop, &input).unwrap();
        output.value.verify().unwrap();
        assert!(matches!(
            run_verified_pass(&CorruptAliases, &input),
            Err(ExtensionPassError::InvalidOutput { .. })
        ));
        input.verify().unwrap();

        let _: crate::api::v1::Confidence = crate::api::v1::Confidence::CERTAIN;
        assert_eq!(crate::api::v1::API_VERSION, 1);
    }
}

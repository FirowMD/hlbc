use std::fmt;

use hlbc::opcodes::{Opcode, OpcodeOperand};
use hlbc::types::Function;
use hlbc::Bytecode;
use serde::Serialize;

use crate::optimize::{OptimizationProfile, OptimizationTrace};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum DiagnosticSeverity {
    Information,
    Approximation,
    Unsupported,
    Fatal,
}

impl fmt::Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Information => "information",
            Self::Approximation => "approximation",
            Self::Unsupported => "unsupported",
            Self::Fatal => "fatal",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SourceRange {
    pub file_index: usize,
    pub start_line: usize,
    pub end_line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Provenance {
    pub function_index: usize,
    pub opcode_start: usize,
    pub opcode_end: usize,
}

impl Provenance {
    pub const fn new(function_index: usize, opcode_start: usize, opcode_end: usize) -> Self {
        Self {
            function_index,
            opcode_start,
            opcode_end,
        }
    }

    pub const fn opcode(function_index: usize, opcode_index: usize) -> Self {
        Self::new(function_index, opcode_index, opcode_index + 1)
    }

    pub const fn contains_opcode(self, function_index: usize, opcode_index: usize) -> bool {
        self.function_index == function_index
            && self.opcode_start <= opcode_index
            && opcode_index < self.opcode_end
    }

    pub const fn len(self) -> usize {
        self.opcode_end.saturating_sub(self.opcode_start)
    }

    pub const fn is_empty(self) -> bool {
        self.opcode_start >= self.opcode_end
    }
}

/// A deterministic confidence value in the inclusive range `0..=1000`.
///
/// Integer storage avoids architecture- and serialization-dependent floating
/// point ordering while [`Confidence::as_f64`] remains convenient for clients.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Confidence(u16);

impl Confidence {
    pub const NONE: Self = Self(0);
    pub const LOW: Self = Self(250);
    pub const MEDIUM: Self = Self(500);
    pub const HIGH: Self = Self(750);
    pub const CERTAIN: Self = Self(1000);

    pub const fn from_millis(value: u16) -> Self {
        Self(if value > 1000 { 1000 } else { value })
    }

    pub const fn millis(self) -> u16 {
        self.0
    }

    pub fn as_f64(self) -> f64 {
        f64::from(self.0) / 1000.0
    }
}

impl Default for Confidence {
    fn default() -> Self {
        Self::CERTAIN
    }
}

/// Why a recovered construct is known to be an approximation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Approximation {
    AmbiguousCallTarget,
    ConflictingConstants,
    ConflictingTypes,
    DynamicDispatch,
    EscapedClosure,
    IrreducibleControlFlow,
    MissingDebugInformation,
    PatternValidation,
    RecompilationMismatch,
    UnsupportedOpcode,
    Other(String),
}

/// Public categories used to annotate recovered AST, IR, and analysis facts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RecoveredConstruct {
    Expression,
    Statement,
    ControlFlow,
    TypeFact,
    ConstantFact,
    ClosureFact,
    CallTarget,
    CompilerPattern,
    GeneratedDeclaration,
}

/// Provenance-indexed uncertainty metadata attached to recovered constructs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RecoveryAnnotation {
    pub construct: RecoveredConstruct,
    pub provenance: Provenance,
    pub confidence: Confidence,
    pub approximations: Vec<Approximation>,
    pub producer: String,
}

impl RecoveryAnnotation {
    pub fn exact(
        construct: RecoveredConstruct,
        provenance: Provenance,
        producer: impl Into<String>,
    ) -> Self {
        Self {
            construct,
            provenance,
            confidence: Confidence::CERTAIN,
            approximations: Vec::new(),
            producer: producer.into(),
        }
    }

    pub fn approximate(
        construct: RecoveredConstruct,
        provenance: Provenance,
        confidence: Confidence,
        approximation: Approximation,
        producer: impl Into<String>,
    ) -> Self {
        Self {
            construct,
            provenance,
            confidence,
            approximations: vec![approximation],
            producer: producer.into(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub function_index: usize,
    pub opcode_index: usize,
    pub opcode_name: String,
    pub operands: Vec<OpcodeOperand>,
    pub source_range: Option<SourceRange>,
    pub message: String,
}

impl Diagnostic {
    pub fn for_opcode(
        severity: DiagnosticSeverity,
        code: &Bytecode,
        function_index: usize,
        function: &Function,
        opcode_index: usize,
        opcode: &Opcode,
        message: impl Into<String>,
    ) -> Self {
        let source_range = function.debug_info.as_ref().and_then(|info| {
            let &(file_index, start_line) = info.get(opcode_index)?;
            let end_line = info
                .get(opcode_index + 1)
                .filter(|(next_file, _)| *next_file == file_index)
                .map_or(start_line, |(_, line)| *line);
            if code
                .debug_files
                .as_ref()
                .is_some_and(|files| file_index >= files.len())
            {
                None
            } else {
                Some(SourceRange {
                    file_index,
                    start_line,
                    end_line,
                })
            }
        });
        Self {
            severity,
            function_index,
            opcode_index,
            opcode_name: opcode.name().to_owned(),
            operands: opcode.operands(),
            source_range,
            message: message.into(),
        }
    }

    pub fn fatal(function_index: usize, message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Fatal,
            function_index,
            opcode_index: 0,
            opcode_name: String::new(),
            operands: Vec::new(),
            source_range: None,
            message: message.into(),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: function {} opcode {} {}",
            self.severity, self.function_index, self.opcode_index, self.opcode_name
        )?;
        if !self.operands.is_empty() {
            f.write_str(" (")?;
            for (index, operand) in self.operands.iter().enumerate() {
                if index > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{}={}", operand.name, operand.value)?;
            }
            f.write_str(")")?;
        }
        if let Some(source) = self.source_range {
            write!(
                f,
                " source {}:{}-{}",
                source.file_index, source.start_line, source.end_line
            )?;
        }
        write!(f, ": {}", self.message)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize)]
pub enum DecompileMode {
    Strict,
    #[default]
    BestEffort,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct DecompileOptions {
    pub mode: DecompileMode,
    pub include_unreachable: bool,
    pub optimization_profile: OptimizationProfile,
    pub trace_optimizations: bool,
}

impl Default for DecompileOptions {
    fn default() -> Self {
        Self {
            mode: DecompileMode::BestEffort,
            include_unreachable: true,
            optimization_profile: OptimizationProfile::Balanced,
            trace_optimizations: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decompiled<T> {
    pub value: T,
    pub diagnostics: Vec<Diagnostic>,
    pub optimization_traces: Option<Vec<OptimizationTrace>>,
    pub recovery_annotations: Vec<RecoveryAnnotation>,
}

impl<T> Decompiled<T> {
    pub fn new(value: T, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            value,
            diagnostics,
            optimization_traces: None,
            recovery_annotations: Vec::new(),
        }
    }

    pub fn with_optimization_trace(mut self, trace: Option<OptimizationTrace>) -> Self {
        self.optimization_traces = trace.map(|trace| vec![trace]);
        self
    }

    pub fn with_optimization_traces(mut self, traces: Vec<OptimizationTrace>) -> Self {
        self.optimization_traces = (!traces.is_empty()).then_some(traces);
        self
    }

    pub fn with_recovery_annotations(mut self, mut annotations: Vec<RecoveryAnnotation>) -> Self {
        annotations.sort_by(|left, right| {
            (
                left.provenance.function_index,
                left.provenance.opcode_start,
                left.provenance.opcode_end,
                left.construct,
                &left.producer,
            )
                .cmp(&(
                    right.provenance.function_index,
                    right.provenance.opcode_start,
                    right.provenance.opcode_end,
                    right.construct,
                    &right.producer,
                ))
        });
        self.recovery_annotations = annotations;
        self
    }

    pub fn annotations_for_opcode(
        &self,
        function_index: usize,
        opcode_index: usize,
    ) -> impl Iterator<Item = &RecoveryAnnotation> {
        self.recovery_annotations.iter().filter(move |annotation| {
            annotation
                .provenance
                .contains_opcode(function_index, opcode_index)
        })
    }

    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Decompiled<U> {
        Decompiled {
            value: map(self.value),
            diagnostics: self.diagnostics,
            optimization_traces: self.optimization_traces,
            recovery_annotations: self.recovery_annotations,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("decompilation failed with {fatal_count} fatal/unsupported diagnostic(s)")]
pub struct DecompileError {
    pub diagnostics: Vec<Diagnostic>,
    fatal_count: usize,
}

impl DecompileError {
    pub fn new(diagnostics: Vec<Diagnostic>) -> Self {
        let fatal_count = diagnostics
            .iter()
            .filter(|diagnostic| {
                matches!(
                    diagnostic.severity,
                    DiagnosticSeverity::Unsupported | DiagnosticSeverity::Fatal
                )
            })
            .count();
        Self {
            diagnostics,
            fatal_count,
        }
    }
}

use std::fmt;

use hlbc::opcodes::{Opcode, OpcodeOperand};
use hlbc::types::Function;
use hlbc::Bytecode;
use serde::Serialize;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DecompileMode {
    Strict,
    #[default]
    BestEffort,
}

#[derive(Debug, Clone, Copy)]
pub struct DecompileOptions {
    pub mode: DecompileMode,
    pub include_unreachable: bool,
}

impl Default for DecompileOptions {
    fn default() -> Self {
        Self {
            mode: DecompileMode::BestEffort,
            include_unreachable: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decompiled<T> {
    pub value: T,
    pub diagnostics: Vec<Diagnostic>,
}

impl<T> Decompiled<T> {
    pub fn new(value: T, diagnostics: Vec<Diagnostic>) -> Self {
        Self { value, diagnostics }
    }

    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Decompiled<U> {
        Decompiled {
            value: map(self.value),
            diagnostics: self.diagnostics,
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

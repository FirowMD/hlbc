//! Deterministic parallel function decompilation.

use std::collections::BTreeSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hlbc::types::RefFun;
use hlbc::Bytecode;
use serde::Serialize;

use crate::ast::Statement;
use crate::decompile_code_with_options;
use crate::diagnostics::{DecompileOptions, Diagnostic, RecoveryAnnotation};
use crate::optimize::OptimizationTrace;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct ParallelOptions {
    pub workers: usize,
}

impl Default for ParallelOptions {
    fn default() -> Self {
        Self {
            workers: std::thread::available_parallelism()
                .map(usize::from)
                .unwrap_or(1),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArtifact {
    pub function_index: usize,
    pub statements: Vec<Statement>,
    pub diagnostics: Vec<Diagnostic>,
    pub optimization_traces: Vec<OptimizationTrace>,
    pub recovery_annotations: Vec<RecoveryAnnotation>,
    pub failed: bool,
}

#[derive(Debug, Clone)]
pub struct ParallelDecompilation {
    pub functions: Vec<FunctionArtifact>,
    pub diagnostics: Vec<Diagnostic>,
    pub recovery_annotations: Vec<RecoveryAnnotation>,
    pub workers_used: usize,
}

impl ParallelDecompilation {
    pub fn function(&self, function_index: usize) -> Option<&FunctionArtifact> {
        self.functions
            .binary_search_by_key(&function_index, |artifact| artifact.function_index)
            .ok()
            .and_then(|index| self.functions.get(index))
    }
}

/// Decompile a selected set of functions. Empty `function_indices` means all
/// bytecode functions. Results are always sorted by HashLink function index.
pub fn decompile_functions_parallel(
    code: &Bytecode,
    function_indices: &BTreeSet<usize>,
    options: DecompileOptions,
    parallel: ParallelOptions,
) -> ParallelDecompilation {
    let mut functions: Vec<_> = code
        .functions
        .iter()
        .filter(|function| {
            function_indices.is_empty() || function_indices.contains(&function.findex.0)
        })
        .collect();
    functions.sort_by_key(|function| function.findex.0);
    let worker_count = parallel.workers.max(1).min(functions.len().max(1));
    let next = AtomicUsize::new(0);
    let results = Mutex::new(Vec::with_capacity(functions.len()));

    std::thread::scope(|scope| {
        for _ in 0..worker_count {
            let functions = &functions;
            let next = &next;
            let results = &results;
            scope.spawn(move || loop {
                let position = next.fetch_add(1, Ordering::Relaxed);
                let Some(function) = functions.get(position) else {
                    break;
                };
                let artifact = match decompile_code_with_options(code, function, options) {
                    Ok(result) => FunctionArtifact {
                        function_index: function.findex.0,
                        statements: result.value,
                        diagnostics: result.diagnostics,
                        optimization_traces: result.optimization_traces.unwrap_or_default(),
                        recovery_annotations: result.recovery_annotations,
                        failed: false,
                    },
                    Err(error) => FunctionArtifact {
                        function_index: function.findex.0,
                        statements: Vec::new(),
                        diagnostics: error.diagnostics,
                        optimization_traces: Vec::new(),
                        recovery_annotations: Vec::new(),
                        failed: true,
                    },
                };
                results
                    .lock()
                    .expect("parallel decompilation result mutex poisoned")
                    .push(artifact);
            });
        }
    });

    let mut functions = results
        .into_inner()
        .expect("parallel decompilation result mutex poisoned");
    functions.sort_by_key(|artifact| artifact.function_index);
    let mut diagnostics: Vec<_> = functions
        .iter()
        .flat_map(|artifact| artifact.diagnostics.iter().cloned())
        .collect();
    diagnostics.sort_by(|left, right| {
        (
            left.function_index,
            left.opcode_index,
            left.severity as u8,
            &left.message,
        )
            .cmp(&(
                right.function_index,
                right.opcode_index,
                right.severity as u8,
                &right.message,
            ))
    });
    let mut recovery_annotations: Vec<_> = functions
        .iter()
        .flat_map(|artifact| artifact.recovery_annotations.iter().cloned())
        .collect();
    recovery_annotations.sort_by(|left, right| {
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
    ParallelDecompilation {
        functions,
        diagnostics,
        recovery_annotations,
        workers_used: worker_count,
    }
}

pub fn decompile_all_functions_parallel(
    code: &Bytecode,
    options: DecompileOptions,
    parallel: ParallelOptions,
) -> ParallelDecompilation {
    decompile_functions_parallel(code, &BTreeSet::new(), options, parallel)
}

pub fn selected_function_refs(indices: &BTreeSet<usize>) -> Vec<RefFun> {
    indices.iter().copied().map(RefFun).collect()
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use hlbc::Bytecode;

    use super::{decompile_functions_parallel, ParallelOptions};
    use crate::diagnostics::DecompileOptions;
    use crate::fmt::FormatOptions;

    #[test]
    fn scheduling_does_not_change_function_output_or_diagnostics() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let run = |workers| {
            let result = decompile_functions_parallel(
                &code,
                &BTreeSet::new(),
                DecompileOptions::default(),
                ParallelOptions { workers },
            );
            let sources: Vec<_> = result
                .functions
                .iter()
                .map(|artifact| {
                    let function = code
                        .functions
                        .iter()
                        .find(|function| function.findex.0 == artifact.function_index)
                        .unwrap();
                    artifact
                        .statements
                        .iter()
                        .map(|statement| {
                            statement
                                .display(&FormatOptions::new(4), &code, function)
                                .to_string()
                        })
                        .collect::<Vec<_>>()
                })
                .collect();
            let diagnostics: Vec<_> = result.diagnostics.iter().map(ToString::to_string).collect();
            (sources, diagnostics)
        };
        assert_eq!(run(1), run(8));
    }
}

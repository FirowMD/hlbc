//! Confidence and approximation metadata for recovered constructs.

use std::collections::BTreeMap;

use hlbc::types::Function;

use crate::diagnostics::{
    Approximation, Confidence, Diagnostic, DiagnosticSeverity, Provenance, RecoveredConstruct,
    RecoveryAnnotation,
};
use crate::{opcode_coverage, LoweringCoverage};

/// Build deterministic baseline annotations for every lowered opcode.
///
/// More specialized analyses and pattern passes may add annotations covering
/// wider ranges. Consumers can query all annotations for an opcode and choose
/// the most specific range or the lowest confidence according to their UI.
pub fn annotate_function(
    function_index: usize,
    function: &Function,
    diagnostics: &[Diagnostic],
) -> Vec<RecoveryAnnotation> {
    let mut diagnostics_by_opcode: BTreeMap<usize, Vec<&Diagnostic>> = BTreeMap::new();
    let function_is_fatal = diagnostics.iter().any(|diagnostic| {
        diagnostic.function_index == function_index
            && diagnostic.severity == DiagnosticSeverity::Fatal
    });
    for diagnostic in diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.function_index == function_index)
    {
        diagnostics_by_opcode
            .entry(diagnostic.opcode_index)
            .or_default()
            .push(diagnostic);
    }

    function
        .ops
        .iter()
        .enumerate()
        .map(|(opcode_index, opcode)| {
            let coverage = opcode_coverage(opcode);
            let construct = match coverage.lowering {
                LoweringCoverage::Expression => RecoveredConstruct::Expression,
                LoweringCoverage::Statement | LoweringCoverage::SemanticOnly => {
                    RecoveredConstruct::Statement
                }
                LoweringCoverage::Structural => RecoveredConstruct::ControlFlow,
                LoweringCoverage::UnsupportedFallback => RecoveredConstruct::Statement,
            };
            let mut confidence = Confidence::CERTAIN;
            let mut approximations = Vec::new();
            if function_is_fatal {
                confidence = Confidence::NONE;
                approximations.push(Approximation::UnsupportedOpcode);
            }
            for diagnostic in diagnostics_by_opcode
                .get(&opcode_index)
                .into_iter()
                .flatten()
            {
                match diagnostic.severity {
                    DiagnosticSeverity::Information => {}
                    DiagnosticSeverity::Approximation => {
                        confidence = confidence.min(Confidence::MEDIUM);
                        approximations.push(Approximation::Other(diagnostic.message.clone()));
                    }
                    DiagnosticSeverity::Unsupported | DiagnosticSeverity::Fatal => {
                        confidence = Confidence::NONE;
                        approximations.push(Approximation::UnsupportedOpcode);
                    }
                }
            }
            approximations.sort();
            approximations.dedup();
            RecoveryAnnotation {
                construct,
                provenance: Provenance::opcode(function_index, opcode_index),
                confidence,
                approximations,
                producer: "opcode-lowering".to_owned(),
            }
        })
        .collect()
}

/// Return the most conservative confidence among annotations covering a range.
pub fn range_confidence(annotations: &[RecoveryAnnotation], provenance: Provenance) -> Confidence {
    annotations
        .iter()
        .filter(|annotation| {
            annotation.provenance.function_index == provenance.function_index
                && annotation.provenance.opcode_start < provenance.opcode_end
                && provenance.opcode_start < annotation.provenance.opcode_end
        })
        .map(|annotation| annotation.confidence)
        .min()
        .unwrap_or(Confidence::CERTAIN)
}

#[cfg(test)]
mod tests {
    use super::range_confidence;
    use crate::diagnostics::{
        Approximation, Confidence, Provenance, RecoveredConstruct, RecoveryAnnotation,
    };

    #[test]
    fn range_confidence_is_conservative_and_provenance_scoped() {
        let annotations = vec![
            RecoveryAnnotation::exact(
                RecoveredConstruct::Expression,
                Provenance::opcode(7, 1),
                "test",
            ),
            RecoveryAnnotation::approximate(
                RecoveredConstruct::CallTarget,
                Provenance::opcode(7, 2),
                Confidence::LOW,
                Approximation::DynamicDispatch,
                "test",
            ),
            RecoveryAnnotation::approximate(
                RecoveredConstruct::CallTarget,
                Provenance::opcode(8, 2),
                Confidence::NONE,
                Approximation::DynamicDispatch,
                "test",
            ),
        ];
        assert_eq!(
            range_confidence(&annotations, Provenance::new(7, 1, 3)),
            Confidence::LOW
        );
        assert_eq!(
            range_confidence(&annotations, Provenance::opcode(7, 8)),
            Confidence::CERTAIN
        );
    }
}

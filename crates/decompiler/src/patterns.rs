//! Declarative matching for compiler lowering patterns.
//!
//! The framework intentionally matches the immutable opcode stream. A pattern
//! may inform later AST recovery, but it cannot mutate bytecode while matching.

use std::collections::{BTreeMap, BTreeSet};

use hlbc::opcodes::Opcode;
use hlbc::types::Function;
use serde::Serialize;

use crate::diagnostics::{
    Approximation, Confidence, Provenance, RecoveredConstruct, RecoveryAnnotation,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum OpcodePredicate {
    Named(String),
    AnyOf(BTreeSet<String>),
    Any,
}

impl OpcodePredicate {
    pub fn named(name: impl Into<String>) -> Self {
        Self::Named(name.into())
    }

    fn matches(&self, opcode: &Opcode) -> bool {
        match self {
            Self::Named(name) => opcode.name() == name,
            Self::AnyOf(names) => names.contains(opcode.name()),
            Self::Any => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PatternAtom {
    pub predicate: OpcodePredicate,
    pub capture: Option<String>,
}

impl PatternAtom {
    pub fn named(name: impl Into<String>) -> Self {
        Self {
            predicate: OpcodePredicate::named(name),
            capture: None,
        }
    }

    pub fn captured(name: impl Into<String>, capture: impl Into<String>) -> Self {
        Self {
            predicate: OpcodePredicate::named(name),
            capture: Some(capture.into()),
        }
    }
}

/// Declarative post-match validation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PatternConstraint {
    /// Operand string values must be equal. This is useful for register flow.
    OperandEqual {
        left_atom: usize,
        left_operand: String,
        right_atom: usize,
        right_operand: String,
    },
    /// An atom must expose the named operand.
    HasOperand { atom: usize, operand: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PatternSpec {
    pub id: String,
    pub atoms: Vec<PatternAtom>,
    pub constraints: Vec<PatternConstraint>,
    pub confidence: Confidence,
    /// When false, a successful match consumes its range for this pattern.
    pub allow_overlap: bool,
}

impl PatternSpec {
    pub fn new(id: impl Into<String>, atoms: Vec<PatternAtom>) -> Self {
        Self {
            id: id.into(),
            atoms,
            constraints: Vec::new(),
            confidence: Confidence::HIGH,
            allow_overlap: false,
        }
    }

    pub fn constrained(mut self, constraint: PatternConstraint) -> Self {
        self.constraints.push(constraint);
        self
    }

    pub fn with_confidence(mut self, confidence: Confidence) -> Self {
        self.confidence = confidence;
        self
    }

    pub fn allowing_overlap(mut self) -> Self {
        self.allow_overlap = true;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PatternValidation {
    pub valid: bool,
    pub messages: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PatternMatch {
    pub pattern_id: String,
    pub provenance: Provenance,
    pub captures: BTreeMap<String, usize>,
    pub confidence: Confidence,
    pub validation: PatternValidation,
}

impl PatternMatch {
    pub fn annotation(&self) -> RecoveryAnnotation {
        let mut annotation = RecoveryAnnotation::exact(
            RecoveredConstruct::CompilerPattern,
            self.provenance,
            format!("pattern:{}", self.pattern_id),
        );
        annotation.confidence = self.confidence;
        if !self.validation.valid {
            annotation.confidence = annotation.confidence.min(Confidence::LOW);
            annotation
                .approximations
                .push(Approximation::PatternValidation);
        }
        annotation
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PatternRegistry {
    patterns: Vec<PatternSpec>,
}

impl PatternRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn standard() -> Self {
        let closure_call = PatternSpec::new(
            "closure.create_then_call",
            vec![
                PatternAtom::captured("StaticClosure", "closure"),
                PatternAtom::captured("CallClosure", "call"),
            ],
        )
        .constrained(PatternConstraint::OperandEqual {
            left_atom: 0,
            left_operand: "dst".to_owned(),
            right_atom: 1,
            right_operand: "fun".to_owned(),
        })
        .with_confidence(Confidence::CERTAIN);

        let reference_round_trip = PatternSpec::new(
            "reference.take_then_read",
            vec![
                PatternAtom::captured("Ref", "reference"),
                PatternAtom::captured("Unref", "read"),
            ],
        )
        .constrained(PatternConstraint::OperandEqual {
            left_atom: 0,
            left_operand: "dst".to_owned(),
            right_atom: 1,
            right_operand: "src".to_owned(),
        });

        Self::new()
            .register(closure_call)
            .register(reference_round_trip)
    }

    pub fn register(mut self, pattern: PatternSpec) -> Self {
        self.patterns.push(pattern);
        self.patterns.sort_by(|left, right| left.id.cmp(&right.id));
        self
    }

    pub fn patterns(&self) -> &[PatternSpec] {
        &self.patterns
    }

    pub fn match_function(&self, function: &Function) -> Vec<PatternMatch> {
        let mut matches = Vec::new();
        for pattern in &self.patterns {
            if pattern.atoms.is_empty() || pattern.atoms.len() > function.ops.len() {
                continue;
            }
            let mut start = 0;
            while start + pattern.atoms.len() <= function.ops.len() {
                let window = &function.ops[start..start + pattern.atoms.len()];
                if pattern
                    .atoms
                    .iter()
                    .zip(window)
                    .all(|(atom, opcode)| atom.predicate.matches(opcode))
                {
                    let mut captures = BTreeMap::new();
                    for (offset, atom) in pattern.atoms.iter().enumerate() {
                        if let Some(capture) = &atom.capture {
                            captures.insert(capture.clone(), start + offset);
                        }
                    }
                    let validation = validate(pattern, window);
                    matches.push(PatternMatch {
                        pattern_id: pattern.id.clone(),
                        provenance: Provenance::new(
                            function.findex.0,
                            start,
                            start + pattern.atoms.len(),
                        ),
                        captures,
                        confidence: pattern.confidence,
                        validation,
                    });
                    start += if pattern.allow_overlap {
                        1
                    } else {
                        pattern.atoms.len()
                    };
                } else {
                    start += 1;
                }
            }
        }
        matches.sort_by(|left, right| {
            (
                left.provenance.function_index,
                left.provenance.opcode_start,
                left.provenance.opcode_end,
                &left.pattern_id,
            )
                .cmp(&(
                    right.provenance.function_index,
                    right.provenance.opcode_start,
                    right.provenance.opcode_end,
                    &right.pattern_id,
                ))
        });
        matches
    }
}

fn validate(pattern: &PatternSpec, window: &[Opcode]) -> PatternValidation {
    let mut messages = Vec::new();
    for constraint in &pattern.constraints {
        match constraint {
            PatternConstraint::OperandEqual {
                left_atom,
                left_operand,
                right_atom,
                right_operand,
            } => {
                let left = operand(window.get(*left_atom), left_operand);
                let right = operand(window.get(*right_atom), right_operand);
                if left.is_none() || right.is_none() || left != right {
                    messages.push(format!(
                        "operand flow {}.{} == {}.{} was not satisfied",
                        left_atom, left_operand, right_atom, right_operand
                    ));
                }
            }
            PatternConstraint::HasOperand {
                atom,
                operand: name,
            } => {
                if operand(window.get(*atom), name).is_none() {
                    messages.push(format!("atom {atom} has no operand {name}"));
                }
            }
        }
    }
    PatternValidation {
        valid: messages.is_empty(),
        messages,
    }
}

fn operand(opcode: Option<&Opcode>, name: &str) -> Option<String> {
    opcode?
        .operands()
        .into_iter()
        .find(|operand| operand.name == name)
        .map(|operand| operand.value)
}

#[cfg(test)]
mod tests {
    use hlbc::opcodes::Opcode;
    use hlbc::types::{Function, RefFun, RefType, Reg};

    use super::{PatternRegistry, PatternSpec};

    fn function(ops: Vec<Opcode>) -> Function {
        Function {
            t: RefType(0),
            findex: RefFun(9),
            regs: vec![RefType(0); 3],
            ops,
            debug_info: None,
            assigns: None,
            name: Default::default(),
            parent: None,
        }
    }

    #[test]
    fn standard_pattern_validates_register_flow_and_provenance() {
        let function = function(vec![
            Opcode::StaticClosure {
                dst: Reg(1),
                fun: RefFun(2),
            },
            Opcode::CallClosure {
                dst: Reg(0),
                fun: Reg(1),
                args: Vec::new(),
            },
        ]);
        let matches = PatternRegistry::standard().match_function(&function);
        assert_eq!(matches.len(), 1);
        assert!(matches[0].validation.valid);
        assert_eq!(matches[0].provenance.opcode_start, 0);
        assert_eq!(matches[0].provenance.opcode_end, 2);
    }

    #[test]
    fn failed_validation_is_reported_without_claiming_a_match_is_sound() {
        let function = function(vec![
            Opcode::StaticClosure {
                dst: Reg(1),
                fun: RefFun(2),
            },
            Opcode::CallClosure {
                dst: Reg(0),
                fun: Reg(2),
                args: Vec::new(),
            },
        ]);
        let matches = PatternRegistry::standard().match_function(&function);
        assert_eq!(matches.len(), 1);
        assert!(!matches[0].validation.valid);
        assert!(!matches[0].validation.messages.is_empty());
    }

    #[test]
    fn empty_patterns_are_ignored() {
        let registry = PatternRegistry::new().register(PatternSpec::new("empty", Vec::new()));
        assert!(registry.match_function(&function(Vec::new())).is_empty());
    }
}

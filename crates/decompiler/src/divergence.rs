//! Attribution of recompilation opcode mismatches to AST and IR regions.

use hlbc::opcodes::{Opcode, OpcodeOperand};
use hlbc::types::Function;
use serde::Serialize;

use crate::ast::{Expr, Operation, RuntimeCheck, StateTerminator, Statement, StringPart};
use crate::diagnostics::Provenance;
use crate::ir::{IrProvenance, OpcodeRange, TypedIr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DivergenceKind {
    Replacement,
    MissingFromRecompiled,
    AddedByRecompiler,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OpcodeSnapshot {
    pub name: String,
    pub operands: Vec<OpcodeOperand>,
}

impl From<&Opcode> for OpcodeSnapshot {
    fn from(opcode: &Opcode) -> Self {
        Self {
            name: opcode.name().to_owned(),
            operands: opcode.operands(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AstRegion {
    pub path: String,
    pub node_kind: String,
    pub provenance: Provenance,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DivergenceRegion {
    pub ast: Option<AstRegion>,
    pub ir: Option<IrProvenance>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OpcodeDivergence {
    pub kind: DivergenceKind,
    pub original: Option<OpcodeRange>,
    pub recompiled: Option<OpcodeRange>,
    pub expected: Vec<OpcodeSnapshot>,
    pub actual: Vec<OpcodeSnapshot>,
    pub responsible_region: DivergenceRegion,
}

pub fn attribute_opcode_divergences(
    original: &Function,
    recompiled: Option<&Function>,
    ir: &TypedIr,
    statements: &[Statement],
) -> Vec<OpcodeDivergence> {
    let empty = Vec::new();
    let recompiled_ops = recompiled.map(|function| &function.ops).unwrap_or(&empty);
    let mut divergences = opcode_diff(&original.ops, recompiled_ops);
    let ast_regions = collect_ast_regions(statements);
    for divergence in &mut divergences {
        let opcode_index = divergence
            .original
            .filter(|range| !range.is_empty())
            .map(|range| range.start)
            .or_else(|| {
                divergence
                    .original
                    .map(|range| range.start.saturating_sub(1))
            });
        let ast = opcode_index.and_then(|opcode_index| {
            smallest_ast_region(&ast_regions, original.findex.0, opcode_index)
        });
        let ir = opcode_index.and_then(|opcode_index| {
            ir.blocks
                .iter()
                .flat_map(|block| &block.operations)
                .filter(|operation| {
                    operation
                        .provenance
                        .opcode_ranges
                        .iter()
                        .any(|range| range.start <= opcode_index && opcode_index < range.end)
                })
                .min_by_key(|operation| {
                    operation
                        .provenance
                        .opcode_ranges
                        .iter()
                        .map(|range| range.end.saturating_sub(range.start))
                        .sum::<usize>()
                })
                .map(|operation| operation.provenance.clone())
        });
        divergence.responsible_region = DivergenceRegion { ast, ir };
    }
    divergences
}

pub fn opcode_diff(original: &[Opcode], recompiled: &[Opcode]) -> Vec<OpcodeDivergence> {
    let mut lcs = vec![vec![0usize; recompiled.len() + 1]; original.len() + 1];
    for left in (0..original.len()).rev() {
        for right in (0..recompiled.len()).rev() {
            lcs[left][right] = if original[left] == recompiled[right] {
                lcs[left + 1][right + 1] + 1
            } else {
                lcs[left + 1][right].max(lcs[left][right + 1])
            };
        }
    }
    let mut left = 0;
    let mut right = 0;
    let mut result = Vec::new();
    while left < original.len() || right < recompiled.len() {
        if left < original.len() && right < recompiled.len() && original[left] == recompiled[right]
        {
            left += 1;
            right += 1;
            continue;
        }
        let original_start = left;
        let recompiled_start = right;
        while left < original.len() || right < recompiled.len() {
            if left < original.len()
                && right < recompiled.len()
                && original[left] == recompiled[right]
            {
                break;
            }
            if right == recompiled.len()
                || (left < original.len() && lcs[left + 1][right] >= lcs[left][right + 1])
            {
                left += 1;
            } else {
                right += 1;
            }
        }
        let original_range = OpcodeRange::new(original_start, left);
        let recompiled_range = OpcodeRange::new(recompiled_start, right);
        let kind = if original_range.is_empty() {
            DivergenceKind::AddedByRecompiler
        } else if recompiled_range.is_empty() {
            DivergenceKind::MissingFromRecompiled
        } else {
            DivergenceKind::Replacement
        };
        result.push(OpcodeDivergence {
            kind,
            original: Some(original_range),
            recompiled: Some(recompiled_range),
            expected: original[original_start..left]
                .iter()
                .map(OpcodeSnapshot::from)
                .collect(),
            actual: recompiled[recompiled_start..right]
                .iter()
                .map(OpcodeSnapshot::from)
                .collect(),
            responsible_region: DivergenceRegion {
                ast: None,
                ir: None,
            },
        });
    }
    result
}

pub fn collect_ast_regions(statements: &[Statement]) -> Vec<AstRegion> {
    let mut regions = Vec::new();
    for (index, statement) in statements.iter().enumerate() {
        visit_statement(statement, &format!("body[{index}]"), &mut regions);
    }
    regions.sort_by(|left, right| {
        (
            left.provenance.function_index,
            left.provenance.opcode_start,
            left.provenance.opcode_end,
            &left.path,
        )
            .cmp(&(
                right.provenance.function_index,
                right.provenance.opcode_start,
                right.provenance.opcode_end,
                &right.path,
            ))
    });
    regions
}

fn smallest_ast_region(
    regions: &[AstRegion],
    function_index: usize,
    opcode_index: usize,
) -> Option<AstRegion> {
    regions
        .iter()
        .filter(|region| {
            region
                .provenance
                .contains_opcode(function_index, opcode_index)
        })
        .min_by(|left, right| {
            left.provenance
                .len()
                .cmp(&right.provenance.len())
                .then_with(|| {
                    right
                        .path
                        .matches('/')
                        .count()
                        .cmp(&left.path.matches('/').count())
                })
                .then_with(|| left.path.cmp(&right.path))
        })
        .cloned()
}

fn push_region(regions: &mut Vec<AstRegion>, path: &str, node_kind: &str, provenance: Provenance) {
    regions.push(AstRegion {
        path: path.to_owned(),
        node_kind: node_kind.to_owned(),
        provenance,
    });
}

fn visit_statement(statement: &Statement, path: &str, regions: &mut Vec<AstRegion>) {
    match statement {
        Statement::Provenanced {
            statement,
            provenance,
        } => {
            push_region(regions, path, "statement", *provenance);
            visit_statement(statement, &format!("{path}/inner"), regions);
        }
        Statement::UnhandledOpcode { provenance, .. } => {
            push_region(regions, path, "unhandled_opcode", *provenance)
        }
        Statement::VarDecl {
            variable, value, ..
        } => {
            visit_expr(variable, &format!("{path}/variable"), regions);
            if let Some(value) = value {
                visit_expr(value, &format!("{path}/value"), regions);
            }
        }
        Statement::Assign {
            variable, assign, ..
        } => {
            visit_expr(variable, &format!("{path}/variable"), regions);
            visit_expr(assign, &format!("{path}/assign"), regions);
        }
        Statement::ExprStatement(expression) | Statement::Throw(expression) => {
            visit_expr(expression, &format!("{path}/expression"), regions)
        }
        Statement::GlobalStore { value, .. } => {
            visit_expr(value, &format!("{path}/value"), regions)
        }
        Statement::DynamicFieldStore { object, value, .. } => {
            visit_expr(object, &format!("{path}/object"), regions);
            visit_expr(value, &format!("{path}/value"), regions);
        }
        Statement::MemoryStore {
            bytes,
            index,
            value,
            ..
        } => {
            visit_expr(bytes, &format!("{path}/bytes"), regions);
            visit_expr(index, &format!("{path}/index"), regions);
            visit_expr(value, &format!("{path}/value"), regions);
        }
        Statement::ReferenceStore {
            reference, value, ..
        } => {
            visit_expr(reference, &format!("{path}/reference"), regions);
            visit_expr(value, &format!("{path}/value"), regions);
        }
        Statement::RuntimeCheck(RuntimeCheck::Null(value)) => {
            visit_expr(value, &format!("{path}/null_check"), regions)
        }
        Statement::RuntimeCheck(RuntimeCheck::Assert)
        | Statement::Nop
        | Statement::Break
        | Statement::Continue
        | Statement::Comment(_) => {}
        Statement::Prefetch { value, .. } => visit_expr(value, &format!("{path}/value"), regions),
        Statement::Return(value) => {
            if let Some(value) = value {
                visit_expr(value, &format!("{path}/return"), regions);
            }
        }
        Statement::IfElse { cond, if_, else_ } => {
            visit_expr(cond, &format!("{path}/condition"), regions);
            visit_statements(if_, &format!("{path}/then"), regions);
            visit_statements(else_, &format!("{path}/else"), regions);
        }
        Statement::Switch {
            arg,
            default,
            cases,
        } => {
            visit_expr(arg, &format!("{path}/argument"), regions);
            visit_statements(default, &format!("{path}/default"), regions);
            for (index, (patterns, body)) in cases.iter().enumerate() {
                for (pattern_index, pattern) in patterns.iter().enumerate() {
                    visit_expr(
                        pattern,
                        &format!("{path}/case[{index}]/pattern[{pattern_index}]"),
                        regions,
                    );
                }
                visit_statements(body, &format!("{path}/case[{index}]"), regions);
            }
        }
        Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
            visit_expr(cond, &format!("{path}/condition"), regions);
            visit_statements(stmts, &format!("{path}/body"), regions);
        }
        Statement::ForEach {
            variable,
            iterable,
            stmts,
        } => {
            visit_expr(variable, &format!("{path}/variable"), regions);
            visit_expr(iterable, &format!("{path}/iterable"), regions);
            visit_statements(stmts, &format!("{path}/body"), regions);
        }
        Statement::ForRange {
            variable,
            start,
            end,
            stmts,
        } => {
            visit_expr(variable, &format!("{path}/variable"), regions);
            visit_expr(start, &format!("{path}/start"), regions);
            visit_expr(end, &format!("{path}/end"), regions);
            visit_statements(stmts, &format!("{path}/body"), regions);
        }
        Statement::Try { stmts } | Statement::Catch { stmts } => {
            visit_statements(stmts, &format!("{path}/body"), regions)
        }
        Statement::TryCatch { try_stmts, catches } => {
            visit_statements(try_stmts, &format!("{path}/try"), regions);
            for (index, catch) in catches.iter().enumerate() {
                visit_expr(
                    &catch.variable,
                    &format!("{path}/catch[{index}]/variable"),
                    regions,
                );
                visit_statements(
                    &catch.stmts,
                    &format!("{path}/catch[{index}]/body"),
                    regions,
                );
            }
        }
        Statement::StateMachine { locals, blocks, .. } => {
            for (index, local) in locals.iter().enumerate() {
                visit_expr(local, &format!("{path}/local[{index}]"), regions);
            }
            for block in blocks {
                let block_path = format!("{path}/state[{}]", block.state);
                visit_statements(&block.stmts, &format!("{block_path}/body"), regions);
                match &block.terminator {
                    StateTerminator::Branch { cond, .. } => {
                        visit_expr(cond, &format!("{block_path}/branch"), regions)
                    }
                    StateTerminator::Switch { arg, .. } => {
                        visit_expr(arg, &format!("{block_path}/switch"), regions)
                    }
                    StateTerminator::Return(Some(value)) | StateTerminator::Throw(value) => {
                        visit_expr(value, &format!("{block_path}/terminator"), regions)
                    }
                    StateTerminator::Goto(_)
                    | StateTerminator::Return(None)
                    | StateTerminator::Exit => {}
                }
            }
        }
    }
}

fn visit_statements(statements: &[Statement], path: &str, regions: &mut Vec<AstRegion>) {
    for (index, statement) in statements.iter().enumerate() {
        visit_statement(statement, &format!("{path}[{index}]"), regions);
    }
}

fn visit_expr(expression: &Expr, path: &str, regions: &mut Vec<AstRegion>) {
    match expression {
        Expr::Provenanced {
            expression,
            provenance,
        } => {
            push_region(regions, path, "expression", *provenance);
            visit_expr(expression, &format!("{path}/inner"), regions);
        }
        Expr::Anonymous(_, fields) => {
            for (field, value) in fields {
                visit_expr(value, &format!("{path}/field[{}]", field.0), regions);
            }
        }
        Expr::Array(array, index) => {
            visit_expr(array, &format!("{path}/array"), regions);
            visit_expr(index, &format!("{path}/index"), regions);
        }
        Expr::ArrayLiteral { elements, .. } => {
            for (index, element) in elements.iter().enumerate() {
                visit_expr(element, &format!("{path}/element[{index}]"), regions);
            }
        }
        Expr::MapLiteral { entries } => {
            for (index, (key, value)) in entries.iter().enumerate() {
                visit_expr(key, &format!("{path}/entry[{index}]/key"), regions);
                visit_expr(value, &format!("{path}/entry[{index}]/value"), regions);
            }
        }
        Expr::ArrayAlloc { length, .. } => visit_expr(length, &format!("{path}/length"), regions),
        Expr::Call(call) => {
            visit_expr(&call.fun, &format!("{path}/callee"), regions);
            for (index, argument) in call.args.iter().enumerate() {
                visit_expr(argument, &format!("{path}/argument[{index}]"), regions);
            }
        }
        Expr::Constructor(call) => {
            for (index, argument) in call.args.iter().enumerate() {
                visit_expr(argument, &format!("{path}/argument[{index}]"), regions);
            }
        }
        Expr::Closure(_, statements) => {
            visit_statements(statements, &format!("{path}/closure"), regions)
        }
        Expr::EnumConstr(_, _, arguments) | Expr::SuperCall(arguments) => {
            for (index, argument) in arguments.iter().enumerate() {
                visit_expr(argument, &format!("{path}/argument[{index}]"), regions);
            }
        }
        Expr::EnumIndex(value) => visit_expr(value, &format!("{path}/value"), regions),
        Expr::EnumPatternBinding(_, _, variables) => {
            for (index, variable) in variables.iter().enumerate() {
                visit_expr(variable, &format!("{path}/binding[{index}]"), regions);
            }
        }
        Expr::EnumField { value, .. }
        | Expr::Field(value, _)
        | Expr::DynamicField(value, _)
        | Expr::RuntimeType { value, .. }
        | Expr::TypeId { value, .. }
        | Expr::VirtualClosure {
            receiver: value, ..
        }
        | Expr::Dereference {
            reference: value, ..
        }
        | Expr::ReferenceData { array: value, .. }
        | Expr::ToString(value) => visit_expr(value, &format!("{path}/value"), regions),
        Expr::SuperMethod { args, .. } => {
            for (index, argument) in args.iter().enumerate() {
                visit_expr(argument, &format!("{path}/argument[{index}]"), regions);
            }
        }
        Expr::MemoryLoad { bytes, index, .. } => {
            visit_expr(bytes, &format!("{path}/bytes"), regions);
            visit_expr(index, &format!("{path}/index"), regions);
        }
        Expr::Reference { value, .. } => visit_expr(value, &format!("{path}/value"), regions),
        Expr::ReferenceOffset {
            reference, offset, ..
        } => {
            visit_expr(reference, &format!("{path}/reference"), regions);
            visit_expr(offset, &format!("{path}/offset"), regions);
        }
        Expr::IfElse { cond, if_, else_ } => {
            visit_expr(cond, &format!("{path}/condition"), regions);
            visit_statements(if_, &format!("{path}/then"), regions);
            visit_statements(else_, &format!("{path}/else"), regions);
        }
        Expr::Op(operation) => visit_operation(operation, path, regions),
        Expr::StringConcat(expressions) => {
            for (index, expression) in expressions.iter().enumerate() {
                visit_expr(expression, &format!("{path}/part[{index}]"), regions);
            }
        }
        Expr::StringInterpolation(parts) => {
            for (index, part) in parts.iter().enumerate() {
                if let StringPart::Expression(expression) = part {
                    visit_expr(expression, &format!("{path}/part[{index}]"), regions);
                }
            }
        }
        Expr::Bytes(_)
        | Expr::Constant(_)
        | Expr::EnumPattern(_, _, _)
        | Expr::FunRef(_)
        | Expr::TypeValue { .. }
        | Expr::Unknown(_)
        | Expr::Variable(_, _) => {}
    }
}

fn visit_operation(operation: &Operation, path: &str, regions: &mut Vec<AstRegion>) {
    use Operation::*;
    match operation {
        Add(left, right)
        | Sub(left, right)
        | Mul(left, right)
        | Div(left, right)
        | Mod(left, right)
        | Shl(left, right)
        | Shr(left, right)
        | And(left, right)
        | Or(left, right)
        | Xor(left, right)
        | Eq(left, right)
        | NotEq(left, right)
        | Gt(left, right)
        | Gte(left, right)
        | Lt(left, right)
        | Lte(left, right) => {
            visit_expr(left, &format!("{path}/left"), regions);
            visit_expr(right, &format!("{path}/right"), regions);
        }
        Neg(value) | Not(value) | Incr(value) | Decr(value) => {
            visit_expr(value, &format!("{path}/operand"), regions)
        }
    }
}

#[cfg(test)]
mod tests {
    use hlbc::opcodes::Opcode;
    use hlbc::Bytecode;

    use super::attribute_opcode_divergences;
    use crate::decompile_code;
    use crate::ir::TypedIr;

    #[test]
    fn mismatch_is_attributed_to_ast_and_ir_provenance() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let original = code.function_by_name("main").unwrap();
        let mut recompiled = original.clone();
        recompiled.ops.insert(0, Opcode::Nop);
        let ir = TypedIr::build(&code, original).unwrap().value;
        let statements = decompile_code(&code, original).unwrap().value;
        let divergences =
            attribute_opcode_divergences(original, Some(&recompiled), &ir, &statements);
        assert!(!divergences.is_empty());
        assert!(divergences
            .iter()
            .any(|divergence| divergence.responsible_region.ir.is_some()));
    }
}

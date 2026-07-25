//! Synchronized faithful and readable Haxe views.

use hlbc::types::Function;
use hlbc::Bytecode;
use serde::Serialize;

use crate::ast::Statement;
use crate::decompile_code_with_options;
use crate::diagnostics::{
    DecompileError, DecompileOptions, Diagnostic, Provenance, RecoveryAnnotation,
};
use crate::fmt::FormatOptions;
use crate::ir::{OperationId, TypedIr};
use crate::optimize::OptimizationProfile;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ViewKind {
    Faithful,
    Readable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SourceSpan {
    pub byte_start: usize,
    pub byte_end: usize,
    pub line_start: usize,
    pub line_end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ViewNode {
    pub id: usize,
    pub kind: ViewKind,
    pub ast_path: String,
    pub provenance: Option<Provenance>,
    pub source: SourceSpan,
    pub annotations: Vec<RecoveryAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HaxeView {
    pub kind: ViewKind,
    pub source: String,
    pub nodes: Vec<ViewNode>,
}

impl HaxeView {
    pub fn nodes_for_opcode(
        &self,
        function_index: usize,
        opcode_index: usize,
    ) -> impl Iterator<Item = &ViewNode> {
        self.nodes.iter().filter(move |node| {
            node.provenance.map_or(false, |provenance| {
                provenance.contains_opcode(function_index, opcode_index)
            })
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OpcodeViewLink {
    pub opcode_index: usize,
    pub ir_operations: Vec<OperationId>,
    pub faithful_nodes: Vec<usize>,
    pub readable_nodes: Vec<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SynchronizedViews {
    pub function_index: usize,
    pub shared_ir: TypedIr,
    pub faithful: HaxeView,
    pub readable: HaxeView,
    pub opcode_links: Vec<OpcodeViewLink>,
    pub diagnostics: Vec<Diagnostic>,
    pub recovery_annotations: Vec<RecoveryAnnotation>,
}

impl SynchronizedViews {
    pub fn link_for_opcode(&self, opcode_index: usize) -> Option<&OpcodeViewLink> {
        self.opcode_links.get(opcode_index)
    }
}

pub fn decompile_synchronized_views(
    code: &Bytecode,
    function: &Function,
    base_options: DecompileOptions,
) -> Result<SynchronizedViews, DecompileError> {
    let ir_result = TypedIr::build(code, function)?;
    let mut faithful_options = base_options;
    faithful_options.optimization_profile = OptimizationProfile::Fidelity;
    let mut readable_options = base_options;
    readable_options.optimization_profile = OptimizationProfile::Readability;
    let faithful_result = decompile_code_with_options(code, function, faithful_options)?;
    let readable_result = decompile_code_with_options(code, function, readable_options)?;

    let mut diagnostics = ir_result.diagnostics;
    diagnostics.extend(faithful_result.diagnostics);
    diagnostics.extend(readable_result.diagnostics);
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
    diagnostics.dedup_by(|left, right| {
        left.function_index == right.function_index
            && left.opcode_index == right.opcode_index
            && left.severity == right.severity
            && left.message == right.message
    });

    let mut recovery_annotations = faithful_result.recovery_annotations;
    recovery_annotations.extend(readable_result.recovery_annotations);
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
    recovery_annotations.dedup();

    let faithful = render_view(
        code,
        function,
        &faithful_result.value,
        ViewKind::Faithful,
        &recovery_annotations,
    );
    let readable = render_view(
        code,
        function,
        &readable_result.value,
        ViewKind::Readable,
        &recovery_annotations,
    );
    let opcode_links = build_links(function, &ir_result.value, &faithful, &readable);
    Ok(SynchronizedViews {
        function_index: function.findex.0,
        shared_ir: ir_result.value,
        faithful,
        readable,
        opcode_links,
        diagnostics,
        recovery_annotations,
    })
}

fn render_view(
    code: &Bytecode,
    function: &Function,
    statements: &[Statement],
    kind: ViewKind,
    annotations: &[RecoveryAnnotation],
) -> HaxeView {
    let mut source = String::from("{\n");
    let mut nodes = Vec::with_capacity(statements.len());
    let indent = FormatOptions::new(4).inc_nesting();
    for (index, statement) in statements.iter().enumerate() {
        let byte_start = source.len();
        let line_start = source.lines().count() + 1;
        let rendered = statement.display(&indent, code, function).to_string();
        source.push_str(&rendered);
        if !rendered.ends_with('\n') {
            source.push('\n');
        }
        let byte_end = source.len();
        let line_end = source.lines().count();
        let provenance = statement.provenance();
        let node_annotations = provenance
            .map(|provenance| {
                annotations
                    .iter()
                    .filter(|annotation| {
                        annotation.provenance.function_index == provenance.function_index
                            && annotation.provenance.opcode_start < provenance.opcode_end
                            && provenance.opcode_start < annotation.provenance.opcode_end
                    })
                    .cloned()
                    .collect()
            })
            .unwrap_or_default();
        nodes.push(ViewNode {
            id: index,
            kind,
            ast_path: format!("body[{index}]"),
            provenance,
            source: SourceSpan {
                byte_start,
                byte_end,
                line_start,
                line_end,
            },
            annotations: node_annotations,
        });
    }
    source.push_str("}\n");
    HaxeView {
        kind,
        source,
        nodes,
    }
}

fn build_links(
    function: &Function,
    ir: &TypedIr,
    faithful: &HaxeView,
    readable: &HaxeView,
) -> Vec<OpcodeViewLink> {
    (0..function.ops.len())
        .map(|opcode_index| {
            let mut ir_operations: Vec<_> = ir
                .blocks
                .iter()
                .flat_map(|block| &block.operations)
                .filter(|operation| {
                    operation
                        .provenance
                        .opcode_ranges
                        .iter()
                        .any(|range| range.start <= opcode_index && opcode_index < range.end)
                })
                .map(|operation| operation.id)
                .collect();
            ir_operations.sort();
            ir_operations.dedup();
            OpcodeViewLink {
                opcode_index,
                ir_operations,
                faithful_nodes: faithful
                    .nodes_for_opcode(function.findex.0, opcode_index)
                    .map(|node| node.id)
                    .collect(),
                readable_nodes: readable
                    .nodes_for_opcode(function.findex.0, opcode_index)
                    .map(|node| node.id)
                    .collect(),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use hlbc::Bytecode;

    use super::decompile_synchronized_views;
    use crate::diagnostics::DecompileOptions;

    #[test]
    fn views_share_complete_deterministic_opcode_links() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let function = code.function_by_name("main").unwrap();
        let first =
            decompile_synchronized_views(&code, function, DecompileOptions::default()).unwrap();
        let second =
            decompile_synchronized_views(&code, function, DecompileOptions::default()).unwrap();
        assert_eq!(first.opcode_links.len(), function.ops.len());
        assert_eq!(
            serde_json::to_string(&first).unwrap(),
            serde_json::to_string(&second).unwrap()
        );
        assert!(first.faithful.source.starts_with("{\n"));
        assert!(first.readable.source.ends_with("}\n"));
    }
}

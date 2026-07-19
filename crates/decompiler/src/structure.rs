//! Dominance-based control-flow regions and structured-edge verification.

use std::collections::{BTreeMap, BTreeSet};

use serde::Serialize;

use crate::cfg::{
    ControlFlowGraph, Edge, EdgeKind, ExceptionRegion, FallbackRegion, NaturalLoop, NodeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum LoopKind {
    While,
    DoWhile,
    Infinite,
    Natural,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConditionalRegion {
    pub header: NodeId,
    pub taken: NodeId,
    pub fallthrough: NodeId,
    pub join: Option<NodeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct LoopRegion {
    pub header: NodeId,
    pub latches: BTreeSet<NodeId>,
    pub nodes: BTreeSet<NodeId>,
    pub exits: BTreeSet<NodeId>,
    pub kind: LoopKind,
}

#[derive(Debug, Clone, Serialize)]
pub struct SwitchTarget {
    pub target: NodeId,
    pub cases: Vec<usize>,
    pub is_default: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct SwitchRegion {
    pub header: NodeId,
    pub targets: Vec<SwitchTarget>,
    pub join: Option<NodeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TryRegion {
    pub trap_opcode: usize,
    pub protected_nodes: BTreeSet<NodeId>,
    pub handler: NodeId,
    pub exception_register: u32,
    pub catch_opcodes: Vec<usize>,
    pub catch_globals: Vec<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Region {
    Conditional(ConditionalRegion),
    Loop(LoopRegion),
    Switch(SwitchRegion),
    Try(TryRegion),
    Fallback(FallbackRegion),
}

/// Graph-level region facts used by the statement emitter. Regions may overlap:
/// nesting is established by dominance and node-set containment during emission.
#[derive(Debug, Clone, Serialize)]
pub struct RegionAnalysis {
    pub regions: Vec<Region>,
}

impl RegionAnalysis {
    pub fn analyze(cfg: &ControlFlowGraph) -> Self {
        let post_dominators = cfg.normal_post_dominators();
        let mut regions = Vec::new();

        for block in &cfg.blocks {
            let node = NodeId::Block(block.id);
            let mut taken = None;
            let mut fallthrough = None;
            let mut switch_targets: BTreeMap<NodeId, SwitchTarget> = BTreeMap::new();
            for edge in cfg.edges.iter().filter(|edge| edge.from == node) {
                match edge.kind {
                    EdgeKind::BranchTaken => taken = Some(edge.to),
                    EdgeKind::Fallthrough => fallthrough = Some(edge.to),
                    EdgeKind::SwitchCase(case) => {
                        switch_targets
                            .entry(edge.to)
                            .or_insert_with(|| SwitchTarget {
                                target: edge.to,
                                cases: Vec::new(),
                                is_default: false,
                            })
                            .cases
                            .push(case);
                    }
                    EdgeKind::SwitchDefault => {
                        switch_targets
                            .entry(edge.to)
                            .or_insert_with(|| SwitchTarget {
                                target: edge.to,
                                cases: Vec::new(),
                                is_default: false,
                            })
                            .is_default = true;
                    }
                    _ => {}
                }
            }
            let join = post_dominators.immediate.get(&node).copied().flatten();
            if let (Some(taken), Some(fallthrough)) = (taken, fallthrough) {
                regions.push(Region::Conditional(ConditionalRegion {
                    header: node,
                    taken,
                    fallthrough,
                    join,
                }));
            }
            if !switch_targets.is_empty() {
                regions.push(Region::Switch(SwitchRegion {
                    header: node,
                    targets: switch_targets.into_values().collect(),
                    join,
                }));
            }
        }

        let mut loops: BTreeMap<NodeId, LoopRegion> = BTreeMap::new();
        for natural_loop in cfg.natural_loops() {
            merge_loop(cfg, &mut loops, natural_loop);
        }
        regions.extend(loops.into_values().map(Region::Loop));
        regions.extend(
            cfg.exception_regions
                .iter()
                .map(|region| Region::Try(try_region(cfg, region))),
        );
        regions.extend(
            cfg.irreducible_regions
                .iter()
                .cloned()
                .map(Region::Fallback),
        );

        regions.sort_by_key(region_order);
        Self { regions }
    }

    pub fn loops(&self) -> impl Iterator<Item = &LoopRegion> {
        self.regions.iter().filter_map(|region| match region {
            Region::Loop(region) => Some(region),
            _ => None,
        })
    }

    pub fn conditionals(&self) -> impl Iterator<Item = &ConditionalRegion> {
        self.regions.iter().filter_map(|region| match region {
            Region::Conditional(region) => Some(region),
            _ => None,
        })
    }

    pub fn switches(&self) -> impl Iterator<Item = &SwitchRegion> {
        self.regions.iter().filter_map(|region| match region {
            Region::Switch(region) => Some(region),
            _ => None,
        })
    }
}

fn merge_loop(
    cfg: &ControlFlowGraph,
    loops: &mut BTreeMap<NodeId, LoopRegion>,
    natural_loop: NaturalLoop,
) {
    let region = loops
        .entry(natural_loop.header)
        .or_insert_with(|| LoopRegion {
            header: natural_loop.header,
            latches: BTreeSet::new(),
            nodes: BTreeSet::new(),
            exits: BTreeSet::new(),
            kind: LoopKind::Natural,
        });
    region.latches.insert(natural_loop.latch);
    region.nodes.extend(natural_loop.nodes);

    region.exits = cfg
        .edges
        .iter()
        .filter(|edge| {
            region.nodes.contains(&edge.from)
                && !region.nodes.contains(&edge.to)
                && edge.kind != EdgeKind::Exception
        })
        .map(|edge| edge.to)
        .collect();

    let header_has_conditional_exit = cfg.edges.iter().any(|edge| {
        edge.from == region.header
            && !region.nodes.contains(&edge.to)
            && matches!(edge.kind, EdgeKind::BranchTaken | EdgeKind::Fallthrough)
    });
    let latch_has_conditional_exit = region.latches.iter().any(|latch| {
        cfg.edges.iter().any(|edge| {
            edge.from == *latch
                && !region.nodes.contains(&edge.to)
                && matches!(edge.kind, EdgeKind::BranchTaken | EdgeKind::Fallthrough)
        })
    });
    region.kind = if region.latches.contains(&region.header) && header_has_conditional_exit {
        LoopKind::DoWhile
    } else if header_has_conditional_exit {
        LoopKind::While
    } else if latch_has_conditional_exit {
        LoopKind::DoWhile
    } else {
        // A loop controlled by `break` has CFG exits but no loop-condition
        // edge at its header or latch.
        LoopKind::Infinite
    };
}

fn try_region(cfg: &ControlFlowGraph, region: &ExceptionRegion) -> TryRegion {
    let protected_nodes = cfg
        .blocks
        .iter()
        .filter(|block| block.start < region.end && region.start < block.end)
        .map(|block| NodeId::Block(block.id))
        .collect();
    let handler = cfg
        .blocks
        .iter()
        .find(|block| block.start == region.handler)
        .map_or(NodeId::Exit, |block| NodeId::Block(block.id));
    TryRegion {
        trap_opcode: region.trap_opcode,
        protected_nodes,
        handler,
        exception_register: region.exception_register.0,
        catch_opcodes: region.catch_opcodes.clone(),
        catch_globals: region.catch_globals.clone(),
    }
}

fn region_order(region: &Region) -> (usize, u8) {
    let (node, kind) = match region {
        Region::Conditional(region) => (region.header, 1),
        Region::Loop(region) => (region.header, 0),
        Region::Switch(region) => (region.header, 2),
        Region::Try(region) => (NodeId::Block(region.trap_opcode), 3),
        Region::Fallback(region) => (
            region
                .entry_nodes
                .iter()
                .next()
                .copied()
                .unwrap_or(NodeId::Exit),
            4,
        ),
    };
    let order = match node {
        NodeId::Entry => 0,
        NodeId::Block(id) => id + 1,
        NodeId::Exit => usize::MAX,
    };
    (order, kind)
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("structured control-flow verification failed: {message}")]
pub struct StructureVerificationError {
    pub message: String,
    pub missing: Vec<Edge>,
    pub extra: Vec<Edge>,
}

/// Records the CFG edges represented by emitted structured constructs.
#[derive(Debug, Clone, Default)]
pub struct StructuredEdgeSet {
    edges: BTreeSet<Edge>,
}

impl StructuredEdgeSet {
    pub fn record(&mut self, edge: &Edge) {
        self.edges.insert(edge.clone());
    }

    pub fn record_from(&mut self, cfg: &ControlFlowGraph, node: NodeId) {
        self.edges
            .extend(cfg.edges.iter().filter(|edge| edge.from == node).cloned());
    }

    pub fn record_all(&mut self, cfg: &ControlFlowGraph) {
        self.edges.extend(cfg.edges.iter().cloned());
    }

    pub fn verify(&self, cfg: &ControlFlowGraph) -> Result<(), StructureVerificationError> {
        let expected: BTreeSet<_> = cfg.edges.iter().cloned().collect();
        let missing: Vec<_> = expected.difference(&self.edges).cloned().collect();
        let extra: Vec<_> = self.edges.difference(&expected).cloned().collect();
        if missing.is_empty() && extra.is_empty() {
            Ok(())
        } else {
            Err(StructureVerificationError {
                message: format!(
                    "{} missing edge(s), {} extra edge(s)",
                    missing.len(),
                    extra.len()
                ),
                missing,
                extra,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use hlbc::opcodes::Opcode;
    use hlbc::types::{Function, RefFun, RefString, RefType, Reg};

    use super::*;

    fn function(ops: Vec<Opcode>) -> Function {
        Function {
            t: RefType(0),
            findex: RefFun(12),
            regs: vec![RefType(0); 4],
            ops,
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        }
    }

    #[test]
    fn classifies_while_and_conditional_regions() {
        let cfg = ControlFlowGraph::build(&function(vec![
            Opcode::Label,
            Opcode::JFalse {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::Nop,
            Opcode::JAlways { offset: -4 },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let analysis = RegionAnalysis::analyze(&cfg);
        assert!(analysis
            .loops()
            .any(|region| region.kind == LoopKind::While));
        assert_eq!(analysis.conditionals().count(), 1);
    }

    #[test]
    fn groups_shared_switch_targets() {
        let cfg = ControlFlowGraph::build(&function(vec![
            Opcode::Switch {
                reg: Reg(0),
                offsets: vec![1, 1, 2],
                end: 3,
            },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
            Opcode::Ret { ret: Reg(0) },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let analysis = RegionAnalysis::analyze(&cfg);
        let switch = analysis.switches().next().unwrap();
        assert!(switch.targets.iter().any(|target| target.cases == [0, 1]));
        assert!(switch.targets.iter().any(|target| target.is_default));
    }

    #[test]
    fn verifier_rejects_a_lost_branch() {
        let cfg = ControlFlowGraph::build(&function(vec![
            Opcode::JFalse {
                cond: Reg(0),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(0) },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let mut emitted = StructuredEdgeSet::default();
        emitted.record(&cfg.edges[0]);
        let error = emitted.verify(&cfg).unwrap_err();
        assert!(!error.missing.is_empty());
    }
}

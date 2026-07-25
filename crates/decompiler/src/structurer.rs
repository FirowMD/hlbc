use std::collections::{BTreeMap, BTreeSet};

use hlbc::opcodes::Opcode;
use hlbc::types::{Function, RefEnumConstruct, RefType, Type};
use hlbc::Bytecode;

use crate::ast::{
    cst_bool, cst_inline_int, not, CatchClause, Constant, ExceptionTransition, Expr, Operation,
    StateMachineBlock, StateTerminator, Statement,
};
use crate::cfg::{ControlFlowGraph, Edge, EdgeKind, ExceptionRegion, NodeId};
use crate::structure::{LoopKind, LoopRegion, RegionAnalysis, StructuredEdgeSet};
use crate::FlatOutput;

#[derive(Debug)]
pub(crate) struct StructuredOutput {
    pub statements: Vec<Statement>,
    pub used_fallback: bool,
    pub fallback_reason: Option<String>,
}

pub(crate) fn structure_function(
    code: &Bytecode,
    function: &Function,
    cfg: &ControlFlowGraph,
    flat: &FlatOutput,
    include_unreachable: bool,
) -> Result<StructuredOutput, String> {
    let analysis = RegionAnalysis::analyze(cfg);
    if cfg.irreducible_regions.is_empty() {
        let mut emitter =
            RegionEmitter::new(code, function, cfg, flat, &analysis, include_unreachable);
        match emitter.emit_root() {
            Ok(statements) => match emitter.covered.verify(cfg) {
                Ok(()) => {
                    return Ok(StructuredOutput {
                        statements,
                        used_fallback: false,
                        fallback_reason: None,
                    })
                }
                Err(error) => {
                    let reason = error.to_string();
                    let (statements, covered) = state_machine(code, function, cfg, flat)?;
                    covered.verify(cfg).map_err(|error| error.to_string())?;
                    return Ok(StructuredOutput {
                        statements,
                        used_fallback: true,
                        fallback_reason: Some(reason),
                    });
                }
            },
            Err(reason) => {
                let (statements, covered) = state_machine(code, function, cfg, flat)?;
                covered.verify(cfg).map_err(|error| error.to_string())?;
                return Ok(StructuredOutput {
                    statements,
                    used_fallback: true,
                    fallback_reason: Some(reason),
                });
            }
        }
    }

    let (statements, covered) = state_machine(code, function, cfg, flat)?;
    covered.verify(cfg).map_err(|error| error.to_string())?;
    Ok(StructuredOutput {
        statements,
        used_fallback: true,
        fallback_reason: Some("irreducible CFG requires state-machine control flow".to_owned()),
    })
}

struct RegionEmitter<'a> {
    code: &'a Bytecode,
    function: &'a Function,
    cfg: &'a ControlFlowGraph,
    flat: &'a FlatOutput,
    loops: BTreeMap<NodeId, LoopRegion>,
    traps: BTreeMap<NodeId, ExceptionRegion>,
    visited: BTreeSet<NodeId>,
    covered: StructuredEdgeSet,
    loop_stack: Vec<NodeId>,
    include_unreachable: bool,
}

impl<'a> RegionEmitter<'a> {
    fn new(
        code: &'a Bytecode,
        function: &'a Function,
        cfg: &'a ControlFlowGraph,
        flat: &'a FlatOutput,
        analysis: &RegionAnalysis,
        include_unreachable: bool,
    ) -> Self {
        let loops = analysis
            .loops()
            .cloned()
            .map(|region| (region.header, region))
            .collect();
        let traps = cfg
            .exception_regions
            .iter()
            .filter_map(|region| {
                cfg.blocks
                    .iter()
                    .find(|block| {
                        block.start <= region.trap_opcode && region.trap_opcode < block.end
                    })
                    .map(|block| (NodeId::Block(block.id), region.clone()))
            })
            .collect();
        Self {
            code,
            function,
            cfg,
            flat,
            loops,
            traps,
            visited: BTreeSet::new(),
            covered: StructuredEdgeSet::default(),
            loop_stack: Vec::new(),
            include_unreachable,
        }
    }

    fn emit_root(&mut self) -> Result<Vec<Statement>, String> {
        let entry = self
            .cfg
            .edges
            .iter()
            .find(|edge| edge.from == NodeId::Entry && edge.kind == EdgeKind::Entry)
            .ok_or_else(|| "CFG has no entry edge".to_owned())?;
        self.covered.record(entry);
        let mut statements = self.emit_sequence(entry.to, NodeId::Exit, None)?;
        let reachable = self.cfg.reachable();
        if !self.include_unreachable {
            for block in &self.cfg.blocks {
                let node = NodeId::Block(block.id);
                if !reachable.contains(&node) {
                    self.visited.insert(node);
                    self.covered.record_from(self.cfg, node);
                }
            }
        }
        if self.cfg.blocks.iter().any(|block| {
            let node = NodeId::Block(block.id);
            !self.visited.contains(&node)
        }) {
            return Err("normal regions do not own every CFG block".to_owned());
        }
        // A void return at the synthetic exit is implicit in Haxe.
        while matches!(statements.last(), Some(Statement::Return(None))) {
            statements.pop();
        }
        Ok(statements)
    }

    fn emit_sequence(
        &mut self,
        mut current: NodeId,
        stop: NodeId,
        allowed: Option<&BTreeSet<NodeId>>,
    ) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while current != stop && current != NodeId::Exit {
            if allowed.map_or(false, |nodes| !nodes.contains(&current)) {
                return Err(format!("region escaped to {current:?} before {stop:?}"));
            }
            if self.visited.contains(&current) {
                return Err(format!("region reaches already emitted block {current:?}"));
            }

            if let Some(region) = self.traps.get(&current).cloned() {
                let (statement, next) = self.emit_try(current, &region)?;
                statements.push(statement);
                current = next;
                continue;
            }
            if let Some(region) = self.loops.get(&current).cloned() {
                if !self.loop_stack.contains(&current) {
                    let (statement, next) = self.emit_loop(&region)?;
                    statements.push(statement);
                    current = next;
                    continue;
                }
            }

            let node = current;
            self.visited.insert(node);
            statements.extend(self.block_statements(node)?);
            let block = self.block(node)?;
            let opcode_index = block.end - 1;
            let opcode = &self.function.ops[opcode_index];
            let edges = self.normal_edges(node);

            if is_conditional(opcode) {
                let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
                let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
                let cond = self
                    .flat
                    .branch_conditions
                    .get(&opcode_index)
                    .cloned()
                    .ok_or_else(|| format!("missing branch predicate at opcode {opcode_index}"))?;
                if let Some((switch, next)) = self.emit_comparison_switch(node, allowed)? {
                    statements.push(switch);
                    current = next;
                    continue;
                }
                for edge in &edges {
                    self.covered.record(edge);
                }

                if let Some(loop_header) = self.loop_stack.last().copied() {
                    let loop_region = &self.loops[&loop_header];
                    if taken == loop_header || fallthrough == loop_header {
                        let (continue_cond, next) = if taken == loop_header {
                            (cond, fallthrough)
                        } else {
                            (not(cond), taken)
                        };
                        statements.push(Statement::IfElse {
                            cond: continue_cond,
                            if_: vec![Statement::Continue],
                            else_: Vec::new(),
                        });
                        current = next;
                        continue;
                    }
                    let exits = &loop_region.exits;
                    if exits.contains(&taken) || exits.contains(&fallthrough) {
                        let (break_cond, next) = if exits.contains(&taken) {
                            (cond, fallthrough)
                        } else {
                            (not(cond), taken)
                        };
                        statements.push(Statement::IfElse {
                            cond: break_cond,
                            if_: vec![Statement::Break],
                            else_: Vec::new(),
                        });
                        current = next;
                        continue;
                    }
                }

                let join = self
                    .cfg
                    .normal_post_dominators()
                    .immediate
                    .get(&node)
                    .copied()
                    .flatten()
                    .ok_or_else(|| format!("conditional {node:?} has no normal post-dominator"))?;
                if self.is_terminal_path(taken, fallthrough, join) && taken != fallthrough {
                    let terminal = self.emit_sequence(taken, NodeId::Exit, allowed)?;
                    statements.push(Statement::IfElse {
                        cond,
                        if_: terminal,
                        else_: Vec::new(),
                    });
                    current = fallthrough;
                    continue;
                }
                if self.is_terminal_path(fallthrough, taken, join) && taken != fallthrough {
                    let terminal = self.emit_sequence(fallthrough, NodeId::Exit, allowed)?;
                    statements.push(Statement::IfElse {
                        cond: not(cond),
                        if_: terminal,
                        else_: Vec::new(),
                    });
                    current = taken;
                    continue;
                }

                let (cond, if_target, else_target) = if self.is_conditional_block(taken)
                    && !self.is_conditional_block(fallthrough)
                {
                    (not(cond), fallthrough, taken)
                } else {
                    (cond, taken, fallthrough)
                };
                let if_ = self.emit_sequence(if_target, join, allowed)?;
                let else_ = self.emit_sequence(else_target, join, allowed)?;
                statements.push(Statement::IfElse { cond, if_, else_ });
                current = join;
                continue;
            }

            if matches!(opcode, Opcode::Switch { .. }) {
                let join = self
                    .cfg
                    .normal_post_dominators()
                    .immediate
                    .get(&node)
                    .copied()
                    .flatten()
                    .ok_or_else(|| format!("switch {node:?} has no normal post-dominator"))?;
                let arg = self
                    .flat
                    .switch_args
                    .get(&opcode_index)
                    .cloned()
                    .ok_or_else(|| format!("missing switch selector at opcode {opcode_index}"))?;
                let enum_selector = self.enum_selector(&arg);
                let switch_arg = enum_selector
                    .as_ref()
                    .map_or_else(|| arg.clone(), |(value, _)| value.clone());
                let mut grouped: BTreeMap<NodeId, Vec<usize>> = BTreeMap::new();
                let mut default_target = None;
                for edge in &edges {
                    self.covered.record(edge);
                    match edge.kind {
                        EdgeKind::SwitchCase(case) => {
                            grouped.entry(edge.to).or_default().push(case)
                        }
                        EdgeKind::SwitchDefault => default_target = Some(edge.to),
                        _ => {}
                    }
                }
                let mut cases = Vec::new();
                for (target, labels) in grouped {
                    let body = self.emit_sequence(target, join, allowed)?;
                    let patterns = if let Some((_, enum_type)) = enum_selector {
                        labels
                            .into_iter()
                            .map(|label| self.enum_pattern(enum_type, label))
                            .collect::<Result<Vec<_>, _>>()?
                    } else {
                        labels.into_iter().map(cst_inline_int).collect()
                    };
                    cases.push((patterns, body));
                }
                let default = match default_target {
                    Some(target) if target != join => self.emit_sequence(target, join, allowed)?,
                    _ => Vec::new(),
                };
                statements.push(Statement::Switch {
                    arg: switch_arg,
                    default,
                    cases,
                });
                current = join;
                continue;
            }

            match opcode {
                Opcode::Ret { .. } => {
                    self.record_edges(&edges);
                    statements.push(Statement::Return(
                        self.flat
                            .returns
                            .get(&opcode_index)
                            .cloned()
                            .unwrap_or(None),
                    ));
                    current = NodeId::Exit;
                }
                Opcode::Throw { .. } | Opcode::Rethrow { .. } => {
                    self.record_edges(&edges);
                    let value = self
                        .flat
                        .throws
                        .get(&opcode_index)
                        .cloned()
                        .ok_or_else(|| format!("missing throw value at opcode {opcode_index}"))?;
                    statements.push(Statement::Throw(value));
                    current = NodeId::Exit;
                }
                _ => {
                    let target = single_target(&edges)?;
                    self.record_edges(&edges);
                    if let Some(loop_header) = self.loop_stack.last().copied() {
                        let loop_region = &self.loops[&loop_header];
                        if target == loop_header {
                            if !loop_region.latches.contains(&node) {
                                statements.push(Statement::Continue);
                            }
                            current = stop;
                            continue;
                        }
                        if loop_region.exits.contains(&target) {
                            statements.push(Statement::Break);
                            current = stop;
                            continue;
                        }
                    }
                    current = target;
                }
            }
        }
        Ok(statements)
    }

    fn emit_loop(&mut self, region: &LoopRegion) -> Result<(Statement, NodeId), String> {
        if region.exits.len() > 1 {
            return Err(format!(
                "loop {:?} has multiple exits requiring labelled control",
                region.header
            ));
        }
        let exit = region.exits.iter().next().copied().unwrap_or(NodeId::Exit);
        self.loop_stack.push(region.header);
        let result = match region.kind {
            LoopKind::While if self.block_statements(region.header)?.is_empty() => {
                self.emit_while(region, exit)
            }
            LoopKind::While => self.emit_infinite_loop(region, exit),
            LoopKind::DoWhile => self.emit_do_while(region, exit),
            LoopKind::Infinite => self.emit_infinite_loop(region, exit),
            LoopKind::Natural => Err(format!(
                "natural loop {:?} does not match while/do-while/infinite patterns",
                region.header
            )),
        };
        self.loop_stack.pop();
        result
    }

    fn emit_while(
        &mut self,
        region: &LoopRegion,
        exit: NodeId,
    ) -> Result<(Statement, NodeId), String> {
        let header = region.header;
        self.visited.insert(header);
        let header_statements = self.block_statements(header)?;
        if !header_statements.is_empty() {
            return Err(format!("while header {header:?} contains side effects"));
        }
        let block = self.block(header)?;
        let opcode_index = block.end - 1;
        let cond = self
            .flat
            .branch_conditions
            .get(&opcode_index)
            .cloned()
            .ok_or_else(|| format!("while header {header:?} has no predicate"))?;
        let edges = self.normal_edges(header);
        let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
        let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
        self.record_edges(&edges);
        let (cond, body_start) = if region.nodes.contains(&taken) && fallthrough == exit {
            (cond, taken)
        } else if region.nodes.contains(&fallthrough) && taken == exit {
            (not(cond), fallthrough)
        } else {
            return Err(format!(
                "while header {header:?} does not have one body and one exit edge"
            ));
        };
        let body = self.emit_sequence(body_start, header, Some(&region.nodes))?;
        Ok((Statement::While { cond, stmts: body }, exit))
    }

    fn emit_do_while(
        &mut self,
        region: &LoopRegion,
        exit: NodeId,
    ) -> Result<(Statement, NodeId), String> {
        if region.latches.len() != 1 {
            return Err(format!("do-while {:?} has multiple latches", region.header));
        }
        let latch = *region.latches.iter().next().unwrap();
        let header = region.header;
        if latch == header {
            self.visited.insert(header);
            let body = self.block_statements(header)?;
            let block = self.block(header)?;
            let opcode_index = block.end - 1;
            let cond = self
                .flat
                .branch_conditions
                .get(&opcode_index)
                .cloned()
                .ok_or_else(|| format!("do-while block {header:?} has no predicate"))?;
            let edges = self.normal_edges(header);
            let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
            let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
            self.record_edges(&edges);
            let cond = if taken == header && fallthrough == exit {
                cond
            } else if fallthrough == header && taken == exit {
                not(cond)
            } else {
                return Err(format!("do-while block {header:?} has invalid targets"));
            };
            return Ok((Statement::DoWhile { cond, stmts: body }, exit));
        }
        self.visited.insert(header);
        let mut body = self.block_statements(header)?;
        let header_edges = self.normal_edges(header);
        let body_start = single_target(&header_edges)?;
        self.record_edges(&header_edges);
        if body_start != latch {
            body.extend(self.emit_sequence(body_start, latch, Some(&region.nodes))?);
        }
        if !self.visited.insert(latch) {
            return Err(format!("do-while latch {latch:?} was emitted early"));
        }
        body.extend(self.block_statements(latch)?);
        let latch_block = self.block(latch)?;
        let opcode_index = latch_block.end - 1;
        let cond = self
            .flat
            .branch_conditions
            .get(&opcode_index)
            .cloned()
            .ok_or_else(|| format!("do-while latch {latch:?} has no predicate"))?;
        let edges = self.normal_edges(latch);
        let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
        let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
        self.record_edges(&edges);
        let cond = if taken == header && fallthrough == exit {
            cond
        } else if fallthrough == header && taken == exit {
            not(cond)
        } else {
            return Err(format!("do-while latch {latch:?} has invalid targets"));
        };
        Ok((Statement::DoWhile { cond, stmts: body }, exit))
    }

    fn emit_infinite_loop(
        &mut self,
        region: &LoopRegion,
        exit: NodeId,
    ) -> Result<(Statement, NodeId), String> {
        let header = region.header;
        self.visited.insert(header);
        let mut body = self.block_statements(header)?;
        let edges = self.normal_edges(header);
        let start = if edges.iter().any(|edge| edge.kind == EdgeKind::BranchTaken) {
            let block = self.block(header)?;
            let opcode_index = block.end - 1;
            let cond = self
                .flat
                .branch_conditions
                .get(&opcode_index)
                .cloned()
                .ok_or_else(|| format!("infinite loop header {header:?} has no predicate"))?;
            let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
            let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
            let (break_cond, start) = if taken == exit && region.nodes.contains(&fallthrough) {
                (cond, fallthrough)
            } else if fallthrough == exit && region.nodes.contains(&taken) {
                (not(cond), taken)
            } else {
                return Err(format!(
                    "infinite loop header {header:?} has invalid exit targets"
                ));
            };
            body.push(Statement::IfElse {
                cond: break_cond,
                if_: vec![Statement::Break],
                else_: Vec::new(),
            });
            start
        } else {
            single_target(&edges)?
        };
        self.record_edges(&edges);
        if start != header {
            body.extend(self.emit_sequence(start, header, Some(&region.nodes))?);
        }
        Ok((
            Statement::While {
                cond: cst_bool(true),
                stmts: body,
            },
            exit,
        ))
    }

    fn emit_try(
        &mut self,
        trap_node: NodeId,
        region: &ExceptionRegion,
    ) -> Result<(Statement, NodeId), String> {
        self.visited.insert(trap_node);
        self.record_edges(&self.normal_edges(trap_node));

        let start = self.node_at_opcode(region.start)?;
        let handler = self.node_at_opcode(region.handler)?;
        // Catch opcodes are type metadata between Trap and the protected body.
        for &opcode in &region.catch_opcodes {
            let node = self.node_at_opcode(opcode)?;
            self.visited.insert(node);
            self.record_edges(&self.normal_edges(node));
        }

        let protected: BTreeSet<_> = self
            .cfg
            .blocks
            .iter()
            .filter(|block| block.start < region.end && region.start < block.end)
            .map(|block| NodeId::Block(block.id))
            .collect();
        let exits: BTreeSet<_> = self
            .cfg
            .edges
            .iter()
            .filter(|edge| {
                protected.contains(&edge.from)
                    && !protected.contains(&edge.to)
                    && edge.kind != EdgeKind::Exception
            })
            .map(|edge| edge.to)
            .collect();
        let continuation = exits
            .iter()
            .copied()
            .find(|node| *node != NodeId::Exit)
            .unwrap_or(NodeId::Exit);
        let try_stmts = self.emit_sequence(start, continuation, Some(&protected))?;
        for edge in self
            .cfg
            .edges
            .iter()
            .filter(|edge| protected.contains(&edge.from) && edge.kind == EdgeKind::Exception)
        {
            self.covered.record(edge);
        }
        let catch_stmts = self.emit_sequence(handler, continuation, None)?;
        let variable = Expr::Variable(
            region.exception_register,
            self.function.var_name(self.code, region.trap_opcode),
        );
        let catch_types = catch_types(self.code, self.function, region);
        let catches = catch_types
            .into_iter()
            .map(|variable_type| CatchClause {
                variable: variable.clone(),
                variable_type,
                stmts: catch_stmts.clone(),
            })
            .collect();
        Ok((Statement::TryCatch { try_stmts, catches }, continuation))
    }

    fn block_statements(&self, node: NodeId) -> Result<Vec<Statement>, String> {
        let block = self.block(node)?;
        Ok(block
            .range()
            .filter(|&index| !is_structural(&self.function.ops[index]))
            .flat_map(|index| {
                self.flat
                    .statements
                    .get(&index)
                    .into_iter()
                    .flatten()
                    .cloned()
            })
            .collect())
    }

    fn block(&self, node: NodeId) -> Result<&crate::cfg::BasicBlock, String> {
        match node {
            NodeId::Block(id) => self
                .cfg
                .blocks
                .get(id)
                .ok_or_else(|| format!("missing CFG block {id}")),
            _ => Err(format!("expected basic block, got {node:?}")),
        }
    }

    fn node_at_opcode(&self, opcode: usize) -> Result<NodeId, String> {
        self.cfg
            .blocks
            .iter()
            .find(|block| block.start <= opcode && opcode < block.end)
            .map(|block| NodeId::Block(block.id))
            .ok_or_else(|| format!("opcode {opcode} has no CFG block"))
    }

    fn normal_edges(&self, node: NodeId) -> Vec<Edge> {
        self.cfg
            .edges
            .iter()
            .filter(|edge| edge.from == node && edge.kind != EdgeKind::Exception)
            .cloned()
            .collect()
    }

    fn record_edges(&mut self, edges: &[Edge]) {
        for edge in edges {
            self.covered.record(edge);
        }
    }

    fn is_terminal_path(&self, mut node: NodeId, excluded: NodeId, join: NodeId) -> bool {
        let mut seen = BTreeSet::new();
        loop {
            if node == excluded || !seen.insert(node) {
                return false;
            }
            if node == join && join != NodeId::Exit {
                return false;
            }
            if node == NodeId::Exit {
                return true;
            }
            let Ok(block) = self.block(node) else {
                return false;
            };
            if matches!(
                self.function.ops[block.end - 1],
                Opcode::Ret { .. } | Opcode::Throw { .. } | Opcode::Rethrow { .. }
            ) {
                return true;
            }
            let edges = self.normal_edges(node);
            let Ok(target) = single_target(&edges) else {
                return false;
            };
            node = target;
        }
    }

    fn is_conditional_block(&self, node: NodeId) -> bool {
        self.block(node)
            .map(|block| is_conditional(&self.function.ops[block.end - 1]))
            .unwrap_or(false)
    }

    fn enum_selector(&self, expression: &Expr) -> Option<(Expr, RefType)> {
        let Expr::EnumIndex(value) = expression else {
            return None;
        };
        let enum_type = match value.as_ref() {
            Expr::Variable(register, _) => self.function.regtype(*register),
            Expr::EnumConstr(ty, _, _) => *ty,
            _ => return None,
        };
        matches!(self.code.types.get(enum_type.0), Some(Type::Enum { .. }))
            .then(|| (value.as_ref().clone(), enum_type))
    }

    fn enum_pattern(&self, enum_type: RefType, index: usize) -> Result<Expr, String> {
        let Some(Type::Enum { constructs, .. }) = self.code.types.get(enum_type.0) else {
            return Err(format!("t{} is not an enum", enum_type.0));
        };
        let arity = constructs
            .get(index)
            .ok_or_else(|| format!("enum t{} has no constructor {index}", enum_type.0))?
            .params
            .len();
        Ok(Expr::EnumPattern(enum_type, RefEnumConstruct(index), arity))
    }

    fn emit_comparison_switch(
        &mut self,
        first: NodeId,
        allowed: Option<&BTreeSet<NodeId>>,
    ) -> Result<Option<(Statement, NodeId)>, String> {
        let join = match self
            .cfg
            .normal_post_dominators()
            .immediate
            .get(&first)
            .copied()
            .flatten()
        {
            Some(join) => join,
            None => return Ok(None),
        };
        let mut selector = None;
        let mut headers = Vec::new();
        let mut current = first;
        let default_target;
        loop {
            let block = self.block(current)?;
            let opcode_index = block.end - 1;
            if !is_conditional(&self.function.ops[opcode_index]) {
                default_target = current;
                break;
            }
            if current != first && !self.block_statements(current)?.is_empty() {
                return Ok(None);
            }
            let Some(condition) = self.flat.branch_conditions.get(&opcode_index) else {
                return Ok(None);
            };
            let Some((candidate, pattern, equal_when_true)) = split_switch_equality(condition)
            else {
                return Ok(None);
            };
            if let Some(selector) = &selector {
                if !same_selector(selector, &candidate) {
                    return Ok(None);
                }
            } else {
                if !is_stable_selector(&candidate) {
                    return Ok(None);
                }
                selector = Some(candidate);
            }
            let edges = self.normal_edges(current);
            let taken = edge_target(&edges, |kind| kind == &EdgeKind::BranchTaken)?;
            let fallthrough = edge_target(&edges, |kind| kind == &EdgeKind::Fallthrough)?;
            let (case_target, unmatched_target) = if equal_when_true {
                (taken, fallthrough)
            } else {
                (fallthrough, taken)
            };
            headers.push((current, pattern, case_target, edges));
            if unmatched_target == join {
                default_target = join;
                break;
            }
            if headers
                .iter()
                .any(|(header, ..)| *header == unmatched_target)
            {
                return Ok(None);
            }
            current = unmatched_target;
        }
        if headers.len() < 2 {
            return Ok(None);
        }

        let mut grouped: BTreeMap<NodeId, Vec<Expr>> = BTreeMap::new();
        for (index, (header, pattern, target, edges)) in headers.into_iter().enumerate() {
            if index > 0 && !self.visited.insert(header) {
                return Err(format!(
                    "comparison-switch header {header:?} was already emitted"
                ));
            }
            self.record_edges(&edges);
            grouped.entry(target).or_default().push(pattern);
        }
        let mut cases = Vec::new();
        for (target, patterns) in grouped {
            cases.push((patterns, self.emit_sequence(target, join, allowed)?));
        }
        let default = if default_target == join {
            Vec::new()
        } else {
            self.emit_sequence(default_target, join, allowed)?
        };
        Ok(Some((
            Statement::Switch {
                arg: selector.expect("comparison switch has a selector"),
                default,
                cases,
            },
            join,
        )))
    }
}

fn split_switch_equality(condition: &Expr) -> Option<(Expr, Expr, bool)> {
    let (left, right, equal_when_true) = match condition {
        Expr::Op(Operation::Eq(left, right)) => (left.as_ref(), right.as_ref(), true),
        Expr::Op(Operation::NotEq(left, right)) => (left.as_ref(), right.as_ref(), false),
        _ => return None,
    };
    if is_string_or_enum_pattern(right) {
        Some((left.clone(), right.clone(), equal_when_true))
    } else if is_string_or_enum_pattern(left) {
        Some((right.clone(), left.clone(), equal_when_true))
    } else {
        None
    }
}

fn is_string_or_enum_pattern(expression: &Expr) -> bool {
    matches!(
        expression,
        Expr::Constant(Constant::String(_)) | Expr::EnumConstr(_, _, _)
    )
}

fn same_selector(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Variable(left, _), Expr::Variable(right, _)) => left == right,
        (Expr::Constant(Constant::String(left)), Expr::Constant(Constant::String(right))) => {
            left == right
        }
        (
            Expr::Provenanced {
                expression: left, ..
            },
            right,
        ) => same_selector(left, right),
        (
            left,
            Expr::Provenanced {
                expression: right, ..
            },
        ) => same_selector(left, right),
        _ => false,
    }
}

fn is_stable_selector(expression: &Expr) -> bool {
    match expression {
        Expr::Variable(..) | Expr::Constant(..) => true,
        Expr::Provenanced { expression, .. } => is_stable_selector(expression),
        _ => false,
    }
}

fn state_machine(
    code: &Bytecode,
    function: &Function,
    cfg: &ControlFlowGraph,
    flat: &FlatOutput,
) -> Result<(Vec<Statement>, StructuredEdgeSet), String> {
    let entry = cfg
        .edges
        .iter()
        .find(|edge| edge.from == NodeId::Entry && edge.kind == EdgeKind::Entry)
        .ok_or_else(|| "CFG has no entry edge".to_owned())?;
    let entry_state = state_for(entry.to)?;
    let mut covered = StructuredEdgeSet::default();
    covered.record(entry);
    let mut blocks = Vec::new();
    let mut locals = BTreeMap::new();
    for block in &cfg.blocks {
        let node = NodeId::Block(block.id);
        let normal_edges: Vec<_> = cfg
            .edges
            .iter()
            .filter(|edge| edge.from == node && edge.kind != EdgeKind::Exception)
            .cloned()
            .collect();
        let exception_edge = cfg
            .edges
            .iter()
            .find(|edge| edge.from == node && edge.kind == EdgeKind::Exception);
        let opcode_index = block.end - 1;
        let opcode = &function.ops[opcode_index];
        let mut stmts: Vec<_> = block
            .range()
            .filter(|&index| !is_structural(&function.ops[index]))
            .flat_map(|index| flat.statements.get(&index).into_iter().flatten().cloned())
            .collect();
        hoist_declarations(&mut stmts, &mut locals);
        let terminator = match opcode {
            opcode if is_conditional(opcode) => {
                let taken = edge_target(&normal_edges, |kind| kind == &EdgeKind::BranchTaken)?;
                let fallthrough =
                    edge_target(&normal_edges, |kind| kind == &EdgeKind::Fallthrough)?;
                StateTerminator::Branch {
                    cond: flat
                        .branch_conditions
                        .get(&opcode_index)
                        .cloned()
                        .ok_or_else(|| {
                            format!("missing branch predicate at opcode {opcode_index}")
                        })?,
                    taken: state_for(taken)?,
                    fallthrough: state_for(fallthrough)?,
                }
            }
            Opcode::Switch { .. } => {
                let mut cases = Vec::new();
                let mut default = None;
                for edge in &normal_edges {
                    match edge.kind {
                        EdgeKind::SwitchCase(case) => cases.push((case, state_for(edge.to)?)),
                        EdgeKind::SwitchDefault => default = Some(state_for(edge.to)?),
                        _ => {}
                    }
                }
                StateTerminator::Switch {
                    arg: flat
                        .switch_args
                        .get(&opcode_index)
                        .cloned()
                        .ok_or_else(|| {
                            format!("missing switch selector at opcode {opcode_index}")
                        })?,
                    cases,
                    default: default.ok_or_else(|| format!("switch {node:?} has no default"))?,
                }
            }
            Opcode::Ret { .. } => {
                StateTerminator::Return(flat.returns.get(&opcode_index).cloned().unwrap_or(None))
            }
            Opcode::Throw { .. } | Opcode::Rethrow { .. } => StateTerminator::Throw(
                flat.throws
                    .get(&opcode_index)
                    .cloned()
                    .ok_or_else(|| format!("missing throw value at opcode {opcode_index}"))?,
            ),
            _ => match normal_edges.as_slice() {
                [edge] if edge.to == NodeId::Exit => StateTerminator::Exit,
                [edge] => StateTerminator::Goto(state_for(edge.to)?),
                [] => StateTerminator::Exit,
                _ => return Err(format!("block {node:?} has ambiguous normal edges")),
            },
        };
        let exception = if let Some(edge) = exception_edge {
            let region = innermost_region_for_block(cfg, block.start, block.end)
                .ok_or_else(|| format!("exception edge from {node:?} has no trap region"))?;
            let variable = Expr::Variable(
                region.exception_register,
                function.var_name(code, region.trap_opcode),
            );
            locals
                .entry(region.exception_register.0)
                .or_insert_with(|| variable.clone());
            Some(ExceptionTransition {
                variable,
                variable_types: catch_types(code, function, region),
                handler_state: state_for(edge.to)?,
            })
        } else {
            None
        };
        for edge in &normal_edges {
            covered.record(edge);
        }
        if let Some(edge) = exception_edge {
            covered.record(edge);
        }
        blocks.push(StateMachineBlock {
            state: block.id,
            stmts,
            terminator,
            exception,
        });
    }
    Ok((
        vec![Statement::StateMachine {
            entry_state,
            locals: locals.into_values().collect(),
            blocks,
        }],
        covered,
    ))
}

fn catch_types(code: &Bytecode, function: &Function, region: &ExceptionRegion) -> Vec<RefType> {
    let types: Vec<_> = region
        .catch_globals
        .iter()
        .filter_map(|&global| {
            code.types
                .iter()
                .enumerate()
                .find_map(|(index, ty)| match ty {
                    Type::Obj(object) | Type::Struct(object) if object.global.0 == global + 1 => {
                        Some(RefType(index))
                    }
                    _ => None,
                })
        })
        .collect();
    if types.is_empty() {
        vec![function.regtype(region.exception_register)]
    } else {
        types
    }
}

fn hoist_declarations(statements: &mut [Statement], locals: &mut BTreeMap<u32, Expr>) {
    for statement in statements {
        if let Statement::Assign {
            declaration,
            variable,
            ..
        } = statement
        {
            if let Expr::Variable(reg, _) = variable {
                let register = reg.0;
                let variable = variable.clone();
                locals.entry(register).or_insert(variable);
                if *declaration {
                    *declaration = false;
                }
            }
        }
    }
}

fn innermost_region_for_block(
    cfg: &ControlFlowGraph,
    start: usize,
    end: usize,
) -> Option<&ExceptionRegion> {
    cfg.exception_regions
        .iter()
        .filter(|region| start < region.end && region.start < end)
        .max_by_key(|region| (region.start, std::cmp::Reverse(region.end)))
}

fn state_for(node: NodeId) -> Result<usize, String> {
    match node {
        NodeId::Block(id) => Ok(id),
        NodeId::Exit => Ok(usize::MAX),
        NodeId::Entry => Err("transition targets synthetic entry".to_owned()),
    }
}

fn edge_target(edges: &[Edge], predicate: impl Fn(&EdgeKind) -> bool) -> Result<NodeId, String> {
    edges
        .iter()
        .find(|edge| predicate(&edge.kind))
        .map(|edge| edge.to)
        .ok_or_else(|| format!("missing expected edge in {edges:?}"))
}

fn single_target(edges: &[Edge]) -> Result<NodeId, String> {
    match edges {
        [edge] => Ok(edge.to),
        _ => Err(format!("expected one normal edge, got {edges:?}")),
    }
}

fn is_conditional(opcode: &Opcode) -> bool {
    matches!(
        opcode,
        Opcode::JTrue { .. }
            | Opcode::JFalse { .. }
            | Opcode::JNull { .. }
            | Opcode::JNotNull { .. }
            | Opcode::JSLt { .. }
            | Opcode::JSGte { .. }
            | Opcode::JSGt { .. }
            | Opcode::JSLte { .. }
            | Opcode::JULt { .. }
            | Opcode::JUGte { .. }
            | Opcode::JNotLt { .. }
            | Opcode::JNotGte { .. }
            | Opcode::JEq { .. }
            | Opcode::JNotEq { .. }
    )
}

fn is_structural(opcode: &Opcode) -> bool {
    is_conditional(opcode)
        || matches!(
            opcode,
            Opcode::JAlways { .. }
                | Opcode::Switch { .. }
                | Opcode::Label
                | Opcode::Ret { .. }
                | Opcode::Throw { .. }
                | Opcode::Rethrow { .. }
                | Opcode::Trap { .. }
                | Opcode::EndTrap { .. }
                | Opcode::Catch { .. }
        )
}

#[cfg(test)]
mod tests {
    use hlbc::types::Reg;

    use super::*;

    #[test]
    fn exit_state_is_not_a_real_block() {
        assert_eq!(state_for(NodeId::Exit).unwrap(), usize::MAX);
        assert!(state_for(NodeId::Entry).is_err());
    }

    #[test]
    fn catch_global_resolves_to_its_instance_type() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let function = code.function_by_name("main").unwrap();
        let (type_index, global) = code
            .types
            .iter()
            .enumerate()
            .find_map(|(index, ty)| match ty {
                Type::Obj(object) | Type::Struct(object) if object.global.0 > 0 => {
                    Some((index, object.global.0 - 1))
                }
                _ => None,
            })
            .unwrap();
        let region = ExceptionRegion {
            trap_opcode: 0,
            start: 0,
            end: 1,
            handler: 1,
            exception_register: Reg(0),
            catch_opcodes: vec![0],
            catch_globals: vec![global],
            end_trap_opcodes: Vec::new(),
        };
        assert_eq!(
            catch_types(&code, function, &region),
            vec![RefType(type_index)]
        );
    }
}

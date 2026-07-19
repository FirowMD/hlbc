use std::collections::{BTreeMap, BTreeSet, VecDeque};

use hlbc::opcodes::Opcode;
use hlbc::types::{Function, Reg};
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum NodeId {
    Entry,
    Block(usize),
    Exit,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum EdgeKind {
    Entry,
    Fallthrough,
    BranchTaken,
    Jump,
    SwitchCase(usize),
    SwitchDefault,
    Exception,
    Return,
    Throw,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Edge {
    pub from: NodeId,
    pub to: NodeId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Serialize)]
pub struct BasicBlock {
    pub id: usize,
    pub start: usize,
    pub end: usize,
    pub predecessors: BTreeSet<NodeId>,
    pub successors: BTreeSet<NodeId>,
}

impl BasicBlock {
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ExceptionRegion {
    pub trap_opcode: usize,
    pub start: usize,
    pub end: usize,
    pub handler: usize,
    pub exception_register: Reg,
    pub catch_opcodes: Vec<usize>,
    /// Global type-object indices encoded by consecutive `Catch` markers.
    pub catch_globals: Vec<usize>,
    pub end_trap_opcodes: Vec<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FallbackRegion {
    pub nodes: BTreeSet<NodeId>,
    pub entry_nodes: BTreeSet<NodeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct DominatorInfo {
    pub sets: BTreeMap<NodeId, BTreeSet<NodeId>>,
    pub immediate: BTreeMap<NodeId, Option<NodeId>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct NaturalLoop {
    pub header: NodeId,
    pub latch: NodeId,
    pub nodes: BTreeSet<NodeId>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ControlFlowGraph {
    pub function_index: usize,
    pub instruction_count: usize,
    pub blocks: Vec<BasicBlock>,
    pub edges: Vec<Edge>,
    pub exception_regions: Vec<ExceptionRegion>,
    pub unreachable_instructions: Vec<usize>,
    pub irreducible_regions: Vec<FallbackRegion>,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("malformed control flow in function {function_index} at opcode {opcode_index}: {message}")]
pub struct CfgError {
    pub function_index: usize,
    pub opcode_index: usize,
    pub message: String,
}

impl CfgError {
    fn new(function_index: usize, opcode_index: usize, message: impl Into<String>) -> Self {
        Self {
            function_index,
            opcode_index,
            message: message.into(),
        }
    }
}

fn jump_target(
    function_index: usize,
    instruction_count: usize,
    opcode_index: usize,
    offset: i32,
) -> Result<usize, CfgError> {
    let target = opcode_index as i64 + 1 + offset as i64;
    if !(0..=instruction_count as i64).contains(&target) {
        Err(CfgError::new(
            function_index,
            opcode_index,
            format!("jump offset {offset} targets {target}, outside 0..={instruction_count}"),
        ))
    } else {
        Ok(target as usize)
    }
}

fn branch_offset(opcode: &Opcode) -> Option<i32> {
    match opcode {
        Opcode::JTrue { offset, .. }
        | Opcode::JFalse { offset, .. }
        | Opcode::JNull { offset, .. }
        | Opcode::JNotNull { offset, .. }
        | Opcode::JSLt { offset, .. }
        | Opcode::JSGte { offset, .. }
        | Opcode::JSGt { offset, .. }
        | Opcode::JSLte { offset, .. }
        | Opcode::JULt { offset, .. }
        | Opcode::JUGte { offset, .. }
        | Opcode::JNotLt { offset, .. }
        | Opcode::JNotGte { offset, .. }
        | Opcode::JEq { offset, .. }
        | Opcode::JNotEq { offset, .. } => Some(*offset),
        _ => None,
    }
}

fn is_block_terminator(opcode: &Opcode) -> bool {
    branch_offset(opcode).is_some()
        || matches!(
            opcode,
            Opcode::JAlways { .. }
                | Opcode::Switch { .. }
                | Opcode::Ret { .. }
                | Opcode::Throw { .. }
                | Opcode::Rethrow { .. }
        )
}

impl ControlFlowGraph {
    pub fn build(function: &Function) -> Result<Self, CfgError> {
        Self::build_with_index(function, function.findex.0)
    }

    pub fn build_with_index(function: &Function, function_index: usize) -> Result<Self, CfgError> {
        let len = function.ops.len();
        let mut boundaries = BTreeSet::from([0, len]);
        let mut exception_regions = Vec::new();

        for (index, opcode) in function.ops.iter().enumerate() {
            if is_block_terminator(opcode)
                || matches!(opcode, Opcode::Trap { .. } | Opcode::EndTrap { .. })
            {
                boundaries.insert(index + 1);
            }
            if let Some(offset) = branch_offset(opcode) {
                boundaries.insert(jump_target(function_index, len, index, offset)?);
            }
            match opcode {
                Opcode::JAlways { offset } => {
                    boundaries.insert(jump_target(function_index, len, index, *offset)?);
                }
                Opcode::Switch { offsets, end, .. } => {
                    for offset in offsets {
                        boundaries.insert(jump_target(function_index, len, index, *offset)?);
                    }
                    boundaries.insert(jump_target(function_index, len, index, *end)?);
                }
                Opcode::Trap { exc, offset } => {
                    // Keep operations before the trap outside the protected
                    // region, even when the bytecode has no preceding label.
                    boundaries.insert(index);
                    let handler = jump_target(function_index, len, index, *offset)?;
                    boundaries.insert(handler);
                    let mut start = index + 1;
                    let mut catch_opcodes = Vec::new();
                    let mut catch_globals = Vec::new();
                    while let Some(Opcode::Catch { offset }) = function.ops.get(start) {
                        catch_opcodes.push(start);
                        if *offset >= 0 {
                            catch_globals.push(*offset as usize);
                        }
                        boundaries.insert(start);
                        boundaries.insert(start + 1);
                        start += 1;
                    }
                    if handler < start {
                        return Err(CfgError::new(
                            function_index,
                            index,
                            format!("trap handler {handler} precedes protected region {start}"),
                        ));
                    }
                    exception_regions.push(ExceptionRegion {
                        trap_opcode: index,
                        start,
                        end: handler,
                        handler,
                        exception_register: *exc,
                        catch_opcodes,
                        catch_globals,
                        end_trap_opcodes: Vec::new(),
                    });
                }
                Opcode::EndTrap { .. } => {
                    boundaries.insert(index);
                }
                Opcode::Catch { .. } => {
                    boundaries.insert(index);
                    boundaries.insert(index + 1);
                }
                _ => {}
            }
        }
        for region in &mut exception_regions {
            region.end_trap_opcodes = (region.start..region.end)
                .filter(|&index| matches!(function.ops.get(index), Some(Opcode::EndTrap { .. })))
                .collect();
        }
        exception_regions.sort_by_key(|region| (region.start, region.end, region.handler));

        let points: Vec<_> = boundaries.into_iter().collect();
        let mut blocks = Vec::new();
        let mut instruction_owner = vec![None; len];
        for range in points.windows(2) {
            let (start, end) = (range[0], range[1]);
            if start == end {
                continue;
            }
            let id = blocks.len();
            for owner in &mut instruction_owner[start..end] {
                *owner = Some(id);
            }
            blocks.push(BasicBlock {
                id,
                start,
                end,
                predecessors: BTreeSet::new(),
                successors: BTreeSet::new(),
            });
        }

        let node_for_target = |target: usize| -> Result<NodeId, CfgError> {
            if target == len {
                Ok(NodeId::Exit)
            } else {
                instruction_owner
                    .get(target)
                    .and_then(|owner| *owner)
                    .map(NodeId::Block)
                    .ok_or_else(|| {
                        CfgError::new(
                            function_index,
                            target.min(len.saturating_sub(1)),
                            format!("target {target} is not owned by a basic block"),
                        )
                    })
            }
        };

        let mut edges = BTreeSet::new();
        if let Some(first) = blocks.first() {
            edges.insert(Edge {
                from: NodeId::Entry,
                to: NodeId::Block(first.id),
                kind: EdgeKind::Entry,
            });
        } else {
            edges.insert(Edge {
                from: NodeId::Entry,
                to: NodeId::Exit,
                kind: EdgeKind::Entry,
            });
        }

        for block in &blocks {
            let from = NodeId::Block(block.id);
            let opcode_index = block.end - 1;
            let opcode = &function.ops[opcode_index];
            let mut add = |to, kind| {
                edges.insert(Edge { from, to, kind });
            };
            if let Some(offset) = branch_offset(opcode) {
                add(
                    node_for_target(jump_target(function_index, len, opcode_index, offset)?)?,
                    EdgeKind::BranchTaken,
                );
                add(node_for_target(block.end)?, EdgeKind::Fallthrough);
            } else {
                match opcode {
                    Opcode::JAlways { offset } => add(
                        node_for_target(jump_target(function_index, len, opcode_index, *offset)?)?,
                        EdgeKind::Jump,
                    ),
                    Opcode::Switch { offsets, end, .. } => {
                        for (case, offset) in offsets.iter().enumerate() {
                            add(
                                node_for_target(jump_target(
                                    function_index,
                                    len,
                                    opcode_index,
                                    *offset,
                                )?)?,
                                EdgeKind::SwitchCase(case),
                            );
                        }
                        add(
                            node_for_target(jump_target(function_index, len, opcode_index, *end)?)?,
                            EdgeKind::SwitchDefault,
                        );
                    }
                    Opcode::Ret { .. } => add(NodeId::Exit, EdgeKind::Return),
                    Opcode::Throw { .. } | Opcode::Rethrow { .. } => {}
                    _ => add(node_for_target(block.end)?, EdgeKind::Fallthrough),
                }
            }
        }

        add_exception_edges(
            function,
            function_index,
            &blocks,
            &exception_regions,
            &mut edges,
        )?;

        let edges: Vec<_> = edges.into_iter().collect();
        for edge in &edges {
            if let NodeId::Block(id) = edge.from {
                blocks[id].successors.insert(edge.to);
            }
            if let NodeId::Block(id) = edge.to {
                blocks[id].predecessors.insert(edge.from);
            }
        }

        let reachable = reachable_nodes(&edges, NodeId::Entry);
        let unreachable_instructions = blocks
            .iter()
            .filter(|block| !reachable.contains(&NodeId::Block(block.id)))
            .flat_map(|block| block.range())
            .collect();

        let mut graph = Self {
            function_index,
            instruction_count: len,
            blocks,
            edges,
            exception_regions,
            unreachable_instructions,
            irreducible_regions: Vec::new(),
        };
        graph.verify()?;
        graph.verify_with_function(function)?;
        graph.irreducible_regions = graph.detect_irreducible();
        Ok(graph)
    }

    pub fn verify(&self) -> Result<(), CfgError> {
        let entry_edges: Vec<_> = self
            .edges
            .iter()
            .filter(|edge| edge.from == NodeId::Entry)
            .collect();
        if entry_edges.len() != 1 || entry_edges[0].kind != EdgeKind::Entry {
            return Err(CfgError::new(
                self.function_index,
                0,
                "synthetic entry must have exactly one Entry edge",
            ));
        }
        if self
            .edges
            .iter()
            .any(|edge| edge.to == NodeId::Entry || edge.from == NodeId::Exit)
        {
            return Err(CfgError::new(
                self.function_index,
                0,
                "synthetic entry has an incoming edge or synthetic exit has an outgoing edge",
            ));
        }

        let mut owners = vec![None; self.instruction_count];
        for (expected_id, block) in self.blocks.iter().enumerate() {
            if block.id != expected_id
                || block.start >= block.end
                || block.end > self.instruction_count
            {
                return Err(CfgError::new(
                    self.function_index,
                    block.start.min(self.instruction_count.saturating_sub(1)),
                    format!(
                        "invalid block {} range {}..{}",
                        block.id, block.start, block.end
                    ),
                ));
            }
            for index in block.range() {
                if owners[index].replace(block.id).is_some() {
                    return Err(CfgError::new(
                        self.function_index,
                        index,
                        "instruction belongs to more than one block",
                    ));
                }
            }
        }
        if let Some(index) = owners.iter().position(Option::is_none) {
            return Err(CfgError::new(
                self.function_index,
                index,
                "instruction has no owning block",
            ));
        }

        for edge in &self.edges {
            for node in [edge.from, edge.to] {
                if let NodeId::Block(id) = node {
                    if id >= self.blocks.len() {
                        return Err(CfgError::new(
                            self.function_index,
                            0,
                            format!("edge references missing block {id}"),
                        ));
                    }
                }
            }
            if let NodeId::Block(id) = edge.from {
                if !self.blocks[id].successors.contains(&edge.to) {
                    return Err(CfgError::new(
                        self.function_index,
                        self.blocks[id].end - 1,
                        "successor edge is not symmetric",
                    ));
                }
            }
            if let NodeId::Block(id) = edge.to {
                if !self.blocks[id].predecessors.contains(&edge.from) {
                    return Err(CfgError::new(
                        self.function_index,
                        self.blocks[id].start,
                        "predecessor edge is not symmetric",
                    ));
                }
            }
        }
        for block in &self.blocks {
            for successor in &block.successors {
                if !self
                    .edges
                    .iter()
                    .any(|edge| edge.from == NodeId::Block(block.id) && edge.to == *successor)
                {
                    return Err(CfgError::new(
                        self.function_index,
                        block.end - 1,
                        "successor set has no corresponding edge",
                    ));
                }
            }
            for predecessor in &block.predecessors {
                if !self
                    .edges
                    .iter()
                    .any(|edge| edge.from == *predecessor && edge.to == NodeId::Block(block.id))
                {
                    return Err(CfgError::new(
                        self.function_index,
                        block.start,
                        "predecessor set has no corresponding edge",
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn verify_with_function(&self, function: &Function) -> Result<(), CfgError> {
        self.verify()?;
        if function.ops.len() != self.instruction_count {
            return Err(CfgError::new(
                self.function_index,
                0,
                format!(
                    "CFG owns {} instructions but function has {}",
                    self.instruction_count,
                    function.ops.len()
                ),
            ));
        }

        let node_for_target = |target: usize| -> Result<NodeId, CfgError> {
            if target == self.instruction_count {
                return Ok(NodeId::Exit);
            }
            self.blocks
                .iter()
                .find(|block| block.start == target)
                .map(|block| NodeId::Block(block.id))
                .ok_or_else(|| {
                    CfgError::new(
                        self.function_index,
                        target.min(self.instruction_count.saturating_sub(1)),
                        format!("target {target} is not owned by a basic block"),
                    )
                })
        };

        for block in &self.blocks {
            let opcode_index = block.end - 1;
            let opcode = &function.ops[opcode_index];
            let from = NodeId::Block(block.id);
            let mut expected = BTreeSet::new();
            let mut add_target = |offset: i32, kind: EdgeKind| -> Result<(), CfgError> {
                let target = jump_target(
                    self.function_index,
                    self.instruction_count,
                    opcode_index,
                    offset,
                )?;
                expected.insert((node_for_target(target)?, kind));
                Ok(())
            };

            if let Some(offset) = branch_offset(opcode) {
                add_target(offset, EdgeKind::BranchTaken)?;
                expected.insert((node_for_target(block.end)?, EdgeKind::Fallthrough));
            } else {
                match opcode {
                    Opcode::JAlways { offset } => add_target(*offset, EdgeKind::Jump)?,
                    Opcode::Switch { offsets, end, .. } => {
                        for (case, offset) in offsets.iter().enumerate() {
                            add_target(*offset, EdgeKind::SwitchCase(case))?;
                        }
                        add_target(*end, EdgeKind::SwitchDefault)?;
                    }
                    Opcode::Ret { .. } => {
                        expected.insert((NodeId::Exit, EdgeKind::Return));
                    }
                    Opcode::Throw { .. } | Opcode::Rethrow { .. } => {
                        if !self
                            .edges
                            .iter()
                            .any(|edge| edge.from == from && edge.kind == EdgeKind::Exception)
                        {
                            expected.insert((NodeId::Exit, EdgeKind::Throw));
                        }
                    }
                    _ => {
                        expected.insert((node_for_target(block.end)?, EdgeKind::Fallthrough));
                    }
                }
            }

            let actual: BTreeSet<_> = self
                .edges
                .iter()
                .filter(|edge| edge.from == from && edge.kind != EdgeKind::Exception)
                .map(|edge| (edge.to, edge.kind.clone()))
                .collect();
            if actual != expected {
                return Err(CfgError::new(
                    self.function_index,
                    opcode_index,
                    format!(
                        "terminator {} has edges {actual:?}, expected {expected:?}",
                        opcode.name()
                    ),
                ));
            }
        }
        Ok(())
    }

    pub fn reachable(&self) -> BTreeSet<NodeId> {
        reachable_nodes(&self.edges, NodeId::Entry)
    }

    pub fn dominators(&self) -> DominatorInfo {
        compute_dominators(&self.nodes(), &self.edges, NodeId::Entry, false)
    }

    pub fn post_dominators(&self) -> DominatorInfo {
        compute_dominators(&self.nodes(), &self.edges, NodeId::Exit, true)
    }

    /// Post-dominance for normal completion. Exceptional transfers are
    /// structured by their enclosing try region instead of distorting joins
    /// inside the protected body.
    pub fn normal_post_dominators(&self) -> DominatorInfo {
        let edges: Vec<_> = self
            .edges
            .iter()
            .filter(|edge| edge.kind != EdgeKind::Exception)
            .cloned()
            .collect();
        compute_dominators(&self.nodes(), &edges, NodeId::Exit, true)
    }

    pub fn strongly_connected_components(&self) -> Vec<BTreeSet<NodeId>> {
        strongly_connected_components(&self.nodes(), &self.edges)
    }

    pub fn back_edges(&self) -> Vec<(NodeId, NodeId)> {
        let dominators = self.dominators();
        self.edges
            .iter()
            .filter(|edge| {
                dominators
                    .sets
                    .get(&edge.from)
                    .is_some_and(|set| set.contains(&edge.to))
            })
            .map(|edge| (edge.from, edge.to))
            .collect()
    }

    pub fn natural_loops(&self) -> Vec<NaturalLoop> {
        let predecessors = adjacency(&self.edges, true);
        self.back_edges()
            .into_iter()
            .map(|(latch, header)| {
                let mut nodes = BTreeSet::from([header, latch]);
                let mut stack = vec![latch];
                while let Some(node) = stack.pop() {
                    if node == header {
                        continue;
                    }
                    for predecessor in predecessors.get(&node).into_iter().flatten() {
                        if nodes.insert(*predecessor) {
                            stack.push(*predecessor);
                        }
                    }
                }
                NaturalLoop {
                    header,
                    latch,
                    nodes,
                }
            })
            .collect()
    }

    fn nodes(&self) -> BTreeSet<NodeId> {
        std::iter::once(NodeId::Entry)
            .chain(self.blocks.iter().map(|block| NodeId::Block(block.id)))
            .chain(std::iter::once(NodeId::Exit))
            .collect()
    }

    fn detect_irreducible(&self) -> Vec<FallbackRegion> {
        self.strongly_connected_components()
            .into_iter()
            .filter_map(|nodes| {
                let cyclic = nodes.len() > 1
                    || self
                        .edges
                        .iter()
                        .any(|edge| edge.from == edge.to && nodes.contains(&edge.from));
                if !cyclic {
                    return None;
                }
                let entry_nodes: BTreeSet<_> = self
                    .edges
                    .iter()
                    .filter(|edge| !nodes.contains(&edge.from) && nodes.contains(&edge.to))
                    .map(|edge| edge.to)
                    .collect();
                (entry_nodes.len() > 1).then_some(FallbackRegion { nodes, entry_nodes })
            })
            .collect()
    }
}

fn add_exception_edges(
    function: &Function,
    function_index: usize,
    blocks: &[BasicBlock],
    regions: &[ExceptionRegion],
    edges: &mut BTreeSet<Edge>,
) -> Result<(), CfgError> {
    let trap_regions: BTreeMap<_, _> = regions
        .iter()
        .enumerate()
        .map(|(index, region)| (region.trap_opcode, index))
        .collect();
    let handler_nodes: Vec<_> = regions
        .iter()
        .map(|region| {
            blocks
                .iter()
                .find(|block| block.start == region.handler)
                .map(|block| NodeId::Block(block.id))
                .unwrap_or(NodeId::Exit)
        })
        .collect();
    let normal_successors: BTreeMap<NodeId, Vec<NodeId>> =
        edges.iter().fold(BTreeMap::new(), |mut successors, edge| {
            if matches!(
                edge.kind,
                EdgeKind::Fallthrough
                    | EdgeKind::BranchTaken
                    | EdgeKind::Jump
                    | EdgeKind::SwitchCase(_)
                    | EdgeKind::SwitchDefault
            ) {
                successors.entry(edge.from).or_default().push(edge.to);
            }
            successors
        });

    let mut inputs: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    let mut queue = VecDeque::new();
    if let Some(first) = blocks.first() {
        inputs.insert(first.id, Vec::new());
        queue.push_back(first.id);
    }

    let enqueue = |node: NodeId,
                   stack: Vec<usize>,
                   inputs: &mut BTreeMap<usize, Vec<usize>>,
                   queue: &mut VecDeque<usize>|
     -> Result<(), CfgError> {
        let NodeId::Block(id) = node else {
            return Ok(());
        };
        match inputs.get(&id) {
            Some(existing) if existing != &stack => Err(CfgError::new(
                function_index,
                blocks[id].start,
                format!("inconsistent trap stacks at block {id}: {existing:?} and {stack:?}"),
            )),
            Some(_) => Ok(()),
            None => {
                inputs.insert(id, stack);
                queue.push_back(id);
                Ok(())
            }
        }
    };

    loop {
        while let Some(block_id) = queue.pop_front() {
            let block = &blocks[block_id];
            let mut stack = inputs[&block_id].clone();
            for opcode_index in block.range() {
                let opcode = &function.ops[opcode_index];
                if !opcode.metadata().semantics.exceptions.is_empty() {
                    if let Some(&region_id) = stack.last() {
                        edges.insert(Edge {
                            from: NodeId::Block(block_id),
                            to: handler_nodes[region_id],
                            kind: EdgeKind::Exception,
                        });
                        let mut handler_stack = stack.clone();
                        handler_stack.pop();
                        enqueue(
                            handler_nodes[region_id],
                            handler_stack,
                            &mut inputs,
                            &mut queue,
                        )?;
                    } else if matches!(opcode, Opcode::Throw { .. } | Opcode::Rethrow { .. }) {
                        edges.insert(Edge {
                            from: NodeId::Block(block_id),
                            to: NodeId::Exit,
                            kind: EdgeKind::Throw,
                        });
                    }
                }
                match opcode {
                    Opcode::Trap { .. } => {
                        let region_id =
                            trap_regions.get(&opcode_index).copied().ok_or_else(|| {
                                CfgError::new(
                                    function_index,
                                    opcode_index,
                                    "Trap has no exception-region metadata",
                                )
                            })?;
                        stack.push(region_id);
                    }
                    Opcode::EndTrap { .. } => {
                        // HashLink's EndTrap operand is ignored by the VM/JIT. A
                        // valid marker pops the live trap; tolerating an empty
                        // stack also keeps detached recovery blocks representable.
                        stack.pop();
                    }
                    _ => {}
                }
            }
            for &successor in normal_successors
                .get(&NodeId::Block(block_id))
                .into_iter()
                .flatten()
            {
                enqueue(successor, stack.clone(), &mut inputs, &mut queue)?;
            }
        }

        let Some(block) = blocks.iter().find(|block| !inputs.contains_key(&block.id)) else {
            break;
        };
        // Disconnected bytecode has no executable incoming trap state. Seed it
        // independently so explicit throws still retain their exit edge.
        inputs.insert(block.id, Vec::new());
        queue.push_back(block.id);
    }
    Ok(())
}

fn adjacency(edges: &[Edge], reverse: bool) -> BTreeMap<NodeId, BTreeSet<NodeId>> {
    let mut result: BTreeMap<NodeId, BTreeSet<NodeId>> = BTreeMap::new();
    for edge in edges {
        let (from, to) = if reverse {
            (edge.to, edge.from)
        } else {
            (edge.from, edge.to)
        };
        result.entry(from).or_default().insert(to);
    }
    result
}

fn reachable_nodes(edges: &[Edge], start: NodeId) -> BTreeSet<NodeId> {
    let successors = adjacency(edges, false);
    let mut reachable = BTreeSet::from([start]);
    let mut queue = VecDeque::from([start]);
    while let Some(node) = queue.pop_front() {
        for successor in successors.get(&node).into_iter().flatten() {
            if reachable.insert(*successor) {
                queue.push_back(*successor);
            }
        }
    }
    reachable
}

fn compute_dominators(
    nodes: &BTreeSet<NodeId>,
    edges: &[Edge],
    root: NodeId,
    reverse: bool,
) -> DominatorInfo {
    let predecessors = adjacency(edges, !reverse);
    let reachable = {
        let oriented_edges: Vec<_> = if reverse {
            edges
                .iter()
                .map(|edge| Edge {
                    from: edge.to,
                    to: edge.from,
                    kind: edge.kind.clone(),
                })
                .collect()
        } else {
            edges.to_vec()
        };
        reachable_nodes(&oriented_edges, root)
    };
    let mut sets = BTreeMap::new();
    for node in nodes {
        if *node == root || !reachable.contains(node) {
            sets.insert(*node, BTreeSet::from([*node]));
        } else {
            sets.insert(*node, reachable.clone());
        }
    }

    loop {
        let mut changed = false;
        for node in reachable.iter().copied().filter(|node| *node != root) {
            let incoming = predecessors.get(&node).cloned().unwrap_or_default();
            let mut incoming = incoming.into_iter().filter(|n| reachable.contains(n));
            let mut next = if let Some(first) = incoming.next() {
                sets[&first].clone()
            } else {
                BTreeSet::new()
            };
            for predecessor in incoming {
                next = next.intersection(&sets[&predecessor]).copied().collect();
            }
            next.insert(node);
            if next != sets[&node] {
                sets.insert(node, next);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let mut immediate = BTreeMap::new();
    for node in nodes {
        if *node == root || !reachable.contains(node) {
            immediate.insert(*node, None);
            continue;
        }
        let strict: Vec<_> = sets[node]
            .iter()
            .copied()
            .filter(|candidate| candidate != node)
            .collect();
        let idom = strict
            .iter()
            .copied()
            .max_by_key(|candidate| sets[candidate].len());
        immediate.insert(*node, idom);
    }
    DominatorInfo { sets, immediate }
}

fn strongly_connected_components(
    nodes: &BTreeSet<NodeId>,
    edges: &[Edge],
) -> Vec<BTreeSet<NodeId>> {
    struct Tarjan {
        next_index: usize,
        indices: BTreeMap<NodeId, usize>,
        lowlinks: BTreeMap<NodeId, usize>,
        stack: Vec<NodeId>,
        on_stack: BTreeSet<NodeId>,
        components: Vec<BTreeSet<NodeId>>,
    }

    fn visit(node: NodeId, successors: &BTreeMap<NodeId, BTreeSet<NodeId>>, state: &mut Tarjan) {
        let index = state.next_index;
        state.next_index += 1;
        state.indices.insert(node, index);
        state.lowlinks.insert(node, index);
        state.stack.push(node);
        state.on_stack.insert(node);

        for successor in successors.get(&node).into_iter().flatten().copied() {
            if !state.indices.contains_key(&successor) {
                visit(successor, successors, state);
                state
                    .lowlinks
                    .insert(node, state.lowlinks[&node].min(state.lowlinks[&successor]));
            } else if state.on_stack.contains(&successor) {
                state
                    .lowlinks
                    .insert(node, state.lowlinks[&node].min(state.indices[&successor]));
            }
        }

        if state.lowlinks[&node] == state.indices[&node] {
            let mut component = BTreeSet::new();
            while let Some(member) = state.stack.pop() {
                state.on_stack.remove(&member);
                component.insert(member);
                if member == node {
                    break;
                }
            }
            state.components.push(component);
        }
    }

    let successors = adjacency(edges, false);
    let mut state = Tarjan {
        next_index: 0,
        indices: BTreeMap::new(),
        lowlinks: BTreeMap::new(),
        stack: Vec::new(),
        on_stack: BTreeSet::new(),
        components: Vec::new(),
    };
    for node in nodes {
        if !state.indices.contains_key(node) {
            visit(*node, &successors, &mut state);
        }
    }
    state
        .components
        .sort_by_key(|component| component.iter().next().copied());
    state.components
}

#[cfg(test)]
mod tests {
    use std::panic::{catch_unwind, AssertUnwindSafe};

    use hlbc::types::{RefFun, RefString, RefType};
    use proptest::prelude::*;

    use super::*;

    fn function(ops: Vec<Opcode>) -> Function {
        Function {
            t: RefType(0),
            findex: RefFun(7),
            regs: vec![RefType(0); 4],
            ops,
            debug_info: None,
            assigns: None,
            name: RefString(0),
            parent: None,
        }
    }

    fn block_at(graph: &ControlFlowGraph, start: usize) -> NodeId {
        NodeId::Block(
            graph
                .blocks
                .iter()
                .find(|block| block.start == start)
                .expect("test block")
                .id,
        )
    }

    #[test]
    fn diamond_dominators() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::JFalse {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::Nop,
            Opcode::JAlways { offset: 1 },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let dominators = graph.dominators();
        assert_eq!(
            dominators.immediate[&block_at(&graph, 4)],
            Some(block_at(&graph, 0))
        );
    }

    #[test]
    fn loop_and_disconnected_reachability() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Label,
            Opcode::JFalse {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::Nop,
            Opcode::JAlways { offset: -3 },
            Opcode::Ret { ret: Reg(0) },
            Opcode::Nop,
        ]))
        .unwrap();
        assert_eq!(graph.back_edges().len(), 1);
        assert_eq!(graph.natural_loops().len(), 1);
        assert_eq!(graph.unreachable_instructions, vec![5]);
        graph.verify().unwrap();
    }

    #[test]
    fn post_dominators_use_synthetic_exit_for_returns_and_throws() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::JFalse {
                cond: Reg(0),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(0) },
            Opcode::Throw { exc: Reg(1) },
        ]))
        .unwrap();
        let post = graph.post_dominators();
        assert_eq!(post.immediate[&block_at(&graph, 1)], Some(NodeId::Exit));
        assert_eq!(post.immediate[&block_at(&graph, 2)], Some(NodeId::Exit));
        assert_eq!(post.immediate[&block_at(&graph, 0)], Some(NodeId::Exit));
    }

    #[test]
    fn trap_region_has_exception_edges() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 3,
            },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::JAlways { offset: 1 },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        assert_eq!(graph.exception_regions.len(), 1);
        assert_eq!(graph.exception_regions[0].end_trap_opcodes, vec![2]);
        assert!(graph.edges.iter().any(|edge| {
            edge.from == block_at(&graph, 1)
                && edge.to == block_at(&graph, 4)
                && edge.kind == EdgeKind::Exception
        }));
    }

    #[test]
    fn trap_starts_a_new_block_after_prior_side_effects() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Nop,
            Opcode::Trap {
                exc: Reg(1),
                offset: 3,
            },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::EndTrap { exc: Reg(2) },
            Opcode::JAlways { offset: 1 },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        assert_ne!(block_at(&graph, 0), block_at(&graph, 1));
        assert!(graph.edges.iter().any(|edge| {
            edge.from == block_at(&graph, 2)
                && edge.to == block_at(&graph, 5)
                && edge.kind == EdgeKind::Exception
        }));
    }

    #[test]
    fn catch_markers_are_owned_by_exception_regions() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 4,
            },
            Opcode::Catch { offset: 7 },
            Opcode::Nop,
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::JAlways { offset: 1 },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        assert_eq!(graph.exception_regions[0].catch_opcodes, vec![1]);
        assert_eq!(graph.exception_regions[0].catch_globals, vec![7]);
        assert_eq!(graph.exception_regions[0].start, 2);
        assert_eq!(graph.exception_regions[0].end_trap_opcodes, vec![3]);
    }

    #[test]
    fn nested_traps_route_to_the_innermost_live_handler() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 8,
            },
            Opcode::Trap {
                exc: Reg(2),
                offset: 3,
            },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::EndTrap { exc: Reg(2) },
            Opcode::JAlways { offset: 2 },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::JAlways { offset: 0 },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::JAlways { offset: 2 },
            Opcode::Nop,
            Opcode::JAlways { offset: 0 },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let inner_throw = block_at(&graph, 2);
        let inner_handler = block_at(&graph, 5);
        let outer_handler = block_at(&graph, 9);
        assert!(graph.edges.iter().any(|edge| {
            edge.from == inner_throw && edge.to == inner_handler && edge.kind == EdgeKind::Exception
        }));
        assert!(!graph.edges.iter().any(|edge| {
            edge.from == inner_throw && edge.to == outer_handler && edge.kind == EdgeKind::Exception
        }));
        assert!(graph.edges.iter().any(|edge| {
            edge.from == inner_handler
                && edge.to == outer_handler
                && edge.kind == EdgeKind::Exception
        }));
    }

    #[test]
    fn end_trap_removes_the_exception_edge_before_the_handler() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 4,
            },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::JAlways { offset: 1 },
            Opcode::Nop,
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        let protected = block_at(&graph, 1);
        let after_end = block_at(&graph, 3);
        let handler = block_at(&graph, 5);
        assert!(graph.edges.iter().any(|edge| {
            edge.from == protected && edge.to == handler && edge.kind == EdgeKind::Exception
        }));
        assert!(!graph.edges.iter().any(|edge| {
            edge.from == after_end && edge.to == handler && edge.kind == EdgeKind::Exception
        }));
    }

    #[test]
    fn nested_loops_are_discovered() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::Label,
            Opcode::Label,
            Opcode::JFalse {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::Nop,
            Opcode::JAlways { offset: -3 },
            Opcode::JFalse {
                cond: Reg(1),
                offset: 2,
            },
            Opcode::Nop,
            Opcode::JAlways { offset: -7 },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        assert_eq!(graph.natural_loops().len(), 2);
    }

    #[test]
    fn irreducible_graph_has_a_usable_fallback_region() {
        let graph = ControlFlowGraph::build(&function(vec![
            Opcode::JFalse {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::JAlways { offset: 2 },
            Opcode::Nop,
            Opcode::JAlways { offset: 0 },
            Opcode::JFalse {
                cond: Reg(1),
                offset: -2,
            },
            Opcode::Ret { ret: Reg(0) },
        ]))
        .unwrap();
        assert_eq!(graph.irreducible_regions.len(), 1);
        assert_eq!(graph.irreducible_regions[0].entry_nodes.len(), 2);
        assert!(!graph.irreducible_regions[0].nodes.is_empty());
    }

    proptest! {
        #[test]
        fn malformed_random_control_flow_never_panics(
            offsets in prop::collection::vec(any::<i16>(), 0..64)
        ) {
            let ops = offsets
                .into_iter()
                .enumerate()
                .map(|(index, offset)| match index % 5 {
                    0 => Opcode::JAlways { offset: offset as i32 },
                    1 => Opcode::JFalse { cond: Reg(0), offset: offset as i32 },
                    2 => Opcode::Switch { reg: Reg(0), offsets: vec![offset as i32], end: 0 },
                    3 => Opcode::Nop,
                    _ => Opcode::Ret { ret: Reg(0) },
                })
                .collect();
            let result = catch_unwind(AssertUnwindSafe(|| ControlFlowGraph::build(&function(ops))));
            prop_assert!(result.is_ok());
            if let Ok(Ok(graph)) = result {
                prop_assert!(graph.verify().is_ok());
                let first = serde_json::to_string(&graph).unwrap();
                let second = serde_json::to_string(&graph).unwrap();
                prop_assert_eq!(first, second);
            }
        }
    }
}

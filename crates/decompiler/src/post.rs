use std::collections::{BTreeMap, HashMap, HashSet};

use hlbc::types::{Function, RefField, RefType, Reg, Type};
use hlbc::{Bytecode, Str};

use crate::ast::{
    ConstructorCall, Expr, Operation, RuntimeCheck, StateTerminator, Statement, StringPart,
};
use crate::call_fun;
use crate::fmt::escape_identifier;
use crate::ir::{IrType, TypedIr};
use crate::OptimizationProfile;

pub(crate) trait AstVisitor {
    fn visit_stmt(&mut self, _code: &Bytecode, _stmt: &mut Statement) {}
    fn visit_expr(&mut self, _code: &Bytecode, _expr: &mut Expr) {}
}

#[derive(Debug, Clone)]
pub(crate) enum ObjectRecoveryCandidate {
    Constructor {
        ty: RefType,
        arguments: Vec<Expr>,
    },
    Anonymous {
        ty: RefType,
        fields: BTreeMap<RefField, Expr>,
    },
}

/// Finalize a constructor or anonymous allocation after its ordered writes/call are complete.
pub(crate) fn recover_constructor_and_anonymous_object(candidate: ObjectRecoveryCandidate) -> Expr {
    match candidate {
        ObjectRecoveryCandidate::Constructor { ty, arguments } => {
            Expr::Constructor(ConstructorCall::new(ty, arguments))
        }
        ObjectRecoveryCandidate::Anonymous { ty, fields } => Expr::Anonymous(ty, fields),
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayLiteralCandidate {
    pub elements: Vec<Expr>,
    pub element_type: Option<RefType>,
    pub native: bool,
}

/// Finalize an array literal after the builder proves its writes are ordered and unaliased.
pub(crate) fn recover_array_literal(candidate: ArrayLiteralCandidate) -> Expr {
    Expr::ArrayLiteral {
        elements: candidate.elements,
        element_type: candidate.element_type,
        native: candidate.native,
    }
}

/// Recover string operators, conversions, interpolation, and source trace calls.
pub(crate) fn recover_strings_and_trace(code: &Bytecode, statements: &mut [Statement]) {
    visit(
        code,
        statements,
        &mut [
            Box::new(IfExpressions),
            Box::new(StringConcat),
            Box::new(Itos),
            Box::new(StringInterpolation),
            Box::new(Trace),
        ],
    );
}

fn recover_final_strings_and_trace(code: &Bytecode, statements: &mut [Statement]) {
    visit(
        code,
        statements,
        &mut [
            Box::new(StringConcat),
            Box::new(Itos),
            Box::new(StringInterpolation),
            Box::new(Trace),
        ],
    );
}

/// Recover source-only Haxe constructs after verified CFG structuring.
pub(crate) fn recover_haxe(
    code: &Bytecode,
    function: &Function,
    ir: &TypedIr,
    statements: &mut Vec<Statement>,
    profile: OptimizationProfile,
) {
    recover_final_strings_and_trace(code, statements);
    recover_loops(statements);
    recover_guarded_array_reads(statements);
    recover_map_literals(code, statements);
    recover_enum_pattern_variables(statements);
    if function.name(code).as_str() == "__constructor__" {
        move_super_constructor_first(statements);
    }
    rename_overlapping_ssa_locals(statements, ir);
    rename_state_machine_name_conflicts(statements);
    reconcile_state_machine_locals(statements);
    recover_declaration_scopes(statements, function, ir);
    prevent_conflicting_redeclarations(code, function, statements);
    if profile == OptimizationProfile::Readability {
        remove_trace_metadata(code, statements);
    }
}

fn remove_trace_metadata(code: &Bytecode, statements: &mut Vec<Statement>) {
    for statement in statements.iter_mut() {
        match raw_statement_mut(statement) {
            Statement::IfElse { if_, else_, .. } => {
                remove_trace_metadata(code, if_);
                remove_trace_metadata(code, else_);
            }
            Statement::Switch { default, cases, .. } => {
                remove_trace_metadata(code, default);
                for (_, body) in cases {
                    remove_trace_metadata(code, body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => remove_trace_metadata(code, stmts),
            Statement::TryCatch { try_stmts, catches } => {
                remove_trace_metadata(code, try_stmts);
                for catch in catches {
                    remove_trace_metadata(code, &mut catch.stmts);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    remove_trace_metadata(code, &mut block.stmts);
                }
            }
            _ => {}
        }
    }

    let mut trace_index = 0;
    while trace_index < statements.len() {
        if !is_trace_statement(code, &statements[trace_index]) {
            trace_index += 1;
            continue;
        }
        let mut cursor = trace_index;
        let mut metadata_variable = None;
        let mut fields = HashSet::new();
        let mut allocation = None;
        while cursor > 0 {
            match raw_statement(&statements[cursor - 1]) {
                Statement::DynamicFieldStore {
                    object,
                    field,
                    value,
                } => {
                    let Some(field_name) = code.strings.get(field.0).map(|value| value.as_str())
                    else {
                        break;
                    };
                    if !matches!(
                        field_name,
                        "fileName" | "lineNumber" | "className" | "methodName"
                    ) || !matches!(raw_expr(value), Expr::Constant(_) | Expr::Variable(_, _))
                    {
                        break;
                    }
                    if let Some(variable) = &metadata_variable {
                        if !same_variable(variable, object) {
                            break;
                        }
                    } else {
                        metadata_variable = Some(object.clone());
                    }
                    fields.insert(field_name.to_owned());
                    cursor -= 1;
                }
                Statement::Assign {
                    variable, assign, ..
                } if metadata_variable
                    .as_ref()
                    .is_some_and(|metadata| same_variable(metadata, variable))
                    && matches!(raw_expr(assign), Expr::Anonymous(..)) =>
                {
                    allocation = Some(cursor - 1);
                    break;
                }
                Statement::Assign { assign, .. }
                    if matches!(raw_expr(assign), Expr::Constant(_)) =>
                {
                    cursor -= 1;
                }
                _ => break,
            }
        }
        if metadata_variable.is_none() {
            trace_index += 1;
            continue;
        }
        let Some(mut start) = allocation else {
            trace_index += 1;
            continue;
        };
        if fields.len() != 4 {
            trace_index += 1;
            continue;
        }
        if start > 0
            && matches!(
                raw_statement(&statements[start - 1]),
                Statement::RuntimeCheck(RuntimeCheck::Null(_))
            )
        {
            start -= 1;
        }
        statements.drain(start..trace_index);
        if let Some(statement) = statements.get_mut(start) {
            truncate_trace_metadata_argument(statement);
        }
        trace_index = start + 1;
    }
}

fn truncate_trace_metadata_argument(statement: &mut Statement) {
    let Statement::ExprStatement(expression) = raw_statement_mut(statement) else {
        return;
    };
    let Expr::Call(call) = raw_expr_mut(expression) else {
        return;
    };
    call.args.truncate(1);
}

fn is_trace_statement(code: &Bytecode, statement: &Statement) -> bool {
    let Statement::ExprStatement(expression) = raw_statement(statement) else {
        return false;
    };
    let Expr::Call(call) = raw_expr(expression) else {
        return false;
    };
    matches!(
        raw_expr(&call.fun),
        Expr::FunRef(function) if function.name(code).as_str() == "trace"
    )
}

fn rename_state_machine_name_conflicts(statements: &mut [Statement]) {
    for statement in statements {
        let renames = match raw_statement(statement) {
            Statement::StateMachine { locals, blocks, .. } => {
                let mut variables = Vec::new();
                for local in locals {
                    if let Some(variable) = variable_key(local) {
                        variables.push(variable);
                    }
                }
                for block in blocks {
                    collect_assignment_keys(&block.stmts, &mut variables);
                }
                let mut names = HashMap::new();
                let mut renames = HashMap::new();
                for (register, name) in variables {
                    let Some(name) = name else {
                        continue;
                    };
                    let escaped = escape_identifier(name.as_str());
                    match names.get(&escaped).copied() {
                        Some(previous) if previous != register => {
                            renames
                                .entry((register, name.to_string()))
                                .or_insert_with(|| {
                                    Str::from(format!("{escaped}__hl_{}", register.0))
                                });
                        }
                        None => {
                            names.insert(escaped, register);
                        }
                        _ => {}
                    }
                }
                renames
            }
            _ => HashMap::new(),
        };
        if !renames.is_empty() {
            rename_statement_variables(std::slice::from_mut(statement), &renames);
        }
        match raw_statement_mut(statement) {
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    rename_state_machine_name_conflicts(&mut block.stmts);
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                rename_state_machine_name_conflicts(if_);
                rename_state_machine_name_conflicts(else_);
            }
            Statement::Switch { default, cases, .. } => {
                rename_state_machine_name_conflicts(default);
                for (_, body) in cases {
                    rename_state_machine_name_conflicts(body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => rename_state_machine_name_conflicts(stmts),
            Statement::TryCatch { try_stmts, catches } => {
                rename_state_machine_name_conflicts(try_stmts);
                for catch in catches {
                    rename_state_machine_name_conflicts(&mut catch.stmts);
                }
            }
            _ => {}
        }
    }
}

fn collect_assignment_keys(statements: &[Statement], variables: &mut Vec<(Reg, Option<Str>)>) {
    for statement in statements {
        match raw_statement(statement) {
            Statement::Assign { variable, .. } | Statement::VarDecl { variable, .. } => {
                if let Some(variable) = variable_key(variable) {
                    variables.push(variable);
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                collect_assignment_keys(if_, variables);
                collect_assignment_keys(else_, variables);
            }
            Statement::Switch { default, cases, .. } => {
                collect_assignment_keys(default, variables);
                for (_, body) in cases {
                    collect_assignment_keys(body, variables);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => collect_assignment_keys(stmts, variables),
            Statement::TryCatch { try_stmts, catches } => {
                collect_assignment_keys(try_stmts, variables);
                for catch in catches {
                    collect_assignment_keys(&catch.stmts, variables);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    collect_assignment_keys(&block.stmts, variables);
                }
            }
            _ => {}
        }
    }
}

fn move_super_constructor_first(statements: &mut Vec<Statement>) {
    let Some(position) = statements.iter().position(|statement| {
        matches!(
            raw_statement(statement),
            Statement::ExprStatement(expression)
                if matches!(raw_expr(expression), Expr::SuperCall(_))
        )
    }) else {
        return;
    };
    if position > 0 {
        let super_call = statements.remove(position);
        statements.insert(0, super_call);
    }
}

fn raw_statement(statement: &Statement) -> &Statement {
    match statement {
        Statement::Provenanced {
            statement: inner, ..
        } => raw_statement(inner),
        statement => statement,
    }
}

fn raw_statement_mut(statement: &mut Statement) -> &mut Statement {
    match statement {
        Statement::Provenanced {
            statement: inner, ..
        } => raw_statement_mut(inner),
        statement => statement,
    }
}

fn reconcile_state_machine_locals(statements: &mut [Statement]) {
    for statement in statements {
        match raw_statement_mut(statement) {
            Statement::StateMachine { locals, blocks, .. } => {
                let mut assigned = Vec::new();
                let mut assigned_keys = HashSet::new();
                for block in blocks.iter() {
                    collect_assigned_variables(&block.stmts, &mut assigned, &mut assigned_keys);
                }
                let mut existing = HashSet::new();
                locals.retain(|local| {
                    variable_identity(local).map_or(true, |key| existing.insert(key))
                });
                for variable in assigned {
                    if variable_identity(&variable).map_or(false, |key| existing.insert(key)) {
                        locals.push(variable);
                    }
                }
                for block in blocks {
                    reconcile_state_machine_locals(&mut block.stmts);
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                reconcile_state_machine_locals(if_);
                reconcile_state_machine_locals(else_);
            }
            Statement::Switch { default, cases, .. } => {
                reconcile_state_machine_locals(default);
                for (_, body) in cases {
                    reconcile_state_machine_locals(body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => reconcile_state_machine_locals(stmts),
            Statement::TryCatch { try_stmts, catches } => {
                reconcile_state_machine_locals(try_stmts);
                for catch in catches {
                    reconcile_state_machine_locals(&mut catch.stmts);
                }
            }
            _ => {}
        }
    }
}

fn collect_assigned_variables(
    statements: &[Statement],
    assigned: &mut Vec<Expr>,
    assigned_keys: &mut HashSet<String>,
) {
    for statement in statements {
        match raw_statement(statement) {
            Statement::Assign { variable, .. } | Statement::VarDecl { variable, .. } => {
                if variable_identity(variable).map_or(false, |key| assigned_keys.insert(key)) {
                    assigned.push(variable.clone());
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                collect_assigned_variables(if_, assigned, assigned_keys);
                collect_assigned_variables(else_, assigned, assigned_keys);
            }
            Statement::Switch { default, cases, .. } => {
                collect_assigned_variables(default, assigned, assigned_keys);
                for (_, body) in cases {
                    collect_assigned_variables(body, assigned, assigned_keys);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => {
                collect_assigned_variables(stmts, assigned, assigned_keys)
            }
            Statement::TryCatch { try_stmts, catches } => {
                collect_assigned_variables(try_stmts, assigned, assigned_keys);
                for catch in catches {
                    collect_assigned_variables(&catch.stmts, assigned, assigned_keys);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    collect_assigned_variables(&block.stmts, assigned, assigned_keys);
                }
            }
            _ => {}
        }
    }
}

fn variable_identity(expression: &Expr) -> Option<String> {
    match raw_expr(expression) {
        Expr::Variable(_, Some(name)) => Some(escape_identifier(name.as_str())),
        Expr::Variable(register, None) => Some(format!("__hl_r{}", register.0)),
        _ => None,
    }
}

fn prevent_conflicting_redeclarations(
    code: &Bytecode,
    function: &Function,
    statements: &mut Vec<Statement>,
) {
    let skip_receiver =
        usize::from(function.is_method() || function.name(code).as_str() == "__constructor__");
    let mut declared = function
        .ty(code)
        .args
        .iter()
        .skip(skip_receiver)
        .enumerate()
        .filter_map(|(index, _)| {
            function.arg_name(code, index).map(|name| {
                (
                    escape_identifier(name.as_str()),
                    Reg((index + skip_receiver) as u32),
                )
            })
        })
        .collect::<HashMap<_, _>>();
    dedupe_block_declarations(statements, &mut declared);
}

fn dedupe_block_declarations(statements: &mut Vec<Statement>, declared: &mut HashMap<String, Reg>) {
    let mut index = 0;
    while index < statements.len() {
        let mut remove = false;
        let declaration = match raw_statement(&statements[index]) {
            Statement::Assign {
                declaration: true,
                variable,
                ..
            } => variable_key(variable).map(|key| (false, key)),
            Statement::VarDecl { variable, .. } => variable_key(variable).map(|key| (true, key)),
            _ => None,
        };
        if let Some((is_var_decl, (register, name))) = declaration {
            let original = name.as_ref().map(ToString::to_string);
            let identity = original
                .as_deref()
                .map(escape_identifier)
                .unwrap_or_else(|| format!("__hl_r{}", register.0));
            if let Some(previous) = declared.get(&identity).copied() {
                if previous != register {
                    let base = format!("{identity}__hl_{}", register.0);
                    let mut replacement = base.clone();
                    let mut suffix = 2;
                    while declared.contains_key(&replacement) {
                        replacement = format!("{base}_{suffix}");
                        suffix += 1;
                    }
                    let renames = HashMap::from([(
                        (register, original.unwrap_or(identity.clone())),
                        Str::from(replacement.clone()),
                    )]);
                    rename_statement_variables(&mut statements[index..], &renames);
                    declared.insert(replacement, register);
                } else if is_var_decl {
                    let Statement::VarDecl {
                        variable, value, ..
                    } = raw_statement_mut(&mut statements[index])
                    else {
                        unreachable!()
                    };
                    if let Some(value) = value.take() {
                        statements[index] = Statement::Assign {
                            declaration: false,
                            variable: variable.clone(),
                            assign: value,
                        };
                    } else {
                        remove = true;
                    }
                } else if let Statement::Assign { declaration, .. } =
                    raw_statement_mut(&mut statements[index])
                {
                    *declaration = false;
                }
            } else {
                declared.insert(identity, register);
            }
        }

        if remove {
            statements.remove(index);
            continue;
        }
        match raw_statement_mut(&mut statements[index]) {
            Statement::IfElse { if_, else_, .. } => {
                dedupe_block_declarations(if_, &mut HashMap::new());
                dedupe_block_declarations(else_, &mut HashMap::new());
            }
            Statement::Switch { default, cases, .. } => {
                dedupe_block_declarations(default, &mut HashMap::new());
                for (_, body) in cases {
                    dedupe_block_declarations(body, &mut HashMap::new());
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => dedupe_block_declarations(stmts, &mut HashMap::new()),
            Statement::TryCatch { try_stmts, catches } => {
                dedupe_block_declarations(try_stmts, &mut HashMap::new());
                for catch in catches {
                    dedupe_block_declarations(&mut catch.stmts, &mut HashMap::new());
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    dedupe_block_declarations(&mut block.stmts, &mut HashMap::new());
                }
            }
            _ => {}
        }
        index += 1;
    }
}

fn raw_expr(expression: &Expr) -> &Expr {
    match expression {
        Expr::Provenanced {
            expression: inner, ..
        } => raw_expr(inner),
        expression => expression,
    }
}

fn raw_expr_mut(expression: &mut Expr) -> &mut Expr {
    match expression {
        Expr::Provenanced {
            expression: inner, ..
        } => raw_expr_mut(inner),
        expression => expression,
    }
}

fn variable_key(expression: &Expr) -> Option<(Reg, Option<Str>)> {
    match raw_expr(expression) {
        Expr::Variable(register, name) => Some((*register, name.clone())),
        _ => None,
    }
}

fn recover_map_literals(code: &Bytecode, statements: &mut Vec<Statement>) {
    for statement in statements.iter_mut() {
        match raw_statement_mut(statement) {
            Statement::IfElse { if_, else_, .. } => {
                recover_map_literals(code, if_);
                recover_map_literals(code, else_);
            }
            Statement::Switch { default, cases, .. } => {
                recover_map_literals(code, default);
                for (_, body) in cases {
                    recover_map_literals(code, body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => recover_map_literals(code, stmts),
            Statement::TryCatch { try_stmts, catches } => {
                recover_map_literals(code, try_stmts);
                for catch in catches {
                    recover_map_literals(code, &mut catch.stmts);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    recover_map_literals(code, &mut block.stmts);
                }
            }
            _ => {}
        }
    }

    let mut constructor_index = 0;
    while constructor_index < statements.len() {
        let Some(map_variable) = map_constructor_variable(code, &statements[constructor_index])
        else {
            constructor_index += 1;
            continue;
        };

        let mut entries = Vec::new();
        let mut removals = Vec::new();
        let mut consumed_variables = HashSet::new();
        let mut cursor = constructor_index + 1;
        loop {
            let group_start = cursor;
            let mut temporaries = Vec::new();
            while let Some((variable, value)) =
                statements.get(cursor).and_then(temporary_assignment)
            {
                if variable == map_variable {
                    break;
                }
                temporaries.push((cursor, variable, value));
                cursor += 1;
            }

            let Some((key_argument, value_argument)) = statements
                .get(cursor)
                .and_then(|statement| map_set_arguments(statement, &map_variable))
            else {
                break;
            };
            let key_temporary = variable_key(&key_argument).and_then(|key| {
                temporaries
                    .iter()
                    .find(|(_, temporary, _)| *temporary == key)
            });
            let value_temporary = variable_key(&value_argument).and_then(|key| {
                temporaries
                    .iter()
                    .find(|(_, temporary, _)| *temporary == key)
            });

            let mut consumed_indices = Vec::new();
            let key = if let Some((index, variable, value)) = key_temporary {
                consumed_indices.push(*index);
                consumed_variables.insert(variable.clone());
                value.clone()
            } else {
                key_argument
            };
            let value = if let Some((index, variable, value)) = value_temporary {
                consumed_indices.push(*index);
                consumed_variables.insert(variable.clone());
                value.clone()
            } else {
                value_argument
            };

            let preserves_order = match (key_temporary, value_temporary) {
                (Some((key_index, ..)), Some((value_index, ..))) => key_index < value_index,
                (None, Some(_)) => false,
                _ => true,
            };
            let mut entry_variables = Vec::new();
            expression_variables(&key, &mut entry_variables);
            expression_variables(&value, &mut entry_variables);
            let references_map_before_initialization = entry_variables
                .iter()
                .any(|variable| variable == &map_variable);
            consumed_indices.sort_unstable();
            let all_temporaries_consumed = consumed_indices.len() == temporaries.len()
                && consumed_indices.iter().copied().eq(group_start..cursor);
            if !preserves_order || references_map_before_initialization || !all_temporaries_consumed
            {
                break;
            }

            entries.push((key, value));
            removals.extend(consumed_indices);
            removals.push(cursor);
            cursor += 1;
        }

        let temporary_uses_are_exclusive = consumed_variables
            .iter()
            .all(|variable| !variable_used_before_reassignment(&statements[cursor..], variable));
        if entries.is_empty() || !temporary_uses_are_exclusive {
            constructor_index += 1;
            continue;
        }

        if let Some(initializer) = statement_initializer_mut(&mut statements[constructor_index]) {
            *raw_expr_mut(initializer) = Expr::MapLiteral { entries };
        }
        removals.sort_unstable();
        removals.dedup();
        for index in removals.into_iter().rev() {
            statements.remove(index);
        }
        constructor_index += 1;
    }
}

fn variable_used_before_reassignment(
    statements: &[Statement],
    variable: &(Reg, Option<Str>),
) -> bool {
    for statement in statements {
        if matches!(
            raw_statement(statement),
            Statement::Assign { variable: assigned, .. }
                | Statement::VarDecl { variable: assigned, .. }
                if variable_key(assigned).as_ref() == Some(variable)
        ) {
            return false;
        }
        let mut uses = HashMap::new();
        collect_statement_variable_uses(std::slice::from_ref(statement), &mut uses);
        if uses.contains_key(variable) {
            return true;
        }
    }
    false
}

fn map_constructor_variable(code: &Bytecode, statement: &Statement) -> Option<(Reg, Option<Str>)> {
    let (variable, initializer) = statement_initializer(statement)?;
    let Expr::Constructor(call) = raw_expr(initializer) else {
        return None;
    };
    is_haxe_map_type(code, call.ty).then_some(variable)
}

fn is_haxe_map_type(code: &Bytecode, ty: RefType) -> bool {
    let Some(Type::Obj(object) | Type::Struct(object)) = code.types.get(ty.0) else {
        return false;
    };
    let name = object.name(code);
    let name = name.as_str();
    name.starts_with("haxe.ds.")
        && matches!(
            name.rsplit('.').next(),
            Some("StringMap" | "IntMap" | "ObjectMap" | "EnumValueMap")
        )
}

fn statement_initializer(statement: &Statement) -> Option<((Reg, Option<Str>), &Expr)> {
    match raw_statement(statement) {
        Statement::Assign {
            declaration: true,
            variable,
            assign,
        } => variable_key(variable).map(|variable| (variable, assign)),
        Statement::VarDecl {
            variable,
            value: Some(value),
            ..
        } => variable_key(variable).map(|variable| (variable, value)),
        _ => None,
    }
}

fn statement_initializer_mut(statement: &mut Statement) -> Option<&mut Expr> {
    match raw_statement_mut(statement) {
        Statement::Assign {
            declaration: true,
            assign,
            ..
        } => Some(assign),
        Statement::VarDecl {
            value: Some(value), ..
        } => Some(value),
        _ => None,
    }
}

fn temporary_assignment(statement: &Statement) -> Option<((Reg, Option<Str>), Expr)> {
    match raw_statement(statement) {
        Statement::Assign {
            variable, assign, ..
        } => variable_key(variable).map(|variable| (variable, assign.clone())),
        Statement::VarDecl {
            variable,
            value: Some(value),
            ..
        } => variable_key(variable).map(|variable| (variable, value.clone())),
        _ => None,
    }
}

fn map_set_arguments(
    statement: &Statement,
    map_variable: &(Reg, Option<Str>),
) -> Option<(Expr, Expr)> {
    let Statement::ExprStatement(expression) = raw_statement(statement) else {
        return None;
    };
    let Expr::Call(call) = raw_expr(expression) else {
        return None;
    };
    let Expr::Field(receiver, field) = raw_expr(&call.fun) else {
        return None;
    };
    if field != "set"
        || variable_key(receiver) != Some(map_variable.clone())
        || call.args.len() != 2
    {
        return None;
    }
    Some((call.args[0].clone(), call.args[1].clone()))
}

fn same_variable(left: &Expr, right: &Expr) -> bool {
    variable_key(left) == variable_key(right)
}

fn recover_guarded_array_reads(statements: &mut [Statement]) {
    for statement in statements.iter_mut() {
        match raw_statement_mut(statement) {
            Statement::IfElse { if_, else_, .. } => {
                recover_guarded_array_reads(if_);
                recover_guarded_array_reads(else_);
            }
            Statement::Switch { default, cases, .. } => {
                recover_guarded_array_reads(default);
                for (_, body) in cases {
                    recover_guarded_array_reads(body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => recover_guarded_array_reads(stmts),
            Statement::TryCatch { try_stmts, catches } => {
                recover_guarded_array_reads(try_stmts);
                for catch in catches {
                    recover_guarded_array_reads(&mut catch.stmts);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    recover_guarded_array_reads(&mut block.stmts);
                }
            }
            _ => {}
        }
    }

    for statement in statements.iter_mut() {
        let replacement = match raw_statement(statement) {
            Statement::IfElse { if_, else_, .. } if if_.len() == 1 && else_.len() == 1 => {
                let Statement::Assign {
                    declaration: left_declaration,
                    variable: left_variable,
                    assign: left_value,
                } = raw_statement(&if_[0])
                else {
                    continue;
                };
                let Statement::Assign {
                    declaration: right_declaration,
                    variable: right_variable,
                    assign: right_value,
                } = raw_statement(&else_[0])
                else {
                    continue;
                };
                if same_variable(left_variable, right_variable)
                    && matches!(raw_expr(left_value), Expr::Array(..))
                {
                    let assign = match raw_expr(left_value) {
                        Expr::Array(array, index) if same_variable(index, left_variable) => {
                            Expr::Array(array.clone(), Box::new(right_value.clone()))
                        }
                        _ => left_value.clone(),
                    };
                    Some(Statement::Assign {
                        declaration: *left_declaration || *right_declaration,
                        variable: left_variable.clone(),
                        assign,
                    })
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(replacement) = replacement {
            *statement = replacement;
        }
    }
}

fn recover_enum_pattern_variables(statements: &mut [Statement]) {
    for statement in statements {
        match raw_statement_mut(statement) {
            Statement::Switch {
                arg,
                default,
                cases,
            } => {
                recover_enum_pattern_variables(default);
                for (patterns, body) in cases {
                    for pattern in patterns {
                        let Expr::EnumPattern(ty, constructor, arity) = raw_expr(pattern) else {
                            continue;
                        };
                        let (ty, constructor, arity) = (*ty, *constructor, *arity);
                        if arity == 0 {
                            continue;
                        }
                        let mut variables = Vec::new();
                        for field_index in 0..arity {
                            let Some(statement) = body.get(field_index) else {
                                break;
                            };
                            let Statement::Assign {
                                variable, assign, ..
                            } = raw_statement(statement)
                            else {
                                break;
                            };
                            let Expr::EnumField {
                                value,
                                construct,
                                field,
                                ..
                            } = raw_expr(assign)
                            else {
                                break;
                            };
                            if *construct != constructor
                                || field.0 != field_index
                                || !same_variable(value, arg)
                            {
                                break;
                            }
                            variables.push(variable.clone());
                        }
                        if variables.len() == arity {
                            *pattern = Expr::EnumPatternBinding(ty, constructor, variables);
                            body.drain(..arity);
                        }
                    }
                    recover_enum_pattern_variables(body);
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                recover_enum_pattern_variables(if_);
                recover_enum_pattern_variables(else_);
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => recover_enum_pattern_variables(stmts),
            Statement::TryCatch { try_stmts, catches } => {
                recover_enum_pattern_variables(try_stmts);
                for catch in catches {
                    recover_enum_pattern_variables(&mut catch.stmts);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    recover_enum_pattern_variables(&mut block.stmts);
                }
            }
            _ => {}
        }
    }
}

fn recover_loops(statements: &mut Vec<Statement>) {
    for statement in statements.iter_mut() {
        match raw_statement_mut(statement) {
            Statement::IfElse { if_, else_, .. } => {
                recover_loops(if_);
                recover_loops(else_);
            }
            Statement::Switch { default, cases, .. } => {
                recover_loops(default);
                for (_, body) in cases {
                    recover_loops(body);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => recover_loops(stmts),
            Statement::TryCatch { try_stmts, catches } => {
                recover_loops(try_stmts);
                for catch in catches {
                    recover_loops(&mut catch.stmts);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    recover_loops(&mut block.stmts);
                }
            }
            _ => {}
        }
    }

    let mut index = 0;
    while index < statements.len() {
        if let Some((start, replacement)) = recover_foreach_at(statements, index) {
            statements.splice(start..=index, [replacement]);
            index = start + 1;
            continue;
        }
        if let Some((start, replacement)) = recover_range_at(statements, index) {
            statements.splice(start..=index, [replacement]);
            index = start + 1;
            continue;
        }
        index += 1;
    }
}

fn recover_foreach_at(statements: &[Statement], loop_index: usize) -> Option<(usize, Statement)> {
    let Statement::While { cond, stmts } = raw_statement(statements.get(loop_index)?) else {
        return None;
    };
    if !matches!(
        raw_expr(cond),
        Expr::Constant(crate::ast::Constant::Bool(true))
    ) {
        return None;
    }

    let mut element_variable = None;
    let mut iterable_variable = None;
    let mut index_variable = None;
    let mut extraction_index = None;
    for (position, statement) in stmts.iter().enumerate() {
        let Statement::IfElse { if_, else_, .. } = raw_statement(statement) else {
            continue;
        };
        if if_.len() != 1 || else_.len() != 1 {
            continue;
        }
        let Statement::Assign {
            variable, assign, ..
        } = raw_statement(&if_[0])
        else {
            continue;
        };
        let Statement::Assign {
            variable: fallback_variable,
            ..
        } = raw_statement(&else_[0])
        else {
            continue;
        };
        let Expr::Array(iterable, array_index) = raw_expr(assign) else {
            continue;
        };
        if !same_variable(variable, fallback_variable) {
            continue;
        }
        element_variable = Some(variable.clone());
        iterable_variable = Some(iterable.as_ref().clone());
        index_variable = Some(array_index.as_ref().clone());
        extraction_index = Some(position);
        break;
    }
    let element_variable = element_variable?;
    let iterable_variable = iterable_variable?;
    let index_variable = index_variable?;
    let extraction_index = extraction_index?;

    let increment_index = stmts.iter().position(|statement| {
        matches!(
            raw_statement(statement),
            Statement::ExprStatement(Expr::Op(Operation::Incr(variable)))
                if same_variable(variable, &index_variable)
        )
    })?;
    let mut body = stmts.clone();
    for position in (0..body.len()).rev() {
        let remove = position == extraction_index
            || position == increment_index
            || matches!(
                raw_statement(&body[position]),
                Statement::RuntimeCheck(RuntimeCheck::Null(value))
                    if same_variable(value, &iterable_variable)
            )
            || matches!(
                raw_statement(&body[position]),
                Statement::IfElse { if_, .. }
                    if if_.iter().any(|statement| matches!(raw_statement(statement), Statement::Break))
            );
        if remove {
            body.remove(position);
        }
    }

    let iterable_init = (0..loop_index).rev().find(|position| {
        matches!(
            raw_statement(&statements[*position]),
            Statement::Assign { variable, .. } if same_variable(variable, &iterable_variable)
        )
    })?;
    let index_init = (0..iterable_init).rev().find(|position| {
        matches!(
            raw_statement(&statements[*position]),
            Statement::Assign { variable, .. } if same_variable(variable, &index_variable)
        )
    })?;
    let Statement::Assign {
        assign: iterable, ..
    } = raw_statement(&statements[iterable_init])
    else {
        return None;
    };
    Some((
        index_init,
        Statement::ForEach {
            variable: element_variable,
            iterable: iterable.clone(),
            stmts: body,
        },
    ))
}

fn recover_range_at(statements: &[Statement], loop_index: usize) -> Option<(usize, Statement)> {
    let Statement::While { cond, stmts } = raw_statement(statements.get(loop_index)?) else {
        return None;
    };
    let (counter, end) = match raw_expr(cond) {
        Expr::Op(Operation::Lt(counter, end)) => (counter.as_ref(), end.as_ref()),
        Expr::Op(Operation::Gt(end, counter)) => (counter.as_ref(), end.as_ref()),
        _ => return None,
    };
    let increment = stmts.iter().position(|statement| {
        matches!(
            raw_statement(statement),
            Statement::ExprStatement(Expr::Op(Operation::Incr(candidate)))
                if same_variable(candidate, counter)
        )
    })?;
    let initializer = (0..loop_index).rev().find(|position| {
        matches!(
            raw_statement(&statements[*position]),
            Statement::Assign { variable: candidate, .. } if same_variable(candidate, counter)
        )
    })?;
    let Statement::Assign { assign: start, .. } = raw_statement(&statements[initializer]) else {
        return None;
    };

    let mut splice_start = initializer;
    let recovered_end = if let Some(end_initializer) =
        (initializer + 1..loop_index).rev().find(|position| {
            matches!(
                raw_statement(&statements[*position]),
                Statement::Assign { variable, .. } if same_variable(variable, end)
            )
        }) {
        let Statement::Assign { assign, .. } = raw_statement(&statements[end_initializer]) else {
            return None;
        };
        splice_start = splice_start.min(end_initializer);
        assign.clone()
    } else {
        end.clone()
    };

    let mut body = stmts.clone();
    let loop_variable = body.iter().enumerate().find_map(|(position, statement)| {
        let Statement::Assign {
            variable, assign, ..
        } = raw_statement(statement)
        else {
            return None;
        };
        (matches!(raw_expr(variable), Expr::Variable(..))
            && same_variable(assign, counter)
            && !same_variable(variable, counter))
        .then(|| (position, variable.clone()))
    });
    body.remove(increment);
    let variable = if let Some((position, variable)) = loop_variable {
        let position = if position > increment {
            position - 1
        } else {
            position
        };
        body.remove(position);
        variable
    } else {
        counter.clone()
    };
    Some((
        splice_start,
        Statement::ForRange {
            variable,
            start: start.clone(),
            end: recovered_end,
            stmts: body,
        },
    ))
}

fn rename_overlapping_ssa_locals(statements: &mut [Statement], ir: &TypedIr) {
    let mut groups: BTreeMap<String, Vec<(Reg, usize, usize)>> = BTreeMap::new();
    for local in &ir.locals {
        let Some(name) = &local.debug_name else {
            continue;
        };
        let start = local
            .lifetime
            .opcode_ranges
            .iter()
            .map(|range| range.start)
            .min()
            .unwrap_or(0);
        let end = local
            .lifetime
            .opcode_ranges
            .iter()
            .map(|range| range.end)
            .max()
            .unwrap_or(start);
        let entries = groups.entry(name.clone()).or_default();
        if !entries
            .iter()
            .any(|(register, _, _)| *register == local.register)
        {
            entries.push((local.register, start, end));
        }
    }

    let mut renames = HashMap::new();
    for (name, mut entries) in groups {
        entries.sort_by_key(|(register, start, _)| (*start, register.0));
        let mut accepted: Vec<(Reg, usize, usize)> = Vec::new();
        for (register, start, end) in entries {
            if accepted
                .iter()
                .any(|(_, other_start, other_end)| start < *other_end && *other_start < end)
            {
                renames.insert(
                    (register, name.clone()),
                    Str::from(format!("{name}_r{}", register.0)),
                );
            }
            accepted.push((register, start, end));
        }
    }
    if !renames.is_empty() {
        rename_statement_variables(statements, &renames);
    }
}

fn rename_statement_variables(statements: &mut [Statement], renames: &HashMap<(Reg, String), Str>) {
    // The visitor API needs a bytecode only for semantic visitors. Renaming is
    // performed by the dedicated walker below to avoid manufacturing context.
    fn rename_expr(expression: &mut Expr, renames: &HashMap<(Reg, String), Str>) {
        match expression {
            Expr::Variable(register, Some(name)) => {
                if let Some(replacement) = renames.get(&(*register, name.to_string())) {
                    *name = replacement.clone();
                }
            }
            Expr::Anonymous(_, fields) => {
                for value in fields.values_mut() {
                    rename_expr(value, renames);
                }
            }
            Expr::Array(array, index) => {
                rename_expr(array, renames);
                rename_expr(index, renames);
            }
            Expr::ArrayLiteral { elements, .. } | Expr::StringConcat(elements) => {
                for element in elements {
                    rename_expr(element, renames);
                }
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    rename_expr(key, renames);
                    rename_expr(value, renames);
                }
            }
            Expr::ArrayAlloc { length, .. } | Expr::ToString(length) => {
                rename_expr(length, renames)
            }
            Expr::Call(call) => {
                rename_expr(&mut call.fun, renames);
                for argument in &mut call.args {
                    rename_expr(argument, renames);
                }
            }
            Expr::Constructor(call) => {
                for argument in &mut call.args {
                    rename_expr(argument, renames);
                }
            }
            Expr::Closure(_, _, captures, body) => {
                for (_, value) in captures {
                    rename_expr(value, renames);
                }
                rename_statement_variables(body, renames);
            }
            Expr::EnumConstr(_, _, arguments)
            | Expr::EnumPatternBinding(_, _, arguments)
            | Expr::SuperCall(arguments) => {
                for argument in arguments {
                    rename_expr(argument, renames);
                }
            }
            Expr::SuperMethod { args, .. } => {
                for argument in args {
                    rename_expr(argument, renames);
                }
            }
            Expr::EnumIndex(value)
            | Expr::Field(value, _)
            | Expr::DynamicField(value, _)
            | Expr::RuntimeType { value, .. }
            | Expr::TypeId { value, .. }
            | Expr::SafeCast { value, .. }
            | Expr::VirtualClosure {
                receiver: value, ..
            }
            | Expr::Reference { value, .. }
            | Expr::Dereference {
                reference: value, ..
            }
            | Expr::ReferenceData { array: value, .. } => rename_expr(value, renames),
            Expr::EnumField { value, .. } => rename_expr(value, renames),
            Expr::MemoryLoad { bytes, index, .. } => {
                rename_expr(bytes, renames);
                rename_expr(index, renames);
            }
            Expr::ReferenceOffset {
                reference, offset, ..
            } => {
                rename_expr(reference, renames);
                rename_expr(offset, renames);
            }
            Expr::IfElse { cond, if_, else_ } => {
                rename_expr(cond, renames);
                rename_statement_variables(if_, renames);
                rename_statement_variables(else_, renames);
            }
            Expr::Op(operation) => rename_operation(operation, renames),
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let StringPart::Expression(expression) = part {
                        rename_expr(expression, renames);
                    }
                }
            }
            Expr::Provenanced {
                expression: inner, ..
            } => rename_expr(inner, renames),
            Expr::Bytes(_)
            | Expr::Constant(_)
            | Expr::Capture(_)
            | Expr::EnumPattern(_, _, _)
            | Expr::FunRef(_)
            | Expr::GlobalLoad { .. }
            | Expr::TypeValue { .. }
            | Expr::Unknown(_)
            | Expr::Variable(_, None) => {}
        }
    }
    fn rename_operation(operation: &mut Operation, renames: &HashMap<(Reg, String), Str>) {
        match operation {
            Operation::Add(left, right)
            | Operation::Sub(left, right)
            | Operation::Mul(left, right)
            | Operation::Div(left, right)
            | Operation::Mod(left, right)
            | Operation::Shl(left, right)
            | Operation::Shr(left, right)
            | Operation::And(left, right)
            | Operation::Or(left, right)
            | Operation::BitAnd(left, right)
            | Operation::BitOr(left, right)
            | Operation::Xor(left, right)
            | Operation::Eq(left, right)
            | Operation::NotEq(left, right)
            | Operation::Gt(left, right)
            | Operation::Gte(left, right)
            | Operation::Lt(left, right)
            | Operation::Lte(left, right) => {
                rename_expr(left, renames);
                rename_expr(right, renames);
            }
            Operation::Neg(value)
            | Operation::Not(value)
            | Operation::Incr(value)
            | Operation::Decr(value) => rename_expr(value, renames),
        }
    }
    for statement in statements {
        match raw_statement_mut(statement) {
            Statement::VarDecl {
                variable, value, ..
            } => {
                rename_expr(variable, renames);
                if let Some(value) = value {
                    rename_expr(value, renames);
                }
            }
            Statement::Assign {
                variable, assign, ..
            } => {
                rename_expr(variable, renames);
                rename_expr(assign, renames);
            }
            Statement::ExprStatement(value)
            | Statement::Throw(value)
            | Statement::RuntimeCheck(RuntimeCheck::Null(value)) => rename_expr(value, renames),
            Statement::GlobalStore { value, .. } => rename_expr(value, renames),
            Statement::DynamicFieldStore { object, value, .. } => {
                rename_expr(object, renames);
                rename_expr(value, renames);
            }
            Statement::MemoryStore {
                bytes,
                index,
                value,
                ..
            } => {
                rename_expr(bytes, renames);
                rename_expr(index, renames);
                rename_expr(value, renames);
            }
            Statement::ReferenceStore {
                reference, value, ..
            } => {
                rename_expr(reference, renames);
                rename_expr(value, renames);
            }
            Statement::Prefetch { value, .. } => rename_expr(value, renames),
            Statement::Return(Some(value)) => rename_expr(value, renames),
            Statement::IfElse { cond, if_, else_ } => {
                rename_expr(cond, renames);
                rename_statement_variables(if_, renames);
                rename_statement_variables(else_, renames);
            }
            Statement::Switch {
                arg,
                default,
                cases,
            } => {
                rename_expr(arg, renames);
                rename_statement_variables(default, renames);
                for (patterns, body) in cases {
                    for pattern in patterns {
                        rename_expr(pattern, renames);
                    }
                    rename_statement_variables(body, renames);
                }
            }
            Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
                rename_expr(cond, renames);
                rename_statement_variables(stmts, renames);
            }
            Statement::ForEach {
                variable,
                iterable,
                stmts,
            } => {
                rename_expr(variable, renames);
                rename_expr(iterable, renames);
                rename_statement_variables(stmts, renames);
            }
            Statement::ForRange {
                variable,
                start,
                end,
                stmts,
            } => {
                rename_expr(variable, renames);
                rename_expr(start, renames);
                rename_expr(end, renames);
                rename_statement_variables(stmts, renames);
            }
            Statement::Try { stmts } | Statement::Catch { stmts } => {
                rename_statement_variables(stmts, renames)
            }
            Statement::TryCatch { try_stmts, catches } => {
                rename_statement_variables(try_stmts, renames);
                for catch in catches {
                    rename_expr(&mut catch.variable, renames);
                    rename_statement_variables(&mut catch.stmts, renames);
                }
            }
            Statement::StateMachine { locals, blocks, .. } => {
                for local in locals {
                    rename_expr(local, renames);
                }
                for block in blocks {
                    rename_statement_variables(&mut block.stmts, renames);
                    match &mut block.terminator {
                        StateTerminator::Branch { cond, .. } => rename_expr(cond, renames),
                        StateTerminator::Switch { arg, .. } => rename_expr(arg, renames),
                        StateTerminator::Return(Some(value)) | StateTerminator::Throw(value) => {
                            rename_expr(value, renames)
                        }
                        _ => {}
                    }
                }
            }
            Statement::RuntimeCheck(RuntimeCheck::Assert)
            | Statement::Nop
            | Statement::Return(None)
            | Statement::Break
            | Statement::Continue
            | Statement::Comment(_)
            | Statement::UnhandledOpcode { .. }
            | Statement::Provenanced { .. } => {}
        }
    }
}

fn recover_declaration_scopes(statements: &mut Vec<Statement>, function: &Function, ir: &TypedIr) {
    let mut declaration_keys = HashSet::new();
    collect_declarations(statements, &mut declaration_keys);
    let ssa_registers = ir
        .locals
        .iter()
        .filter(|local| !matches!(local.ty, IrType::Unknown))
        .map(|local| local.register)
        .collect::<HashSet<_>>();
    declaration_keys.retain(|(register, _)| ssa_registers.contains(register));

    let mut hoist = HashSet::new();
    let mut available = HashSet::new();
    find_undeclared_uses(statements, &declaration_keys, &mut available, &mut hoist);
    if hoist.is_empty() {
        return;
    }
    clear_declarations(statements, &hoist);
    let mut declarations = hoist
        .into_iter()
        .map(|(register, name)| {
            let ty = function
                .regs
                .get(register.0 as usize)
                .copied()
                .unwrap_or(RefType(9));
            Statement::VarDecl {
                variable: Expr::Variable(register, name),
                variable_type: ty,
                value: None,
            }
        })
        .collect::<Vec<_>>();
    declarations.sort_by_key(|statement| match statement {
        Statement::VarDecl {
            variable: Expr::Variable(register, _),
            ..
        } => register.0,
        _ => u32::MAX,
    });
    declarations.append(statements);
    *statements = declarations;
}

fn collect_declarations(statements: &[Statement], declarations: &mut HashSet<(Reg, Option<Str>)>) {
    for statement in statements {
        match raw_statement(statement) {
            Statement::Assign {
                declaration: true,
                variable,
                ..
            }
            | Statement::VarDecl { variable, .. } => {
                if let Some(key) = variable_key(variable) {
                    declarations.insert(key);
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                collect_declarations(if_, declarations);
                collect_declarations(else_, declarations);
            }
            Statement::Switch { default, cases, .. } => {
                collect_declarations(default, declarations);
                for (_, body) in cases {
                    collect_declarations(body, declarations);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => collect_declarations(stmts, declarations),
            Statement::TryCatch { try_stmts, catches } => {
                collect_declarations(try_stmts, declarations);
                for catch in catches {
                    collect_declarations(&catch.stmts, declarations);
                }
            }
            Statement::StateMachine { .. } => {}
            _ => {}
        }
    }
}

fn expression_variables(expression: &Expr, variables: &mut Vec<(Reg, Option<Str>)>) {
    match raw_expr(expression) {
        Expr::Variable(register, name) => variables.push((*register, name.clone())),
        Expr::Anonymous(_, fields) => {
            for value in fields.values() {
                expression_variables(value, variables);
            }
        }
        Expr::Array(array, index) => {
            expression_variables(array, variables);
            expression_variables(index, variables);
        }
        Expr::ArrayLiteral { elements, .. } | Expr::StringConcat(elements) => {
            for element in elements {
                expression_variables(element, variables);
            }
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                expression_variables(key, variables);
                expression_variables(value, variables);
            }
        }
        Expr::ArrayAlloc { length, .. } | Expr::ToString(length) => {
            expression_variables(length, variables)
        }
        Expr::Call(call) => {
            expression_variables(&call.fun, variables);
            for argument in &call.args {
                expression_variables(argument, variables);
            }
        }
        Expr::Constructor(call) => {
            for argument in &call.args {
                expression_variables(argument, variables);
            }
        }
        Expr::EnumConstr(_, _, arguments)
        | Expr::EnumPatternBinding(_, _, arguments)
        | Expr::SuperCall(arguments) => {
            for argument in arguments {
                expression_variables(argument, variables);
            }
        }
        Expr::SuperMethod { args, .. } => {
            for argument in args {
                expression_variables(argument, variables);
            }
        }
        Expr::EnumIndex(value)
        | Expr::Field(value, _)
        | Expr::DynamicField(value, _)
        | Expr::EnumField { value, .. }
        | Expr::RuntimeType { value, .. }
        | Expr::TypeId { value, .. }
        | Expr::SafeCast { value, .. }
        | Expr::VirtualClosure {
            receiver: value, ..
        }
        | Expr::Reference { value, .. }
        | Expr::Dereference {
            reference: value, ..
        }
        | Expr::ReferenceData { array: value, .. } => expression_variables(value, variables),
        Expr::MemoryLoad { bytes, index, .. } => {
            expression_variables(bytes, variables);
            expression_variables(index, variables);
        }
        Expr::ReferenceOffset {
            reference, offset, ..
        } => {
            expression_variables(reference, variables);
            expression_variables(offset, variables);
        }
        Expr::IfElse { cond, .. } => expression_variables(cond, variables),
        Expr::Op(operation) => match operation {
            Operation::Add(left, right)
            | Operation::Sub(left, right)
            | Operation::Mul(left, right)
            | Operation::Div(left, right)
            | Operation::Mod(left, right)
            | Operation::Shl(left, right)
            | Operation::Shr(left, right)
            | Operation::And(left, right)
            | Operation::Or(left, right)
            | Operation::BitAnd(left, right)
            | Operation::BitOr(left, right)
            | Operation::Xor(left, right)
            | Operation::Eq(left, right)
            | Operation::NotEq(left, right)
            | Operation::Gt(left, right)
            | Operation::Gte(left, right)
            | Operation::Lt(left, right)
            | Operation::Lte(left, right) => {
                expression_variables(left, variables);
                expression_variables(right, variables);
            }
            Operation::Neg(value)
            | Operation::Not(value)
            | Operation::Incr(value)
            | Operation::Decr(value) => expression_variables(value, variables),
        },
        Expr::StringInterpolation(parts) => {
            for part in parts {
                if let StringPart::Expression(expression) = part {
                    expression_variables(expression, variables);
                }
            }
        }
        Expr::Closure(_, _, _, _)
        | Expr::Capture(_)
        | Expr::Bytes(_)
        | Expr::Constant(_)
        | Expr::EnumPattern(_, _, _)
        | Expr::FunRef(_)
        | Expr::GlobalLoad { .. }
        | Expr::TypeValue { .. }
        | Expr::Unknown(_)
        | Expr::Provenanced { .. } => {}
    }
}

fn collect_statement_variable_uses(
    statements: &[Statement],
    uses: &mut HashMap<(Reg, Option<Str>), usize>,
) {
    fn collect_expression(expression: &Expr, uses: &mut HashMap<(Reg, Option<Str>), usize>) {
        let mut variables = Vec::new();
        expression_variables(expression, &mut variables);
        for variable in variables {
            *uses.entry(variable).or_default() += 1;
        }
    }

    for statement in statements {
        match raw_statement(statement) {
            Statement::VarDecl { value, .. } => {
                if let Some(value) = value {
                    collect_expression(value, uses);
                }
            }
            Statement::Assign {
                variable, assign, ..
            } => {
                if variable_key(variable).is_none() {
                    collect_expression(variable, uses);
                }
                collect_expression(assign, uses);
            }
            Statement::ExprStatement(expression)
            | Statement::Throw(expression)
            | Statement::RuntimeCheck(RuntimeCheck::Null(expression)) => {
                collect_expression(expression, uses)
            }
            Statement::GlobalStore { value, .. } | Statement::Prefetch { value, .. } => {
                collect_expression(value, uses)
            }
            Statement::DynamicFieldStore { object, value, .. } => {
                collect_expression(object, uses);
                collect_expression(value, uses);
            }
            Statement::MemoryStore {
                bytes,
                index,
                value,
                ..
            } => {
                collect_expression(bytes, uses);
                collect_expression(index, uses);
                collect_expression(value, uses);
            }
            Statement::ReferenceStore {
                reference, value, ..
            } => {
                collect_expression(reference, uses);
                collect_expression(value, uses);
            }
            Statement::Return(Some(value)) => collect_expression(value, uses),
            Statement::IfElse { cond, if_, else_ } => {
                collect_expression(cond, uses);
                collect_statement_variable_uses(if_, uses);
                collect_statement_variable_uses(else_, uses);
            }
            Statement::Switch {
                arg,
                default,
                cases,
            } => {
                collect_expression(arg, uses);
                collect_statement_variable_uses(default, uses);
                for (patterns, body) in cases {
                    for pattern in patterns {
                        collect_expression(pattern, uses);
                    }
                    collect_statement_variable_uses(body, uses);
                }
            }
            Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
                collect_expression(cond, uses);
                collect_statement_variable_uses(stmts, uses);
            }
            Statement::ForEach {
                iterable, stmts, ..
            } => {
                collect_expression(iterable, uses);
                collect_statement_variable_uses(stmts, uses);
            }
            Statement::ForRange {
                start, end, stmts, ..
            } => {
                collect_expression(start, uses);
                collect_expression(end, uses);
                collect_statement_variable_uses(stmts, uses);
            }
            Statement::Try { stmts } | Statement::Catch { stmts } => {
                collect_statement_variable_uses(stmts, uses)
            }
            Statement::TryCatch { try_stmts, catches } => {
                collect_statement_variable_uses(try_stmts, uses);
                for catch in catches {
                    collect_statement_variable_uses(&catch.stmts, uses);
                }
            }
            Statement::StateMachine { blocks, .. } => {
                for block in blocks {
                    collect_statement_variable_uses(&block.stmts, uses);
                    match &block.terminator {
                        StateTerminator::Branch { cond, .. } => collect_expression(cond, uses),
                        StateTerminator::Switch { arg, .. } => collect_expression(arg, uses),
                        StateTerminator::Return(Some(value)) | StateTerminator::Throw(value) => {
                            collect_expression(value, uses)
                        }
                        StateTerminator::Goto(_)
                        | StateTerminator::Return(None)
                        | StateTerminator::Exit => {}
                    }
                }
            }
            Statement::RuntimeCheck(RuntimeCheck::Assert)
            | Statement::Nop
            | Statement::Return(None)
            | Statement::Break
            | Statement::Continue
            | Statement::Comment(_)
            | Statement::UnhandledOpcode { .. }
            | Statement::Provenanced { .. } => {}
        }
    }
}

fn check_expression_scope(
    expression: &Expr,
    declarations: &HashSet<(Reg, Option<Str>)>,
    available: &HashSet<(Reg, Option<Str>)>,
    hoist: &mut HashSet<(Reg, Option<Str>)>,
) {
    let mut variables = Vec::new();
    expression_variables(expression, &mut variables);
    for variable in variables {
        if declarations.contains(&variable) && !available.contains(&variable) {
            hoist.insert(variable);
        }
    }
}

fn find_undeclared_uses(
    statements: &[Statement],
    declarations: &HashSet<(Reg, Option<Str>)>,
    available: &mut HashSet<(Reg, Option<Str>)>,
    hoist: &mut HashSet<(Reg, Option<Str>)>,
) {
    for statement in statements {
        match raw_statement(statement) {
            Statement::Assign {
                declaration,
                variable,
                assign,
            } => {
                check_expression_scope(assign, declarations, available, hoist);
                if let Some(key) = variable_key(variable) {
                    if *declaration {
                        available.insert(key);
                    } else if declarations.contains(&key) && !available.contains(&key) {
                        hoist.insert(key);
                    }
                } else {
                    check_expression_scope(variable, declarations, available, hoist);
                }
            }
            Statement::VarDecl {
                variable, value, ..
            } => {
                if let Some(value) = value {
                    check_expression_scope(value, declarations, available, hoist);
                }
                if let Some(key) = variable_key(variable) {
                    available.insert(key);
                }
            }
            Statement::ExprStatement(value)
            | Statement::Throw(value)
            | Statement::RuntimeCheck(RuntimeCheck::Null(value)) => {
                check_expression_scope(value, declarations, available, hoist)
            }
            Statement::Return(Some(value))
            | Statement::GlobalStore { value, .. }
            | Statement::Prefetch { value, .. } => {
                check_expression_scope(value, declarations, available, hoist)
            }
            Statement::DynamicFieldStore { object, value, .. } => {
                check_expression_scope(object, declarations, available, hoist);
                check_expression_scope(value, declarations, available, hoist);
            }
            Statement::MemoryStore {
                bytes,
                index,
                value,
                ..
            } => {
                check_expression_scope(bytes, declarations, available, hoist);
                check_expression_scope(index, declarations, available, hoist);
                check_expression_scope(value, declarations, available, hoist);
            }
            Statement::ReferenceStore {
                reference, value, ..
            } => {
                check_expression_scope(reference, declarations, available, hoist);
                check_expression_scope(value, declarations, available, hoist);
            }
            Statement::IfElse { cond, if_, else_ } => {
                check_expression_scope(cond, declarations, available, hoist);
                let mut branch = available.clone();
                find_undeclared_uses(if_, declarations, &mut branch, hoist);
                let mut branch = available.clone();
                find_undeclared_uses(else_, declarations, &mut branch, hoist);
            }
            Statement::Switch {
                arg,
                default,
                cases,
            } => {
                check_expression_scope(arg, declarations, available, hoist);
                let mut branch = available.clone();
                find_undeclared_uses(default, declarations, &mut branch, hoist);
                for (patterns, body) in cases {
                    let mut branch = available.clone();
                    for pattern in patterns {
                        check_expression_scope(pattern, declarations, &branch, hoist);
                        if let Expr::EnumPatternBinding(_, _, variables) = raw_expr(pattern) {
                            for variable in variables {
                                if let Some(key) = variable_key(variable) {
                                    branch.insert(key);
                                }
                            }
                        }
                    }
                    find_undeclared_uses(body, declarations, &mut branch, hoist);
                }
            }
            Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
                check_expression_scope(cond, declarations, available, hoist);
                let mut nested = available.clone();
                find_undeclared_uses(stmts, declarations, &mut nested, hoist);
            }
            Statement::ForEach {
                variable,
                iterable,
                stmts,
            } => {
                check_expression_scope(iterable, declarations, available, hoist);
                let mut nested = available.clone();
                if let Some(key) = variable_key(variable) {
                    nested.insert(key);
                }
                find_undeclared_uses(stmts, declarations, &mut nested, hoist);
            }
            Statement::ForRange {
                variable,
                start,
                end,
                stmts,
            } => {
                check_expression_scope(start, declarations, available, hoist);
                check_expression_scope(end, declarations, available, hoist);
                let mut nested = available.clone();
                if let Some(key) = variable_key(variable) {
                    nested.insert(key);
                }
                find_undeclared_uses(stmts, declarations, &mut nested, hoist);
            }
            Statement::Try { stmts } | Statement::Catch { stmts } => {
                let mut nested = available.clone();
                find_undeclared_uses(stmts, declarations, &mut nested, hoist);
            }
            Statement::TryCatch { try_stmts, catches } => {
                let mut nested = available.clone();
                find_undeclared_uses(try_stmts, declarations, &mut nested, hoist);
                for catch in catches {
                    let mut nested = available.clone();
                    if let Some(key) = variable_key(&catch.variable) {
                        nested.insert(key);
                    }
                    find_undeclared_uses(&catch.stmts, declarations, &mut nested, hoist);
                }
            }
            Statement::StateMachine { .. } => {}
            Statement::RuntimeCheck(RuntimeCheck::Assert)
            | Statement::Nop
            | Statement::Return(None)
            | Statement::Break
            | Statement::Continue
            | Statement::Comment(_)
            | Statement::UnhandledOpcode { .. }
            | Statement::Provenanced { .. } => {}
        }
    }
}

fn clear_declarations(statements: &mut [Statement], hoisted: &HashSet<(Reg, Option<Str>)>) {
    for statement in statements {
        match raw_statement_mut(statement) {
            Statement::Assign {
                declaration,
                variable,
                ..
            } => {
                if variable_key(variable).map_or(false, |key| hoisted.contains(&key)) {
                    *declaration = false;
                }
            }
            Statement::IfElse { if_, else_, .. } => {
                clear_declarations(if_, hoisted);
                clear_declarations(else_, hoisted);
            }
            Statement::Switch { default, cases, .. } => {
                clear_declarations(default, hoisted);
                for (_, body) in cases {
                    clear_declarations(body, hoisted);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => clear_declarations(stmts, hoisted),
            Statement::TryCatch { try_stmts, catches } => {
                clear_declarations(try_stmts, hoisted);
                for catch in catches {
                    clear_declarations(&mut catch.stmts, hoisted);
                }
            }
            Statement::StateMachine { .. } => {}
            _ => {}
        }
    }
}

/// Visit everything depth-first
pub(crate) fn visit(
    code: &Bytecode,
    stmts: &mut [Statement],
    visitors: &mut [Box<dyn AstVisitor>],
) {
    // Recurse
    macro_rules! rec {
        ($stmts:expr) => {
            visit(code, $stmts, visitors)
        };
    }
    // Visit an expression
    macro_rules! v {
        ($e:expr) => {
            visit_expr(code, $e, visitors)
        };
    }
    for stmt in stmts {
        // No _ pattern, wouldn't want this match to de-sync when adding new items
        match stmt {
            Statement::VarDecl {
                variable, value, ..
            } => {
                v!(variable);
                if let Some(value) = value {
                    v!(value);
                }
            }
            Statement::Assign {
                assign, variable, ..
            } => {
                v!(assign);
                v!(variable);
            }
            Statement::ExprStatement(e) => {
                v!(e);
            }
            Statement::GlobalStore { value, .. } => v!(value),
            Statement::DynamicFieldStore { object, value, .. } => {
                v!(object);
                v!(value);
            }
            Statement::MemoryStore {
                bytes,
                index,
                value,
                ..
            } => {
                v!(bytes);
                v!(index);
                v!(value);
            }
            Statement::ReferenceStore {
                reference, value, ..
            } => {
                v!(reference);
                v!(value);
            }
            Statement::RuntimeCheck(RuntimeCheck::Null(value)) => v!(value),
            Statement::RuntimeCheck(RuntimeCheck::Assert) => {}
            Statement::Prefetch { value, .. } => v!(value),
            Statement::Nop => {}
            Statement::Return(opt) => {
                if let Some(e) = opt {
                    v!(e);
                }
            }
            Statement::IfElse { cond, if_, else_ } => {
                v!(cond);
                rec!(if_);
                rec!(else_);
            }
            Statement::Switch {
                arg,
                default,
                cases,
            } => {
                v!(arg);
                rec!(default);
                for (patterns, case) in cases {
                    patterns.iter_mut().for_each(|pattern| v!(pattern));
                    rec!(case);
                }
            }
            Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
                v!(cond);
                rec!(stmts);
            }
            Statement::ForEach {
                variable,
                iterable,
                stmts,
            } => {
                v!(variable);
                v!(iterable);
                rec!(stmts);
            }
            Statement::ForRange {
                variable,
                start,
                end,
                stmts,
            } => {
                v!(variable);
                v!(start);
                v!(end);
                rec!(stmts);
            }
            Statement::Break => {}
            Statement::Continue => {}
            Statement::Throw(e) => {
                v!(e);
            }
            Statement::Try { stmts } => {
                rec!(stmts);
            }
            Statement::Catch { stmts } => {
                rec!(stmts);
            }
            Statement::TryCatch { try_stmts, catches } => {
                rec!(try_stmts);
                for catch in catches {
                    v!(&mut catch.variable);
                    rec!(&mut catch.stmts);
                }
            }
            Statement::StateMachine { locals, blocks, .. } => {
                for local in locals {
                    v!(local);
                }
                for block in blocks {
                    rec!(&mut block.stmts);
                    match &mut block.terminator {
                        StateTerminator::Branch { cond, .. } => v!(cond),
                        StateTerminator::Switch { arg, .. } => v!(arg),
                        StateTerminator::Return(Some(value)) | StateTerminator::Throw(value) => {
                            v!(value)
                        }
                        StateTerminator::Goto(_)
                        | StateTerminator::Return(None)
                        | StateTerminator::Exit => {}
                    }
                    if let Some(exception) = &mut block.exception {
                        v!(&mut exception.variable);
                    }
                }
            }
            Statement::Comment(_) => {}
            Statement::UnhandledOpcode { .. } => {}
            Statement::Provenanced { statement, .. } => {
                visit(code, std::slice::from_mut(statement), visitors);
            }
        }
        for visitor in visitors.iter_mut() {
            visitor.visit_stmt(code, stmt);
        }
    }
}

/// Visit expressions by depth-first recursion into [Expr].
pub(crate) fn visit_expr(code: &Bytecode, expr: &mut Expr, visitors: &mut [Box<dyn AstVisitor>]) {
    // Recurse
    macro_rules! rec {
        ($e:expr) => {
            visit_expr(code, $e, visitors)
        };
    }
    // Visit statements
    macro_rules! v {
        ($stmts:expr) => {
            visit(code, $stmts, visitors)
        };
    }
    // No _ pattern, wouldn't want this match to de-sync when adding new items
    match expr {
        Expr::Anonymous(_, fields) => {
            for e in fields.values_mut() {
                rec!(e);
            }
        }
        Expr::Array(arr, index) => {
            rec!(arr);
            rec!(index);
        }
        Expr::ArrayLiteral { elements, .. } => {
            for element in elements {
                rec!(element);
            }
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                rec!(key);
                rec!(value);
            }
        }
        Expr::ArrayAlloc { length, .. } => rec!(length),
        Expr::Bytes(_) => {}
        Expr::Call(call) => {
            rec!(&mut call.fun);
            for arg in call.args.iter_mut() {
                rec!(arg);
            }
        }
        Expr::Constant(_) => {}
        Expr::Constructor(ConstructorCall { args, .. }) => {
            for arg in args {
                rec!(arg);
            }
        }
        // /!\ No recurse in closure, as closure decompilation is already recursive.
        Expr::Closure(_, _, _, _) | Expr::Capture(_) => {}
        Expr::EnumConstr(_, _, args) => {
            for arg in args {
                rec!(arg);
            }
        }
        Expr::EnumIndex(value) => rec!(value),
        Expr::EnumPattern(_, _, _) => {}
        Expr::EnumPatternBinding(_, _, variables) => {
            for variable in variables {
                rec!(variable);
            }
        }
        Expr::EnumField { value, .. } => rec!(value),
        Expr::Field(obj, _) | Expr::DynamicField(obj, _) => {
            rec!(obj);
        }
        Expr::FunRef(_) => {}
        Expr::GlobalLoad { .. } => {}
        Expr::SuperCall(arguments) => {
            for argument in arguments {
                rec!(argument);
            }
        }
        Expr::SuperMethod { args, .. } => {
            for argument in args {
                rec!(argument);
            }
        }
        Expr::MemoryLoad { bytes, index, .. } => {
            rec!(bytes);
            rec!(index);
        }
        Expr::TypeValue { .. } => {}
        Expr::RuntimeType { value, .. }
        | Expr::TypeId { value, .. }
        | Expr::SafeCast { value, .. } => rec!(value),
        Expr::VirtualClosure { receiver, .. } => rec!(receiver),
        Expr::Reference { value, .. } => rec!(value),
        Expr::Dereference { reference, .. } => rec!(reference),
        Expr::ReferenceData { array, .. } => rec!(array),
        Expr::ReferenceOffset {
            reference, offset, ..
        } => {
            rec!(reference);
            rec!(offset);
        }
        Expr::IfElse { cond, if_, else_ } => {
            rec!(cond);
            v!(if_);
            v!(else_);
        }
        Expr::Op(op) => match op {
            Operation::Add(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Sub(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Mul(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Div(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Mod(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Shl(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Shr(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::And(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Or(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::BitAnd(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::BitOr(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Xor(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Neg(e1) => {
                rec!(e1);
            }
            Operation::Not(e1) => {
                rec!(e1);
            }
            Operation::Incr(e1) => {
                rec!(e1);
            }
            Operation::Decr(e1) => {
                rec!(e1);
            }
            Operation::Eq(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::NotEq(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Gt(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Gte(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Lt(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
            Operation::Lte(e1, e2) => {
                rec!(e1);
                rec!(e2);
            }
        },
        Expr::StringConcat(expressions) => {
            for expression in expressions {
                rec!(expression);
            }
        }
        Expr::StringInterpolation(parts) => {
            for part in parts {
                if let StringPart::Expression(expression) = part {
                    rec!(expression);
                }
            }
        }
        Expr::ToString(expression) => rec!(expression),
        Expr::Unknown(_) => {}
        Expr::Variable(_, _) => {}
        Expr::Provenanced { expression, .. } => rec!(expression),
    }
    for visitor in visitors.iter_mut() {
        visitor.visit_expr(code, expr);
    }
}

/// Transforms an if/else statement where both branches assign a value to the same variable to an if/else expression.
/// ```haxe
/// if (cond) {
///     var a = 1;
/// } else {
///     a = 2;
/// }
/// ```
/// becomes this :
/// ```haxe
/// var a = if (cond) {
///     1
/// } else {
///     2
/// };
/// ```
pub(crate) struct IfExpressions;

impl AstVisitor for IfExpressions {
    fn visit_stmt(&mut self, _code: &Bytecode, stmt: &mut Statement) {
        let opt = match stmt {
            Statement::IfElse { cond, if_, else_ } => {
                // We only have to check the last statement in each branches.
                // We assume their types to be the same (checked by the haxe compiler)
                match if_.last() {
                    Some(Statement::Assign {
                        declaration,
                        variable: if_var,
                        assign: if_assign,
                    }) => match else_.last() {
                        Some(Statement::Assign {
                            variable: else_var,
                            assign: else_assign,
                            ..
                        }) => match if_var {
                            Expr::Variable(r1, _) => match else_var {
                                Expr::Variable(r2, _) if r1 == r2 => Some((
                                    *declaration,
                                    if_var.clone(),
                                    cond.clone(),
                                    if_assign.clone(),
                                    else_assign.clone(),
                                    if_.clone(),
                                    else_.clone(),
                                )),
                                _ => None,
                            },
                            _ => None,
                        },
                        _ => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        };

        if let Some((decl, var, cond, if_assign, else_assign, mut if_stmts, mut else_stmts)) = opt {
            let (Some(last_if), Some(last_else)) = (if_stmts.last_mut(), else_stmts.last_mut())
            else {
                return;
            };
            *last_if = Statement::ExprStatement(if_assign);
            *last_else = Statement::ExprStatement(else_assign);
            *stmt = Statement::Assign {
                declaration: decl,
                variable: var,
                assign: Expr::IfElse {
                    cond: Box::new(cond),
                    if_: if_stmts,
                    else_: else_stmts,
                },
            }
        }
    }
}

// TODO AST-PP switch expressions

/// Restore string concatenation. They are translated to calls to \_\_add__ at compilation.
/// ```haxe
/// __add__("hello ", "world")
/// ```
/// becomes :
/// ```haxe
/// "hello " + "world"
/// ```
pub(crate) struct StringConcat;

impl AstVisitor for StringConcat {
    fn visit_expr(&mut self, code: &Bytecode, expr: &mut Expr) {
        let args = match expr {
            Expr::Call(call)
                if callee_named(code, &call.fun, "__add__") && call.args.len() == 2 =>
            {
                Some((call.args[0].clone(), call.args[1].clone()))
            }
            _ => None,
        };

        if let Some((arg0, arg1)) = args {
            let mut expressions = Vec::new();
            append_concat(&mut expressions, arg0);
            append_concat(&mut expressions, arg1);
            *expr = Expr::StringConcat(expressions);
        }
    }
}

fn append_concat(expressions: &mut Vec<Expr>, expression: Expr) {
    match expression {
        Expr::StringConcat(nested) => expressions.extend(nested),
        expression => expressions.push(expression),
    }
}

/// Remove calls to `std/itos` and `std/alloc` when converting an integer to a string.
pub(crate) struct Itos;

impl AstVisitor for Itos {
    fn visit_expr(&mut self, code: &Bytecode, expr: &mut Expr) {
        let var = match expr {
            Expr::Call(call) if callee_named(code, &call.fun, "__alloc__") => call
                .args
                .first()
                .and_then(|argument| match raw_expr(argument) {
                    Expr::Call(call) if callee_named(code, &call.fun, "itos") => {
                        call.args.first().cloned()
                    }
                    _ => None,
                }),
            _ => None,
        };

        if let Some(int) = var {
            *expr = Expr::ToString(Box::new(int));
        }
    }
}

fn callee_named(code: &Bytecode, expression: &Expr, expected: &str) -> bool {
    match raw_expr(expression) {
        Expr::FunRef(function) => function.name(code) == expected,
        Expr::Field(_, field) => field == expected,
        _ => false,
    }
}

#[cfg(test)]
#[allow(clippy::items_after_test_module)]
mod recovery_tests {
    use super::*;
    use hlbc::types::{RefFun, RefString};

    #[test]
    fn constructor_and_anonymous_recovery_preserve_order() {
        let constructor =
            recover_constructor_and_anonymous_object(ObjectRecoveryCandidate::Constructor {
                ty: RefType(7),
                arguments: vec![
                    Expr::Unknown("first".to_owned()),
                    Expr::Unknown("second".to_owned()),
                ],
            });
        let Expr::Constructor(call) = constructor else {
            panic!("constructor candidate was not recovered");
        };
        assert_eq!(call.ty, RefType(7));
        assert!(matches!(&call.args[0], Expr::Unknown(value) if value == "first"));
        assert!(matches!(&call.args[1], Expr::Unknown(value) if value == "second"));

        let mut fields = BTreeMap::new();
        fields.insert(RefField(0), Expr::Unknown("value".to_owned()));
        let anonymous =
            recover_constructor_and_anonymous_object(ObjectRecoveryCandidate::Anonymous {
                ty: RefType(8),
                fields,
            });
        assert!(matches!(
            anonymous,
            Expr::Anonymous(RefType(8), values)
                if matches!(values.get(&RefField(0)), Some(Expr::Unknown(value)) if value == "value")
        ));
    }

    #[test]
    fn array_literal_recovery_preserves_element_order() {
        let recovered = recover_array_literal(ArrayLiteralCandidate {
            elements: vec![
                Expr::Unknown("first()".to_owned()),
                Expr::Unknown("second()".to_owned()),
            ],
            element_type: Some(RefType(3)),
            native: false,
        });
        let Expr::ArrayLiteral { elements, .. } = recovered else {
            panic!("array candidate was not recovered");
        };
        assert!(matches!(&elements[0], Expr::Unknown(value) if value == "first()"));
        assert!(matches!(&elements[1], Expr::Unknown(value) if value == "second()"));
    }

    #[test]
    fn map_literal_recovery_preserves_order_and_refuses_aliases() {
        let code = Bytecode::from_file("../../data/MapLiteral.hl").unwrap();
        let map_type = code
            .types
            .iter()
            .position(|ty| {
                matches!(
                    ty,
                    Type::Obj(object) | Type::Struct(object)
                        if object.name(&code).as_str() == "haxe.ds.StringMap"
                )
            })
            .map(RefType)
            .unwrap();
        let map = Expr::Variable(Reg(1), Some(Str::from("values")));
        let key = Expr::Variable(Reg(2), None);
        let value = Expr::Variable(Reg(3), None);
        let mut statements = vec![
            Statement::Assign {
                declaration: true,
                variable: map.clone(),
                assign: Expr::Constructor(ConstructorCall::new(map_type, Vec::new())),
            },
            Statement::Assign {
                declaration: true,
                variable: key.clone(),
                assign: Expr::Unknown("key()".to_owned()),
            },
            Statement::Assign {
                declaration: true,
                variable: value.clone(),
                assign: Expr::Unknown("value()".to_owned()),
            },
            map_set_statement(map.clone(), key.clone(), value.clone()),
        ];
        recover_map_literals(&code, &mut statements);
        assert_eq!(statements.len(), 1);
        let (_, initializer) = statement_initializer(&statements[0]).unwrap();
        let Expr::MapLiteral { entries } = raw_expr(initializer) else {
            panic!("ordered set sequence was not recovered");
        };
        assert!(matches!(&entries[0].0, Expr::Unknown(value) if value == "key()"));
        assert!(matches!(&entries[0].1, Expr::Unknown(value) if value == "value()"));

        let mut aliased = vec![
            Statement::Assign {
                declaration: true,
                variable: map.clone(),
                assign: Expr::Constructor(ConstructorCall::new(map_type, Vec::new())),
            },
            Statement::Assign {
                declaration: true,
                variable: key.clone(),
                assign: Expr::Unknown("key()".to_owned()),
            },
            Statement::Assign {
                declaration: true,
                variable: value.clone(),
                assign: Expr::Unknown("value()".to_owned()),
            },
            map_set_statement(map, key.clone(), value),
            Statement::Return(Some(key)),
        ];
        recover_map_literals(&code, &mut aliased);
        assert_eq!(aliased.len(), 5);
        let (_, initializer) = statement_initializer(&aliased[0]).unwrap();
        assert!(matches!(raw_expr(initializer), Expr::Constructor(_)));
    }

    #[test]
    fn string_and_trace_recovery_fold_runtime_calls_once() {
        let code = Bytecode::from_file("../../data/MapLiteral.hl").unwrap();
        let value = Expr::Variable(Reg(0), Some(Str::from("value")));
        let conversion = crate::call_fun(
            RefFun(16),
            vec![
                crate::call_fun(
                    RefFun(28),
                    vec![value.clone(), Expr::Variable(Reg(2), None)],
                ),
                value.clone(),
            ],
        );
        let concat = crate::call_fun(
            RefFun(20),
            vec![
                Expr::Constant(crate::ast::Constant::String(RefString(48))),
                conversion,
            ],
        );
        let trace_receiver = Expr::Variable(Reg(4), Some(Str::from("haxe_Log")));
        let trace = crate::ast::call(
            Expr::Field(Box::new(trace_receiver), Str::from("trace")),
            vec![value],
        );
        let mut statements = vec![
            Statement::ExprStatement(concat),
            Statement::ExprStatement(trace),
        ];
        recover_strings_and_trace(&code, &mut statements);
        assert!(matches!(
            raw_statement(&statements[0]),
            Statement::ExprStatement(Expr::StringInterpolation(parts))
                if parts.iter().filter(|part| matches!(part, StringPart::Expression(_))).count() == 1
        ));
        assert!(matches!(
            raw_statement(&statements[1]),
            Statement::ExprStatement(Expr::Call(call))
                if matches!(raw_expr(&call.fun), Expr::FunRef(fun) if fun.name(&code) == "trace")
                    && call.args.len() == 1
        ));
    }

    fn map_set_statement(map: Expr, key: Expr, value: Expr) -> Statement {
        Statement::ExprStatement(crate::ast::call(
            Expr::Field(Box::new(map), Str::from("set")),
            vec![key, value],
        ))
    }
}

/// Fold string constants and explicit conversions into Haxe interpolation.
pub(crate) struct StringInterpolation;

impl AstVisitor for StringInterpolation {
    fn visit_expr(&mut self, code: &Bytecode, expr: &mut Expr) {
        let Expr::StringConcat(expressions) = expr else {
            return;
        };
        if !expressions.iter().any(|expression| {
            matches!(
                raw_expr(expression),
                Expr::ToString(_) | Expr::StringInterpolation(_)
            )
        }) {
            return;
        }

        let mut parts = Vec::new();
        for expression in std::mem::take(expressions) {
            let expression = raw_expr(&expression).clone();
            match expression {
                Expr::Constant(crate::ast::Constant::String(reference)) => {
                    let text = code
                        .strings
                        .get(reference.0)
                        .map(|text| text.to_string())
                        .unwrap_or_default();
                    if let Some(StringPart::Literal(previous)) = parts.last_mut() {
                        previous.push_str(&text);
                    } else {
                        parts.push(StringPart::Literal(text));
                    }
                }
                Expr::ToString(expression) => parts.push(StringPart::Expression(*expression)),
                Expr::StringInterpolation(nested) => {
                    for part in nested {
                        match part {
                            StringPart::Literal(text) => {
                                if let Some(StringPart::Literal(previous)) = parts.last_mut() {
                                    previous.push_str(&text);
                                } else {
                                    parts.push(StringPart::Literal(text));
                                }
                            }
                            StringPart::Expression(expression) => {
                                parts.push(StringPart::Expression(expression))
                            }
                        }
                    }
                }
                expression => parts.push(StringPart::Expression(expression)),
            }
        }
        *expr = Expr::StringInterpolation(parts);
    }
}

/// Restore inlined `trace` calls.
pub(crate) struct Trace;

impl AstVisitor for Trace {
    fn visit_expr(&mut self, code: &Bytecode, expr: &mut Expr) {
        let call = match expr {
            Expr::Call(call) => match &call.fun {
                Expr::Field(obj, field) => match obj.as_ref() {
                    Expr::Variable(_, _) => {
                        if field == "trace" {
                            code.function_by_name(field)
                                .map(|trace| call_fun(trace.findex, call.args.clone()))
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        };
        if let Some(call) = call {
            *expr = call;
        }
    }
}

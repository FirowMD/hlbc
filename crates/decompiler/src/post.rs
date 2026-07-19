use hlbc::Bytecode;

use crate::ast::{add, ConstructorCall, Expr, Operation, RuntimeCheck, StateTerminator, Statement};
use crate::call_fun;

pub(crate) trait AstVisitor {
    fn visit_stmt(&mut self, _code: &Bytecode, _stmt: &mut Statement) {}
    fn visit_expr(&mut self, _code: &Bytecode, _expr: &mut Expr) {}
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
        Expr::Closure(_, _) => {}
        Expr::EnumConstr(_, _, args) => {
            for arg in args {
                rec!(arg);
            }
        }
        Expr::EnumIndex(value) => rec!(value),
        Expr::EnumPattern(_, _, _) => {}
        Expr::Field(obj, _) => {
            rec!(obj);
        }
        Expr::FunRef(_) => {}
        Expr::MemoryLoad { bytes, index, .. } => {
            rec!(bytes);
            rec!(index);
        }
        Expr::TypeValue { .. } => {}
        Expr::RuntimeType { value, .. } | Expr::TypeId { value, .. } => rec!(value),
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
            Expr::Call(call) => match call.fun {
                Expr::FunRef(fun) => {
                    if fun.name(code) == "__add__" && call.args.len() == 2 {
                        Some((call.args[0].clone(), call.args[1].clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        };

        if let Some((arg0, arg1)) = args {
            *expr = add(arg0, arg1);
        }
    }
}

/// Remove calls to `std/itos` and `std/alloc` when converting an integer to a string.
pub(crate) struct Itos;

impl AstVisitor for Itos {
    fn visit_expr(&mut self, code: &Bytecode, expr: &mut Expr) {
        let var = match expr {
            Expr::Call(call) => match call.fun {
                Expr::FunRef(fun) if fun.name(code) == "__alloc__" => match call.args.first() {
                    Some(Expr::Call(call)) => match call.fun {
                        Expr::FunRef(fun) if fun.name(code) == "itos" => call.args.first().cloned(),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        };

        if let Some(int) = var {
            *expr = int;
        }
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
                            code.function_by_name(field).and_then(|trace| {
                                call.args
                                    .first()
                                    .cloned()
                                    .map(|argument| call_fun(trace.findex, vec![argument]))
                            })
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

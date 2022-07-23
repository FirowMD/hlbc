//! The decompiler used to get haxe sources back from the bytecode definitions.
//! More info on how everything works in the [wiki](https://github.com/Gui-Yom/hlbc/wiki/Decompilation).
//!
//! The decompiler takes bytecode elements as input and outputs [ast] structures that can be displayed.

use std::collections::{HashMap, HashSet};

use ast::*;

use crate::types::{FunPtr, Function, RefField, Reg, Type, TypeObj};
use crate::Bytecode;
use crate::Opcode;

/// A simple representation for the Haxe source code generated by the decompiler
pub mod ast;
/// Functions to render the [ast] to a string
pub mod fmt;

/// Decompile a class with its static and instance fields and methods.
pub fn decompile_class(code: &Bytecode, obj: &TypeObj) -> Class {
    let static_type = obj.get_static_type(code);

    let mut fields = Vec::new();
    for (i, f) in obj.own_fields.iter().enumerate() {
        if obj
            .bindings
            .get(&RefField(i + obj.fields.len() - obj.own_fields.len()))
            .is_some()
        {
            continue;
        }
        fields.push(ClassField {
            name: f.name.display(code),
            static_: false,
            ty: f.t,
        });
    }
    if let Some(ty) = static_type {
        for (i, f) in ty.own_fields.iter().enumerate() {
            if ty
                .bindings
                .get(&RefField(i + ty.fields.len() - ty.own_fields.len()))
                .is_some()
            {
                continue;
            }
            fields.push(ClassField {
                name: f.name.display(code),
                static_: true,
                ty: f.t,
            });
        }
    }

    let mut methods = Vec::new();
    for fun in obj.bindings.values() {
        methods.push(Method {
            fun: *fun,
            static_: false,
            dynamic: true,
            statements: decompile_function(code, fun.resolve_as_fn(code).unwrap()),
        })
    }
    if let Some(ty) = static_type {
        for fun in ty.bindings.values() {
            methods.push(Method {
                fun: *fun,
                static_: true,
                dynamic: false,
                statements: decompile_function(code, fun.resolve_as_fn(code).unwrap()),
            })
        }
    }
    for f in &obj.protos {
        methods.push(Method {
            fun: f.findex,
            static_: false,
            dynamic: false,
            statements: decompile_function(code, f.findex.resolve_as_fn(code).unwrap()),
        })
    }

    Class {
        name: obj.name.resolve(&code.strings).to_owned(),
        parent: obj
            .super_
            .and_then(|ty| ty.resolve_as_obj(&code.types))
            .map(|ty| ty.name.display(code)),
        fields,
        methods,
    }
}

/// Helper to process a stack of scopes (branches, loops)
struct Scopes {
    // A linked list would be appreciable i think
    /// There is always at least one scope, the root scope
    scopes: Vec<Scope>,
}

impl Scopes {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::Root(Vec::new())],
        }
    }

    fn push_if(&mut self, len: i32, cond: Expr) {
        self.scopes.push(Scope::If {
            len,
            cond,
            stmts: Vec::new(),
        })
    }

    fn push_else(&mut self, len: i32) {
        self.scopes.push(Scope::Else {
            len,
            stmts: Vec::new(),
        })
    }

    fn push_loop(&mut self, start: usize) {
        self.scopes.push(Scope::Loop(LoopScope {
            start,
            cond: None,
            stmts: Vec::new(),
        }))
    }

    fn push_switch(&mut self, len: i32, arg: Expr, offsets: Vec<usize>) {
        self.scopes.push(Scope::Switch(SwitchScope {
            len,
            arg,
            offsets,
            default: Vec::new(),
            cases: Vec::new(),
        }))
    }

    fn push_switch_case(&mut self, cst: usize) {
        let last = self.pop();
        match last {
            Scope::Switch(switch) => {
                self.scopes.push(Scope::Switch(switch));
                self.scopes.push(Scope::SwitchCase(SwitchCase {
                    pattern: cst_int(cst as i32),
                    stmts: Vec::new(),
                }));
            }
            Scope::SwitchCase(case) => {
                if let Scope::Switch(switch) = self.last_mut() {
                    switch.cases.push(case);
                } else {
                    panic!("push switch case without switch ?\n{:#?}", self.scopes);
                }
                self.scopes.push(Scope::SwitchCase(SwitchCase {
                    pattern: cst_int(cst as i32),
                    stmts: Vec::new(),
                }));
            }
            _ => {
                self.scopes.push(last);
            }
        }
    }

    fn pop_last_loop(&mut self) -> Option<LoopScope> {
        for idx in (0..self.depth()).rev() {
            let scope = self.scopes.remove(idx);
            match scope {
                Scope::Loop(l) => {
                    return Some(l);
                }
                _ => {
                    self.scopes.insert(idx, scope);
                }
            }
        }
        None
    }

    fn last_loop(&self) -> Option<&LoopScope> {
        for idx in (0..self.depth()).rev() {
            if let Scope::Loop(l) = &self.scopes[idx] {
                return Some(l);
            }
        }
        None
    }

    fn last(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn last_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn pop(&mut self) -> Scope {
        self.scopes.pop().unwrap()
    }

    fn depth(&self) -> usize {
        self.scopes.len()
    }

    fn push_stmt(&mut self, mut stmt: Option<Statement>) {
        // Start to iterate from the end ('for' because we need the index)
        for idx in (0..self.depth()).rev() {
            let scope = self.scopes.remove(idx);

            // We only handle branches we know the length of
            // We can't know the end of a loop scope before seeing the jump back
            match scope {
                Scope::If {
                    mut len,
                    cond,
                    mut stmts,
                } => {
                    if let Some(stmt) = stmt.take() {
                        stmts.push(stmt);
                    }

                    // Decrease scope len
                    len -= 1;
                    if len <= 0 {
                        //println!("Decrease nesting {parent:?}");
                        stmt = Some(Statement::If { cond, stmts });
                    } else {
                        // Scope continues
                        self.scopes.insert(idx, Scope::If { len, cond, stmts });
                    }
                }
                Scope::Else { mut len, mut stmts } => {
                    if let Some(stmt) = stmt.take() {
                        stmts.push(stmt);
                    }

                    // Decrease scope len
                    len -= 1;
                    if len <= 0 {
                        //println!("Decrease nesting {parent:?}");
                        stmt = Some(Statement::Else { stmts });
                    } else {
                        // Scope continues
                        self.scopes.insert(idx, Scope::Else { len, stmts });
                    }
                }
                Scope::Switch(mut switch) => {
                    if let Some(stmt) = stmt.take() {
                        switch.default.push(stmt);
                    }

                    switch.len -= 1;
                    if switch.len <= 0 {
                        stmt = Some(Statement::Switch {
                            arg: switch.arg,
                            default: switch.default,
                            cases: switch
                                .cases
                                .into_iter()
                                .map(|case| (case.pattern, case.stmts))
                                .collect(),
                        });
                    } else {
                        self.scopes.insert(idx, Scope::Switch(switch));
                    }
                }
                Scope::SwitchCase(mut case) => {
                    if let Some(stmt) = stmt.take() {
                        case.stmts.push(stmt);
                    }
                    if let Scope::Switch(switch) = self.last_mut() {
                        if switch.len <= 1 {
                            switch.cases.push(case);
                        } else {
                            self.scopes.insert(idx, Scope::SwitchCase(case));
                        }
                    }
                }
                Scope::Root(mut stmts) => {
                    if let Some(stmt) = stmt.take() {
                        stmts.push(stmt);
                    }
                    self.scopes.insert(idx, Scope::Root(stmts));
                }
                Scope::Loop(mut loop_) => {
                    if let Some(stmt) = stmt.take() {
                        loop_.stmts.push(stmt);
                    }
                    self.scopes.insert(idx, Scope::Loop(loop_));
                }
            }
        }
        if let Some(stmt) = stmt.take() {
            match self.last_mut() {
                Scope::Root(stmts)
                | Scope::If { stmts, .. }
                | Scope::Else { stmts, .. }
                | Scope::Switch(SwitchScope { default: stmts, .. })
                | Scope::SwitchCase(SwitchCase { stmts, .. })
                | Scope::Loop(LoopScope { stmts, .. }) => {
                    stmts.push(stmt);
                }
            }
        }
    }

    fn statements(mut self) -> Vec<Statement> {
        if let Scope::Root(stmts) = self.pop() {
            stmts
        } else {
            unreachable!("mmmmhhh kinda sus:\n{:#?}\n", self.scopes);
        }
    }
}

#[derive(Debug)]
enum Scope {
    Root(Vec<Statement>),
    If {
        len: i32,
        cond: Expr,
        stmts: Vec<Statement>,
    },
    Else {
        len: i32,
        stmts: Vec<Statement>,
    },
    Loop(LoopScope),
    Switch(SwitchScope),
    SwitchCase(SwitchCase),
}

#[derive(Debug)]
struct LoopScope {
    cond: Option<Expr>,
    start: usize,
    stmts: Vec<Statement>,
}

#[derive(Debug)]
struct SwitchScope {
    len: i32,
    offsets: Vec<usize>,
    arg: Expr,
    default: Vec<Statement>,
    cases: Vec<SwitchCase>,
}

#[derive(Debug)]
struct SwitchCase {
    pattern: Expr,
    stmts: Vec<Statement>,
}

enum ExprCtx {
    Constructor {
        reg: Reg,
        pos: usize,
    },
    Anonymous {
        pos: usize,
        fields: HashMap<RefField, Expr>,
        remaining: usize,
    },
}

/// Decompile a function to a list of [Statement]s.
/// This works by analyzing each opcodes in order while trying to construct contexts and intents.
pub fn decompile_function(code: &Bytecode, f: &Function) -> Vec<Statement> {
    // Scope stack, holds the statements
    let mut scopes = Scopes::new();
    // Current iteration statement, to be pushed onto the finished statements or the nesting
    let mut statement = None;
    // Expression values for each registers
    let mut reg_state = HashMap::with_capacity(f.regs.len());
    // For parsing statements made of multiple instructions like constructor calls and anonymous structures
    // TODO move this to another pass on the generated ast
    let mut expr_ctx = Vec::new();
    // Variable names we already declared
    let mut seen = HashSet::new();

    let mut start = 0;
    // First argument / First register is 'this'
    if f.is_method()
        || f.name
            .map(|n| n.resolve(&code.strings) == "__constructor__")
            .unwrap_or(false)
    {
        reg_state.insert(Reg(0), cst_this());
        start = 1;
    }

    // Initialize register state with the function arguments
    for i in start..f.ty(code).args.len() {
        let name = f.arg_name(code, i - start).map(ToOwned::to_owned);
        reg_state.insert(Reg(i as u32), Expr::Variable(Reg(i as u32), name.clone()));
        if let Some(name) = name {
            seen.insert(name);
        }
    }

    macro_rules! push_stmt {
        ($stmt:expr) => {
            statement = Some($stmt);
        };
    }

    // Update the register state and create a statement depending on inline rules
    macro_rules! push_expr {
        ($i:expr, $dst:expr, $e:expr) => {
            let name = f.var_name(code, $i);
            let expr = $e;
            // Inline check
            if name.is_none() {
                reg_state.insert($dst, expr);
            } else {
                reg_state.insert($dst, Expr::Variable($dst, name.clone()));
                push_stmt!(Statement::Assign {
                    declaration: seen.insert(name.clone().unwrap()),
                    variable: Expr::Variable($dst, name),
                    assign: expr,
                });
            }
        };
    }

    macro_rules! push_expr_stmt {
        ($i:ident, $dst:ident, $e:expr) => {
            if f.var_name(code, $i).is_some() {
                push_stmt!(stmt($e));
            } else {
                reg_state.insert($dst, $e);
            }
        };
    }

    let missing_expr = || Expr::Unknown("missing expr".to_owned());

    // Get the expr for a register
    macro_rules! expr {
        ($reg:expr) => {
            reg_state.get(&$reg).cloned().unwrap_or_else(missing_expr)
        };
    }

    // Crate a Vec<Expression> for a list of args
    macro_rules! make_args {
        ($($arg:expr),* $(,)?) => {
            vec![$(expr!($arg)),*]
        }
    }

    macro_rules! push_call {
        ($i:ident, $dst:ident, $fun:ident, $arg0:expr $(, $args:expr)*) => {
            if let Some(&ExprCtx::Constructor { reg, pos }) = expr_ctx.last() {
                if reg == $arg0 {
                    push_expr!(
                        pos,
                        reg,
                        Expr::Constructor(ConstructorCall::new(f.regtype(reg), make_args!($($args),*)))
                    );
                    expr_ctx.pop();
                }
            } else {
                match $fun.resolve(code) {
                    FunPtr::Fun(func) => {
                        let call = if func.is_method() {
                            call(Expr::Field(Box::new(expr!($arg0)), func.name.clone().unwrap().resolve(&code.strings).to_owned()), make_args!($($args),*))
                        } else {
                            call_fun($fun, make_args!($arg0 $(, $args)*))
                        };
                        if func.ty(code).ret.is_void() {
                            push_stmt!(stmt(call));
                        } else {
                            push_expr!($i, $dst, call);
                        }
                    }
                    FunPtr::Native(n) => {
                        let call = call_fun($fun, make_args!($arg0 $(, $args)*));
                        if n.ty(code).ret.is_void() {
                            push_stmt!(stmt(call));
                        } else {
                            push_expr!($i, $dst, call);
                        }
                    }
                }
            }
        };
    }

    // Process a jmp instruction, might be the exit condition of a loop or an if
    macro_rules! push_jmp {
        ($i:ident, $offset:ident, $cond:expr) => {
            if $offset > 0 {
                // It's a loop
                if matches!(f.ops[$i + $offset as usize], Opcode::JAlways { offset } if offset < 0) {
                    if let Scope::Loop(loop_) = scopes.last_mut() {
                        if loop_.cond.is_none() {
                            loop_.cond = Some($cond);
                        } else {
                            scopes.push_if($offset + 1, $cond);
                        }
                    } else {
                        scopes.push_if($offset + 1, $cond);
                    }
                } else {
                    // It's an if
                    scopes.push_if($offset + 1, $cond);
                }
            }
        }
    }

    let iter = f.ops.iter().enumerate();
    for (i, o) in iter {
        // Opcodes are in semantic order
        match o {
            //region CONSTANTS
            &Opcode::Int { dst, ptr } => {
                push_expr!(i, dst, cst_int(ptr.resolve(&code.ints)));
            }
            &Opcode::Float { dst, ptr } => {
                push_expr!(i, dst, cst_float(ptr.resolve(&code.floats)));
            }
            &Opcode::Bool { dst, value } => {
                push_expr!(i, dst, cst_bool(value.0));
            }
            &Opcode::String { dst, ptr } => {
                push_expr!(i, dst, cst_string(ptr.resolve(&code.strings).to_owned()));
            }
            &Opcode::Null { dst } => {
                push_expr!(i, dst, cst_null());
            }
            //endregion

            //region OPERATORS
            &Opcode::Mov { dst, src } => {
                push_expr!(i, dst, expr!(src));
                // Workaround for when the instructions after this one use dst and src interchangeably.
                reg_state.insert(src, Expr::Variable(dst, f.var_name(code, i)));
            }
            &Opcode::Add { dst, a, b } => {
                push_expr!(i, dst, add(expr!(a), expr!(b)));
            }
            &Opcode::Sub { dst, a, b } => {
                push_expr!(i, dst, sub(expr!(a), expr!(b)));
            }
            &Opcode::Mul { dst, a, b } => {
                push_expr!(i, dst, mul(expr!(a), expr!(b)));
            }
            &Opcode::Shl { dst, a, b } => {
                push_expr!(i, dst, shl(expr!(a), expr!(b)));
            }
            &Opcode::SShr { dst, a, b } | &Opcode::UShr { dst, a, b } => {
                push_expr!(i, dst, shr(expr!(a), expr!(b)));
            }
            &Opcode::And { dst, a, b } => {
                push_expr!(i, dst, and(expr!(a), expr!(b)));
            }
            &Opcode::Or { dst, a, b } => {
                push_expr!(i, dst, or(expr!(a), expr!(b)));
            }
            &Opcode::Xor { dst, a, b } => {
                push_expr!(i, dst, xor(expr!(a), expr!(b)));
            }
            &Opcode::Neg { dst, src } => {
                push_expr!(i, dst, neg(expr!(src)));
            }
            &Opcode::Not { dst, src } => {
                push_expr!(i, dst, not(expr!(src)));
            }
            &Opcode::Incr { dst } => {
                // FIXME sometimes it should be an expression
                push_stmt!(stmt(incr(expr!(dst))));
            }
            &Opcode::Decr { dst } => {
                push_stmt!(stmt(decr(expr!(dst))));
            }
            //endregion

            //region CALLS
            &Opcode::Call0 { dst, fun } => {
                if fun.ty(code).ret.is_void() {
                    push_stmt!(stmt(call_fun(fun, Vec::new())));
                } else {
                    push_expr!(i, dst, call_fun(fun, Vec::new()));
                }
            }
            &Opcode::Call1 { dst, fun, arg0 } => {
                push_call!(i, dst, fun, arg0)
            }
            &Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                push_call!(i, dst, fun, arg0, arg1)
            }
            &Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                push_call!(i, dst, fun, arg0, arg1, arg2)
            }
            &Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                push_call!(i, dst, fun, arg0, arg1, arg2, arg3)
            }
            Opcode::CallN { dst, fun, args } => {
                if let Some(&ExprCtx::Constructor { reg, pos }) = expr_ctx.last() {
                    if reg == args[0] {
                        push_expr!(
                            pos,
                            reg,
                            Expr::Constructor(ConstructorCall::new(
                                f.regtype(reg),
                                args[1..].iter().map(|x| expr!(x)).collect::<Vec<_>>()
                            ))
                        );
                    }
                } else {
                    let call = call_fun(*fun, args.iter().map(|x| expr!(x)).collect::<Vec<_>>());
                    if fun.ty(code).ret.is_void() {
                        push_stmt!(stmt(call));
                    } else {
                        push_expr!(i, *dst, call);
                    }
                }
            }
            Opcode::CallMethod { dst, field, args } => {
                let call = call(
                    ast::field(expr!(args[0]), f.regtype(args[0]), *field, code),
                    args.iter().skip(1).map(|x| expr!(x)).collect::<Vec<_>>(),
                );
                if f.regtype(args[0])
                    .method(field.0, code)
                    .and_then(|p| p.findex.resolve_as_fn(code))
                    .map(|fun| fun.ty(code).ret.is_void())
                    .unwrap_or(false)
                {
                    push_stmt!(stmt(call));
                } else {
                    push_expr!(i, *dst, call);
                }
            }
            Opcode::CallThis { dst, field, args } => {
                let method = f.regs[0].method(field.0, code).unwrap();
                let call = call(
                    Expr::Field(
                        Box::new(cst_this()),
                        method.name.resolve(&code.strings).to_owned(),
                    ),
                    args.iter().map(|x| expr!(x)).collect::<Vec<_>>(),
                );
                if method
                    .findex
                    .resolve_as_fn(code)
                    .map(|fun| fun.ty(code).ret.is_void())
                    .unwrap_or(false)
                {
                    push_stmt!(stmt(call));
                } else {
                    push_expr!(i, *dst, call);
                }
            }
            Opcode::CallClosure { dst, fun, args } => {
                let call = call(
                    expr!(*fun),
                    args.iter().map(|x| expr!(x)).collect::<Vec<_>>(),
                );
                if f.regtype(*fun)
                    .resolve_as_fun(&code.types)
                    .map(|ty| ty.ret.is_void())
                    .unwrap_or(false)
                {
                    push_stmt!(stmt(call));
                } else {
                    push_expr!(i, *dst, call);
                }
            }
            //endregion

            //region CLOSURES
            &Opcode::StaticClosure { dst, fun } => {
                push_expr!(
                    i,
                    dst,
                    Expr::Closure(
                        fun,
                        decompile_function(code, fun.resolve_as_fn(code).unwrap())
                    )
                );
            }
            &Opcode::InstanceClosure { dst, obj, fun } => {
                push_expr!(
                    i,
                    dst,
                    Expr::Field(
                        Box::new(expr!(obj)),
                        fun.resolve_as_fn(code)
                            .unwrap()
                            .name(code)
                            .unwrap_or("_")
                            .to_owned(),
                    )
                );
            }
            //endregion

            //region ACCESSES
            &Opcode::GetGlobal { dst, global } => {
                // Is a string
                if f.regtype(dst).0 == 13 {
                    push_expr!(
                        i,
                        dst,
                        cst_string(
                            code.globals_initializers
                                .get(&global)
                                .and_then(|&x| {
                                    code.constants.as_ref().map(|constants| {
                                        code.strings[constants[x].fields[0]].to_owned()
                                    })
                                })
                                .unwrap()
                        )
                    );
                } else {
                    match f.regtype(dst).resolve(&code.types) {
                        Type::Obj(obj) | Type::Struct(obj) => {
                            push_expr!(i, dst, Expr::Variable(dst, Some(obj.name.display(code))));
                        }
                        Type::Enum { .. } => {
                            push_expr!(i, dst, Expr::Unknown("unknown enum variant".to_owned()));
                        }
                        _ => {}
                    }
                }
            }
            &Opcode::Field { dst, obj, field } => {
                push_expr!(i, dst, ast::field(expr!(obj), f.regtype(obj), field, code));
            }
            &Opcode::SetField { obj, field, src } => {
                let ctx = expr_ctx.pop();
                // Might be a SetField for an anonymous structure
                if let Some(ExprCtx::Anonymous {
                    pos,
                    mut fields,
                    mut remaining,
                }) = ctx
                {
                    fields.insert(field, expr!(src));
                    remaining -= 1;
                    // If we filled all the structure fields, we emit an expr
                    if remaining == 0 {
                        push_expr!(pos, obj, Expr::Anonymous(f.regtype(obj), fields));
                    } else {
                        expr_ctx.push(ExprCtx::Anonymous {
                            pos,
                            fields,
                            remaining,
                        });
                    }
                } else if let Some(ctx) = ctx {
                    expr_ctx.push(ctx);
                } else {
                    // Otherwise this is just a normal field set
                    push_stmt!(Statement::Assign {
                        declaration: false,
                        variable: ast::field(expr!(obj), f.regtype(obj), field, code),
                        assign: expr!(src),
                    });
                }
            }
            &Opcode::GetThis { dst, field } => {
                push_expr!(i, dst, ast::field(cst_this(), f.regs[0], field, code));
            }
            &Opcode::SetThis { field, src } => {
                push_stmt!(Statement::Assign {
                    declaration: false,
                    variable: ast::field(cst_this(), f.regs[0], field, code),
                    assign: expr!(src),
                });
            }
            /*
            &Opcode::DynGet { dst, obj, field } => {
                // TODO dyn get
            }
            &Opcode::DynSet { obj, field, src } => {
                // TODO dyn set
            }
            &Opcode::EnumIndex { dst, value } => {
                // TODO get enum variant
            }
            &Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                // TODO get enum field
            }
            &Opcode::SetEnumField { value, field, src } => {
                // TODO set enum field
            }*/
            //endregion

            //region CONTROL FLOW
            &Opcode::JTrue { cond, offset } => {
                push_jmp!(i, offset, not(expr!(cond)))
            }
            &Opcode::JFalse { cond, offset } => {
                push_jmp!(i, offset, expr!(cond))
            }
            &Opcode::JNull { reg, offset } => {
                push_jmp!(i, offset, noteq(expr!(reg), cst_null()))
            }
            &Opcode::JNotNull { reg, offset } => {
                push_jmp!(i, offset, eq(expr!(reg), cst_null()))
            }
            &Opcode::JSGte { a, b, offset } | &Opcode::JUGte { a, b, offset } => {
                push_jmp!(i, offset, gt(expr!(b), expr!(a)))
            }
            &Opcode::JSGt { a, b, offset } => {
                push_jmp!(i, offset, gte(expr!(b), expr!(a)))
            }
            &Opcode::JSLte { a, b, offset } => {
                push_jmp!(i, offset, lt(expr!(b), expr!(a)))
            }
            &Opcode::JSLt { a, b, offset } | &Opcode::JULt { a, b, offset } => {
                push_jmp!(i, offset, lte(expr!(b), expr!(a)))
            }
            &Opcode::JEq { a, b, offset } => {
                push_jmp!(i, offset, noteq(expr!(a), expr!(b)))
            }
            &Opcode::JNotEq { a, b, offset } => {
                push_jmp!(i, offset, eq(expr!(a), expr!(b)))
            }
            &Opcode::JAlways { offset } => {
                if offset < 0 {
                    //println!("opcode {i}");
                    // It's either the jump backward of a loop or a continue statement
                    let loop_ = scopes.last_loop().unwrap();
                    // Scan the next instructions in order to find another jump to the same place
                    if f.ops.iter().enumerate().skip(i + 1).find_map(|(j, o)| {
                        // We found another jump to the same place !
                        if matches!(o, Opcode::JAlways {offset} if (j as i32 + offset + 1) as usize == loop_.start) {
                            Some(true)
                        } else {
                            None
                        }
                    }).unwrap_or(false) {
                        // If this jump is not the last jump backward for the current loop,
                        // It's definitely a continue; statement
                        push_stmt!(Statement::Continue);
                    } else {
                        let loop_ = scopes.pop_last_loop().unwrap();
                        // It's the last jump backward of the loop, we generate the loop statement
                        if let Some(cond) = loop_.cond {
                            push_stmt!(Statement::While { cond, stmts: loop_.stmts});
                        } else {
                            push_stmt!(Statement::While {
                                cond: cst_bool(true),
                                stmts: loop_.stmts
                            });
                        }
                    }
                } else {
                    match &scopes.last() {
                        Scope::Switch(switch) => {
                            if let Some(pos) = switch.offsets.iter().position(|o| *o == i) {
                                scopes.push_switch_case(pos);
                            } else {
                                panic!("no offset");
                            }
                        }
                        Scope::SwitchCase(case) => {
                            let len = scopes.scopes.len();
                            let penult = &mut scopes.scopes[len - 2];
                            if let Scope::Switch(switch) = penult {
                                if let Some(pos) = switch.offsets.iter().position(|o| *o == i) {
                                    scopes.push_switch_case(pos);
                                } else {
                                    panic!("no offset");
                                }
                            } else {
                                panic!("wtf");
                            }
                        }
                        Scope::Loop(_) => {
                            // Check the instruction just before the jump target
                            // If it's a jump backward of a loop
                            if matches!(f.ops[(i as i32 + offset) as usize], Opcode::JAlways {offset} if offset < 0)
                            {
                                // It's a break condition
                                push_stmt!(Statement::Break);
                            }
                        }
                        Scope::If { .. } => {
                            // It's the jump over of an else clause
                            scopes.push_else(offset + 1);
                        }
                        _ => {
                            println!("JAlways > 0 with no matching scope ?");
                        }
                    }
                }
            }
            Opcode::Switch { reg, offsets, end } => {
                // Convert to absolute positions
                scopes.push_switch(
                    *end + 1,
                    expr!(reg),
                    offsets.iter().map(|o| i + *o as usize).collect(),
                );
                // The default switch case is implicit
            }
            &Opcode::Label => scopes.push_loop(i),
            &Opcode::Ret { ret } => {
                // Do not display return void; only in case of an early return
                if scopes.depth() > 1 {
                    push_stmt!(Statement::Return(if f.regtype(ret).is_void() {
                        None
                    } else {
                        Some(expr!(ret))
                    }));
                } else if !f.regtype(ret).is_void() {
                    push_stmt!(Statement::Return(Some(expr!(ret))));
                }
            }
            //endregion

            //region CHECKS
            &Opcode::Throw { exc } => {
                push_stmt!(Statement::Throw(expr!(exc)));
            }
            &Opcode::Rethrow { exc } => {
                push_stmt!(Statement::Throw(expr!(exc)));
            }
            &Opcode::Trap { exc, offset } => {
                // TODO try catch
            }
            &Opcode::EndTrap { exc } => {
                // TODO try catch
            }
            //endregion

            //region VALUES
            &Opcode::ToDyn { dst, src } => {
                push_expr!(i, dst, expr!(src));
            }
            &Opcode::ToVirtual { dst, src } => {
                push_expr!(i, dst, expr!(src));
            }
            &Opcode::New { dst } => {
                // Constructor analysis
                let ty = f.regtype(dst).resolve(&code.types);
                match ty {
                    Type::Obj(_) | Type::Struct(_) => {
                        expr_ctx.push(ExprCtx::Constructor { reg: dst, pos: i });
                    }
                    Type::Virtual { fields } => {
                        expr_ctx.push(ExprCtx::Anonymous {
                            pos: i,
                            fields: HashMap::with_capacity(fields.len()),
                            remaining: fields.len(),
                        });
                    }
                    _ => {
                        push_expr!(
                            i,
                            dst,
                            Expr::Constructor(ConstructorCall::new(f.regtype(dst), Vec::new()))
                        );
                    }
                }
            }
            &Opcode::EnumAlloc { dst, construct } => {
                push_expr!(
                    i,
                    dst,
                    Expr::EnumConstr(f.regtype(dst), construct, Vec::new())
                );
            }
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                push_expr!(
                    i,
                    *dst,
                    Expr::EnumConstr(
                        f.regtype(*dst),
                        *construct,
                        args.iter().map(|x| expr!(x)).collect()
                    )
                );
            }
            //endregion
            &Opcode::GetMem { dst, bytes, index } => {
                push_expr!(i, dst, array(expr!(bytes), expr!(index)));
            }
            _ => {}
        }

        // Pass trough all scopes and find which ones should finish
        scopes.push_stmt(statement.take());
    }

    scopes.statements()
}

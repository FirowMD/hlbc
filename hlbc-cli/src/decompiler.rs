use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::{format, Display, Formatter, Write};

use hlbc::opcodes::{JumpOffset, Opcode};
use hlbc::types::{Function, RefField, RefFun, RefGlobal, RefType, Reg, Type, TypeObj};
use hlbc::Bytecode;

pub fn decompile_class(code: &Bytecode, obj: &TypeObj) -> String {
    let mut buf = String::with_capacity(1024);

    let mut is_static = false;
    writeln!(
        &mut buf,
        "class {} {}{{",
        obj.name.display(code),
        if let Some(e) = obj.super_ {
            is_static = e.0 == 12;
            format!("extends {} ", e.display(code))
        } else {
            "".to_string()
        }
    )
    .unwrap();
    let indent = "  ";
    for (i, f) in obj
        .fields
        .iter()
        .enumerate()
        .skip(obj.fields.len() - obj.own_fields.len())
    {
        if obj.bindings.get(&RefField(i)).is_some() {
            continue;
        }
        writeln!(
            &mut buf,
            "{indent}{}var {}: {}",
            if is_static { "static " } else { "" },
            f.name.display(code),
            f.t.display(code)
        )
        .unwrap();
    }
    writeln!(&mut buf, "// BINDINGS").unwrap();

    for (fi, fun) in &obj.bindings {
        //let fi = &obj.fields[fi.0];
        let fun = fun.resolve_as_fn(code).unwrap();
        writeln!(
            &mut buf,
            "{indent}{}{}{}{indent}}}\n",
            if is_static { "static " } else { "" },
            decompile_function_header(code, fun),
            decompile_function_body(code, &format!("{indent}  "), fun)
        )
        .unwrap();
    }

    writeln!(&mut buf, "// METHODS").unwrap();

    for f in &obj.protos {
        let f = f.findex.resolve_as_fn(code).unwrap();
        writeln!(
            &mut buf,
            "{indent}{}{}{indent}}}\n",
            decompile_function_header(code, f),
            decompile_function_body(code, &format!("{indent}  "), f)
        )
        .unwrap();
    }
    writeln!(&mut buf, "}}").unwrap();
    buf
}

pub fn decompile_function_header(code: &Bytecode, f: &Function) -> String {
    let mut buf = String::with_capacity(256);

    write!(&mut buf, "function {}(", f.name.unwrap().display(code)).unwrap();

    match f.t.resolve(&code.types) {
        Type::Fun(fun) => {
            // Skip the first because its a method (this)
            for (i, a) in fun.args.iter().enumerate().skip(1) {
                if i != 1 {
                    write!(&mut buf, ", ").unwrap();
                }
                write!(&mut buf, "reg{}: {}", i, a.display(code)).unwrap();
            }
            writeln!(
                &mut buf,
                "): {} {{ // {}",
                fun.ret.display(code),
                f.findex.0
            )
            .unwrap();
        }
        _ => {
            unreachable!()
        }
    }
    buf
}

pub fn decompile_closure(code: &Bytecode, indent: &str, f: &Function) -> String {
    let mut buf = String::with_capacity(256);

    write!(&mut buf, "(").unwrap();

    match f.t.resolve(&code.types) {
        Type::Fun(fun) => {
            // Skip the first because its a method (this)
            for (i, a) in fun.args.iter().enumerate().skip(1) {
                if i != 1 {
                    write!(&mut buf, ", ").unwrap();
                }
                write!(&mut buf, "reg{}: {}", i, a.display(code)).unwrap();
            }
            writeln!(&mut buf, ") -> {{ // {}", f.findex.0).unwrap();
        }
        _ => {
            unreachable!()
        }
    }
    write!(
        &mut buf,
        "{}",
        decompile_function_body(code, &format!("{indent}  "), f)
    )
    .unwrap();

    writeln!(&mut buf, "{indent}}}").unwrap();
    buf
}

pub fn decompile_function_body(code: &Bytecode, indent: &str, f: &Function) -> String {
    let mut buf = String::with_capacity(256);

    writeln!(&mut buf).unwrap();

    for a in f
        .assigns
        .as_ref()
        .unwrap()
        .iter()
        .map(|(s, i)| format!("{} at opcode {}", s.resolve(&code.strings), i - 1))
    {
        writeln!(&mut buf, "  {a}").unwrap();
    }

    for stmt in make_statements(code, f) {
        stmt.display(&mut buf, code, f).unwrap();
    }

    buf
}

fn make_statements(code: &Bytecode, f: &Function) -> Vec<Statement> {
    // Finished statements
    let mut statements = Vec::with_capacity(f.ops.len());
    // Current iteration statement, to be pushed onto the finished statements or the nesting
    let mut statement = None;
    // Nesting handling
    let mut nesting = Vec::new();
    let mut reg_state = HashMap::new();
    let mut constructor_ctx = None;
    let mut seen = HashSet::new();

    // Initialize register state with the function arguments
    for i in 0..f.ty(code).args.len() {
        reg_state.insert(
            Reg(i as u32),
            Expression::Variable(Reg(i as u32), f.arg_name(code, i)),
        );
    }

    // Create a statement and update the register state (depending on inline rules)
    macro_rules! process_simple_expr {
        ($i:expr, $dst:expr, $e:expr) => {
            let name = f.var_name(code, $i);
            let expr = $e;
            // Inline check
            if name.is_none() {
                reg_state.insert($dst, expr);
            } else {
                reg_state.insert($dst, Expression::Variable($dst, name.clone()));
                if seen.insert(name.clone().unwrap()) {
                    statement = Some(Statement::NewVariable {
                        reg: $dst,
                        name,
                        assign: expr,
                    });
                } else {
                    statement = Some(Statement::Assign {
                        reg: $dst,
                        name,
                        assign: expr,
                    });
                }
            }
        };
    }

    macro_rules! expr {
        ($reg:expr) => {
            reg_state.get(&$reg).expect("No expr for reg ?").clone()
        };
    }

    macro_rules! make_args {
        ($($arg:expr),*) => {
            vec![$(expr!($arg)),*]
        }
    }

    let mut iter = f.ops.iter().enumerate();
    while let Some((i, o)) = iter.next() {
        //println!("ITER");
        match o {
            &Opcode::Int { dst, ptr } => {
                process_simple_expr!(
                    i,
                    dst,
                    Expression::Constant(Constant::Int(ptr.resolve(&code.ints)))
                );
            }
            &Opcode::Float { dst, ptr } => {
                process_simple_expr!(
                    i,
                    dst,
                    Expression::Constant(Constant::Float(ptr.resolve(&code.floats)))
                );
            }
            &Opcode::Bool { dst, value } => {
                process_simple_expr!(i, dst, Expression::Constant(Constant::Bool(value.0)));
            }
            &Opcode::String { dst, ptr } => {
                process_simple_expr!(
                    i,
                    dst,
                    Expression::Constant(Constant::String(ptr.resolve(&code.strings).to_owned()))
                );
            }
            &Opcode::GetGlobal { dst, global } => {
                process_simple_expr!(
                    i,
                    dst,
                    Expression::Constant(Constant::String(
                        global_value_from_constant(code, global).unwrap()
                    ))
                );
            }
            &Opcode::Add { dst, a, b } => {
                process_simple_expr!(i, dst, add(expr!(a), expr!(b)));
            }
            &Opcode::Sub { dst, a, b } => {
                process_simple_expr!(i, dst, sub(expr!(a), expr!(b)));
            }
            &Opcode::Decr { dst } => {
                process_simple_expr!(i, dst, decr(expr!(dst)));
            }
            &Opcode::Incr { dst } => {
                process_simple_expr!(i, dst, incr(expr!(dst)));
            }
            &Opcode::Mov { dst, src } => {
                process_simple_expr!(i, dst, expr!(src));
            }
            &Opcode::New { dst } => {
                // Constructor analysis
                constructor_ctx = Some((dst, i));
            }
            &Opcode::Call0 { dst, fun } => {
                process_simple_expr!(i, dst, Expression::Call(fun, Vec::new()));
            }
            &Opcode::Call1 { dst, fun, arg0 } => {
                if let Some((new, j)) = constructor_ctx {
                    if new == arg0 {
                        process_simple_expr!(
                            j,
                            new,
                            Expression::Constructor(ConstructorCall {
                                ty: f.regtype(new),
                                args: Vec::new()
                            })
                        );
                    }
                } else {
                    process_simple_expr!(i, dst, Expression::Call(fun, make_args!(arg0)));
                }
            }
            &Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                if let Some((new, j)) = constructor_ctx {
                    if new == arg0 {
                        process_simple_expr!(
                            j,
                            new,
                            Expression::Constructor(ConstructorCall {
                                ty: f.regtype(new),
                                args: make_args!(arg1)
                            })
                        );
                    }
                } else {
                    process_simple_expr!(i, dst, Expression::Call(fun, make_args!(arg0, &arg1)));
                }
            }
            &Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                if let Some((new, j)) = constructor_ctx {
                    if new == arg0 {
                        process_simple_expr!(
                            j,
                            new,
                            Expression::Constructor(ConstructorCall {
                                ty: f.regtype(new),
                                args: make_args!(arg1, arg2)
                            })
                        );
                    }
                } else {
                    process_simple_expr!(
                        i,
                        dst,
                        Expression::Call(fun, make_args!(arg0, arg1, arg2))
                    );
                }
            }
            &Opcode::Ret { ret } => {
                // Do not display return void;
                if nesting.len() > 0 {
                    statement = Some(if f.regtype(ret).is_void() {
                        Statement::ReturnVoid
                    } else {
                        Statement::Return { expr: expr!(ret) }
                    });
                } else if !f.regtype(ret).is_void() {
                    statement = Some(Statement::Return { expr: expr!(ret) });
                }
            }
            &Opcode::Label => {
                // We have a loop

                //println!("Increase nesting !");
            }
            &Opcode::JFalse { cond, offset } => {
                if offset > 0 {
                    nesting.push((
                        offset + 1,
                        Statement::If {
                            cond: expr!(cond),
                            stmts: Vec::new(),
                        },
                        Vec::new(),
                    ));
                    //println!("Increase nesting !");
                }
            }
            &Opcode::JTrue { cond, offset } => {
                if offset > 0 {
                    nesting.push((
                        offset + 1,
                        Statement::If {
                            cond: not(expr!(cond)),
                            stmts: Vec::new(),
                        },
                        Vec::new(),
                    ));
                    //println!("Increase nesting !");
                }
            }
            _ => {}
        }

        // Push in the scope if there's one
        if let Some(stmt) = statement.take() {
            if let Some((offset, parent, scope)) = nesting.last_mut() {
                //println!("Push in scope {stmt:?}");
                scope.push(stmt);
            } else {
                //println!("Push in global {stmt:?}");
                statements.push(stmt);
            }
        }
        for idx in (0..nesting.len()).rev() {
            //println!("Update nesting {idx}");
            let (mut len, mut parent, mut scope) = nesting.remove(idx);

            if let Some(stmt) = statement.take() {
                //println!("Push in scope {stmt:?}");
                scope.push(stmt);
            }

            // Decrease scope len
            len -= 1;
            if len <= 0 {
                // End of scope, create a statement
                match &mut parent {
                    Statement::If { stmts, .. } => {
                        *stmts = scope;
                    }
                    _ => {}
                }
                //println!("Decrease nesting {parent:?}");
                statement = Some(parent);
            } else {
                // Scope continues
                nesting.insert(idx, (len, parent, scope));
            }
        }
        if let Some(stmt) = statement.take() {
            if let Some((offset, parent, scope)) = nesting.last_mut() {
                //println!("Push in scope {stmt:?}");
                scope.push(stmt);
            } else {
                //println!("Push in global {stmt:?}");
                statements.push(stmt);
            }
        }
    }

    statements
}

#[derive(Debug, Clone)]
struct ConstructorCall {
    ty: RefType,
    args: Vec<Expression>,
}

#[derive(Debug, Clone)]
enum Constant {
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
enum Operation {
    Not(Box<Expression>),
    Decr(Box<Expression>),
    Incr(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
}

impl Operation {
    fn display(&self, code: &Bytecode) -> String {
        match self {
            Operation::Not(expr) => {
                format!("!{}", expr.display(code))
            }
            Operation::Decr(expr) => {
                format!("{}--", expr.display(code))
            }
            Operation::Incr(expr) => {
                format!("{}++", expr.display(code))
            }
            Operation::Add(e1, e2) => {
                format!("{} + {}", e1.display(code), e2.display(code))
            }
            Operation::Sub(e1, e2) => {
                format!("{} - {}", e1.display(code), e2.display(code))
            }
        }
    }
}

fn not(e: Expression) -> Expression {
    Expression::Op(Operation::Not(Box::new(e)))
}

fn decr(e: Expression) -> Expression {
    Expression::Op(Operation::Decr(Box::new(e)))
}

fn incr(e: Expression) -> Expression {
    Expression::Op(Operation::Incr(Box::new(e)))
}

fn add(e1: Expression, e2: Expression) -> Expression {
    Expression::Op(Operation::Add(Box::new(e1), Box::new(e2)))
}

fn sub(e1: Expression, e2: Expression) -> Expression {
    Expression::Op(Operation::Sub(Box::new(e1), Box::new(e2)))
}

#[derive(Debug, Clone)]
enum Expression {
    Variable(Reg, Option<String>),
    Constant(Constant),
    Constructor(ConstructorCall),
    Call(RefFun, Vec<Expression>),
    Op(Operation),
}

impl Expression {
    fn to_be_inlined(&self) -> bool {
        matches!(self, Expression::Variable(_, _) | Expression::Constant(_))
    }

    fn display(&self, code: &Bytecode) -> String {
        match self {
            Expression::Variable(x, name) => {
                if let Some(name) = name {
                    name.clone()
                } else {
                    format!("{x}")
                }
            }
            Expression::Constant(x) => match x {
                Constant::Int(c) => c.to_string(),
                Constant::Float(c) => c.to_string(),
                Constant::String(c) => format!("\"{c}\""),
                Constant::Bool(c) => c.to_string(),
            },
            Expression::Op(op) => op.display(code),
            Expression::Constructor(ConstructorCall { ty, args }) => {
                format!(
                    "new {}({})",
                    ty.display(code),
                    args.iter()
                        .map(|a| a.display(code))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::Call(fun, args) => {
                format!(
                    "{}({})",
                    fun.display_call(code),
                    args.iter()
                        .map(|a| a.display(code))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Statement {
    NewVariable {
        reg: Reg,
        name: Option<String>,
        assign: Expression,
    },
    Assign {
        reg: Reg,
        name: Option<String>,
        assign: Expression,
    },
    CallVoid {
        fun: RefFun,
        args: Vec<Expression>,
    },
    Return {
        expr: Expression,
    },
    ReturnVoid,
    If {
        cond: Expression,
        stmts: Vec<Statement>,
    },
    Loop {
        cond: Expression,
        stmts: Vec<Statement>,
    },
}

impl Statement {
    fn display(&self, w: &mut impl Write, code: &Bytecode, f: &Function) -> fmt::Result {
        match self {
            Statement::NewVariable { reg, name, assign } => {
                writeln!(
                    w,
                    "var {}: {} = {};",
                    name.as_ref().unwrap_or(&reg.to_string()),
                    f.regtype(*reg).display(code),
                    assign.display(code)
                )?;
            }
            Statement::Assign { reg, name, assign } => {
                writeln!(
                    w,
                    "{} = {};",
                    name.as_ref().unwrap_or(&reg.to_string()),
                    assign.display(code)
                )?;
            }
            Statement::CallVoid { fun, args } => {
                writeln!(
                    w,
                    "{}({})",
                    fun.display_call(code),
                    args.iter()
                        .map(|a| a.display(code))
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
            Statement::Return { expr } => {
                writeln!(w, "return {};", expr.display(code))?;
            }
            Statement::ReturnVoid => {
                writeln!(w, "return;")?;
            }
            Statement::If { cond, stmts } => {
                writeln!(w, "if ({}) {{", cond.display(code))?;
                for stmt in stmts {
                    stmt.display(w, code, f)?;
                }
                writeln!(w, "}}")?;
            }
            Statement::Loop { cond, stmts } => {
                writeln!(w, "while ({}) {{", cond.display(code))?;
                for stmt in stmts {
                    stmt.display(w, code, f)?;
                }
                writeln!(w, "}}")?;
            }
        }
        Ok(())
    }
}

fn global_value_from_constant(code: &Bytecode, global: RefGlobal) -> Option<String> {
    code.globals_initializers.get(&global).and_then(|&x| {
        if let Some(constants) = &code.constants {
            Some(code.strings[constants[x].fields[0]].to_owned())
        } else {
            None
        }
    })
}

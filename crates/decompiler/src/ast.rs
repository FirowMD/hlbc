use std::collections::BTreeMap;

use hlbc::fmt::EnhancedFmt;
use hlbc::opcodes::Opcode;
use hlbc::types::{
    InlineInt, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt,
    RefString, RefType, Reg,
};
use hlbc::{Bytecode, Str};

use crate::diagnostics::Provenance;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub class: Class,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Str,
    pub parent: Option<Str>,
    pub fields: Vec<ClassField>,
    pub methods: Vec<Method>,
}

#[derive(Debug, Clone)]
pub struct ClassField {
    pub name: Str,
    pub ty: RefType,
    pub static_: bool,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub fun: RefFun,
    pub static_: bool,
    pub dynamic: bool,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    InlineInt(usize),
    Int(RefInt),
    Float(RefFloat),
    String(RefString),
    Bool(bool),
    Null,
    /// 'this' instance
    This,
}

/// A byte-pool entry with both its original reference and exact payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BytesLiteral {
    pub reference: RefBytes,
    pub bytes: Vec<u8>,
    pub provenance: Provenance,
}

/// Width and value type used by HashLink's raw memory opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryType {
    U8,
    U16,
    Typed(RefType),
}

#[derive(Debug, Clone)]
pub enum RuntimeCheck {
    Null(Expr),
    Assert,
}

#[derive(Debug, Clone)]
pub enum Operation {
    /// `+`
    Add(Box<Expr>, Box<Expr>),
    /// `-`
    Sub(Box<Expr>, Box<Expr>),
    /// `*`
    Mul(Box<Expr>, Box<Expr>),
    /// `/`
    Div(Box<Expr>, Box<Expr>),
    /// `%`
    Mod(Box<Expr>, Box<Expr>),
    /// `<<`
    Shl(Box<Expr>, Box<Expr>),
    /// `>>`
    Shr(Box<Expr>, Box<Expr>),
    /// && &
    And(Box<Expr>, Box<Expr>),
    /// || |
    Or(Box<Expr>, Box<Expr>),
    /// ^
    Xor(Box<Expr>, Box<Expr>),
    /// \-
    Neg(Box<Expr>),
    /// !
    Not(Box<Expr>),
    /// ++
    Incr(Box<Expr>),
    /// --
    Decr(Box<Expr>),
    /// ==
    Eq(Box<Expr>, Box<Expr>),
    /// !=
    NotEq(Box<Expr>, Box<Expr>),
    /// \>
    Gt(Box<Expr>, Box<Expr>),
    /// \>=
    Gte(Box<Expr>, Box<Expr>),
    /// \<
    Lt(Box<Expr>, Box<Expr>),
    /// \<=
    Lte(Box<Expr>, Box<Expr>),
}

/// Constructor call
#[derive(Debug, Clone)]
pub struct ConstructorCall {
    pub ty: RefType,
    pub args: Vec<Expr>,
}

impl ConstructorCall {
    pub fn new(ty: RefType, args: Vec<Expr>) -> Self {
        Self { ty, args }
    }
}

/// Function or method call
#[derive(Debug, Clone)]
pub struct Call {
    pub fun: Expr,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn new(fun: Expr, args: Vec<Expr>) -> Self {
        Self { fun, args }
    }

    pub fn new_fun(fun: RefFun, args: Vec<Expr>) -> Self {
        Self {
            fun: Expr::FunRef(fun),
            args,
        }
    }
}

/// An expression with a value
#[derive(Debug, Clone)]
pub enum Expr {
    /// An anonymous structure : { field: value }
    Anonymous(RefType, BTreeMap<RefField, Expr>),
    /// Array access : array\[index]
    Array(Box<Expr>, Box<Expr>),
    /// Exact data loaded by the `Bytes` opcode.
    Bytes(BytesLiteral),
    /// Function call
    Call(Box<Call>),
    /// Constant value
    Constant(Constant),
    /// Constructor call
    Constructor(ConstructorCall),
    /// Arrow function (...) -> {...}
    Closure(RefFun, Vec<Statement>),
    EnumConstr(RefType, RefEnumConstruct, Vec<Expr>),
    /// Runtime enum tag used by bytecode switch lowering.
    EnumIndex(Box<Expr>),
    /// Source-level enum switch pattern and its wildcard arity.
    EnumPattern(RefType, RefEnumConstruct, usize),
    /// Field access : obj.field
    Field(Box<Expr>, Str),
    /// Function reference
    FunRef(RefFun),
    /// Raw typed memory load.
    MemoryLoad {
        memory_type: MemoryType,
        bytes: Box<Expr>,
        index: Box<Expr>,
        result_type: RefType,
    },
    /// Runtime type object for a literal bytecode type.
    TypeValue {
        ty: RefType,
        result_type: RefType,
    },
    /// Runtime type object of a value. Null maps to the Void type object.
    RuntimeType {
        value: Box<Expr>,
        result_type: RefType,
    },
    /// HashLink `hl_type_kind` identifier of a runtime type object.
    TypeId {
        value: Box<Expr>,
        result_type: RefType,
    },
    /// A virtually-dispatched method closure with an explicitly captured receiver.
    VirtualClosure {
        receiver: Box<Expr>,
        method: Str,
        target: RefFun,
        callable_type: RefType,
        target_type: RefType,
    },
    /// Address of a VM register. `source` preserves the aliased storage identity.
    Reference {
        source: Reg,
        value: Box<Expr>,
        reference_type: RefType,
    },
    /// Read through a HashLink reference.
    Dereference {
        reference: Box<Expr>,
        result_type: RefType,
    },
    /// Reference to the first element of an array's data area.
    ReferenceData {
        array: Box<Expr>,
        reference_type: RefType,
    },
    /// Offset a reference by `offset * sizeof(element_type)` while retaining aliasing.
    ReferenceOffset {
        reference: Box<Expr>,
        offset: Box<Expr>,
        reference_type: RefType,
        element_type: RefType,
    },
    /// If/Else expression, both branches expressions types must unify (https://haxe.org/manual/expression-if.html)
    IfElse {
        cond: Box<Expr>,
        /// Not empty
        if_: Vec<Statement>,
        /// Not empty
        else_: Vec<Statement>,
    },
    /// Operator
    Op(Operation),
    // For when there should be something, but we don't known what
    Unknown(String),
    /// Variable identifier
    Variable(Reg, Option<Str>),
    /// Original bytecode range for GUI highlighting.
    Provenanced {
        expression: Box<Expr>,
        provenance: Provenance,
    },
}

pub const fn cst_int(cst: RefInt) -> Expr {
    Expr::Constant(Constant::Int(cst))
}

pub const fn cst_float(cst: RefFloat) -> Expr {
    Expr::Constant(Constant::Float(cst))
}

pub const fn cst_bool(cst: bool) -> Expr {
    Expr::Constant(Constant::Bool(cst))
}

pub const fn cst_inline_int(cst: usize) -> Expr {
    Expr::Constant(Constant::InlineInt(cst))
}

pub const fn cst_string(cst: RefString) -> Expr {
    Expr::Constant(Constant::String(cst))
}

pub const fn cst_null() -> Expr {
    Expr::Constant(Constant::Null)
}

pub const fn cst_this() -> Expr {
    Expr::Constant(Constant::This)
}

/// Create a shorthand function to create an expression from an operator
macro_rules! make_op_shorthand {
    ($name:ident, $op:ident, $( $e:ident ),+) => {
        pub(crate) fn $name($( $e: Expr ),+) -> Expr {
            Expr::Op(Operation::$op($( Box::new($e) ),+))
        }
    }
}

make_op_shorthand!(add, Add, e1, e2);
make_op_shorthand!(sub, Sub, e1, e2);
make_op_shorthand!(mul, Mul, e1, e2);
make_op_shorthand!(div, Div, e1, e2);
make_op_shorthand!(modulo, Mod, e1, e2);
make_op_shorthand!(shl, Shl, e1, e2);
make_op_shorthand!(shr, Shr, e1, e2);
make_op_shorthand!(and, And, e1, e2);
make_op_shorthand!(or, Or, e1, e2);
make_op_shorthand!(xor, Xor, e1, e2);
make_op_shorthand!(neg, Neg, e1);
make_op_shorthand!(incr, Incr, e1);
make_op_shorthand!(decr, Decr, e1);
make_op_shorthand!(eq, Eq, e1, e2);
make_op_shorthand!(noteq, NotEq, e1, e2);
make_op_shorthand!(gt, Gt, e1, e2);
make_op_shorthand!(gte, Gte, e1, e2);
make_op_shorthand!(lt, Lt, e1, e2);
make_op_shorthand!(lte, Lte, e1, e2);

/// Invert an expression, will also optimize the expression.
pub fn not(e: Expr) -> Expr {
    use Expr::Op;
    use Operation::*;
    match e {
        Op(Not(a)) => *a,
        Op(Eq(a, b)) => Op(NotEq(a, b)),
        Op(NotEq(a, b)) => Op(Eq(a, b)),
        Op(Gt(a, b)) => Op(Lte(a, b)),
        Op(Gte(a, b)) => Op(Lt(a, b)),
        Op(Lt(a, b)) => Op(Gte(a, b)),
        Op(Lte(a, b)) => Op(Gt(a, b)),
        _ => Op(Not(Box::new(e))),
    }
}

/// Flip the operands of an expression
pub fn flip(e: Expr) -> Expr {
    use Expr::Op;
    use Operation::*;
    match e {
        Op(Add(a, b)) => Op(Add(b, a)),
        Op(Eq(a, b)) => Op(Eq(b, a)),
        Op(NotEq(a, b)) => Op(NotEq(b, a)),
        Op(Gt(a, b)) => Op(Lt(b, a)),
        Op(Gte(a, b)) => Op(Lte(b, a)),
        Op(Lt(a, b)) => Op(Gt(b, a)),
        Op(Lte(a, b)) => Op(Gte(b, a)),
        _ => e,
    }
}

pub fn array(array: Expr, index: Expr) -> Expr {
    Expr::Array(Box::new(array), Box::new(index))
}

pub fn call(fun: Expr, args: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(Call::new(fun, args)))
}

pub fn call_fun(fun: RefFun, args: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(Call::new_fun(fun, args)))
}

pub fn field(expr: Expr, obj: RefType, field: RefField, code: &Bytecode) -> Expr {
    // FIXME meh
    Expr::Field(
        Box::new(expr),
        Str::from(field.display::<EnhancedFmt>(code, &code[obj]).to_string()),
    )
}

#[derive(Debug, Clone)]
pub enum Statement {
    /// Variable assignment
    Assign {
        /// Should 'var' appear
        declaration: bool,
        variable: Expr,
        assign: Expr,
    },
    /// Expression statement
    ExprStatement(Expr),
    /// Write to HashLink's global memory table.
    GlobalStore {
        global: RefGlobal,
        global_type: RefType,
        value: Expr,
    },
    /// Raw typed memory write.
    MemoryStore {
        memory_type: MemoryType,
        bytes: Expr,
        index: Expr,
        value: Expr,
        value_type: RefType,
    },
    /// Write through a reference without replacing the reference register.
    ReferenceStore {
        reference: Expr,
        value: Expr,
        value_type: RefType,
    },
    /// Runtime checks which may throw without corresponding source syntax.
    RuntimeCheck(RuntimeCheck),
    /// Processor cache hint. This has no Haxe-level value.
    Prefetch {
        value: Expr,
        field: RefField,
        mode: InlineInt,
    },
    /// Explicit bytecode no-op retained for semantic/provenance consumers.
    Nop,
    /// Return an expression or nothing (void)
    Return(Option<Expr>),
    /// If/Else statement
    IfElse {
        cond: Expr,
        if_: Vec<Statement>,
        /// Else clause if the vec isn't empty
        else_: Vec<Statement>,
    },
    Switch {
        arg: Expr,
        default: Vec<Statement>,
        /// Several patterns may share one body.
        cases: Vec<(Vec<Expr>, Vec<Statement>)>,
    },
    /// While statement
    While {
        cond: Expr,
        stmts: Vec<Statement>,
    },
    DoWhile {
        cond: Expr,
        stmts: Vec<Statement>,
    },
    Break,
    Continue,
    Throw(Expr),
    Try {
        stmts: Vec<Statement>,
    },
    Catch {
        stmts: Vec<Statement>,
    },
    TryCatch {
        try_stmts: Vec<Statement>,
        catches: Vec<CatchClause>,
    },
    /// Compilable fallback for regions whose edges cannot be represented by
    /// Haxe's unlabelled structured statements.
    StateMachine {
        entry_state: usize,
        locals: Vec<Expr>,
        blocks: Vec<StateMachineBlock>,
    },
    Comment(String),
    /// An opcode with no sound high-level representation.
    UnhandledOpcode {
        opcode: Opcode,
        provenance: Provenance,
    },
    /// Original bytecode range for GUI highlighting.
    Provenanced {
        statement: Box<Statement>,
        provenance: Provenance,
    },
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub variable: Expr,
    pub variable_type: RefType,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ExceptionTransition {
    pub variable: Expr,
    pub variable_types: Vec<RefType>,
    pub handler_state: usize,
}

#[derive(Debug, Clone)]
pub struct StateMachineBlock {
    pub state: usize,
    pub stmts: Vec<Statement>,
    pub terminator: StateTerminator,
    pub exception: Option<ExceptionTransition>,
}

#[derive(Debug, Clone)]
pub enum StateTerminator {
    Goto(usize),
    Branch {
        cond: Expr,
        taken: usize,
        fallthrough: usize,
    },
    Switch {
        arg: Expr,
        cases: Vec<(usize, usize)>,
        default: usize,
    },
    Return(Option<Expr>),
    Throw(Expr),
    Exit,
}

impl Expr {
    pub fn provenance(&self) -> Option<Provenance> {
        match self {
            Expr::Provenanced { provenance, .. } => Some(*provenance),
            _ => None,
        }
    }
}

impl Statement {
    pub fn provenance(&self) -> Option<Provenance> {
        match self {
            Statement::UnhandledOpcode { provenance, .. }
            | Statement::Provenanced { provenance, .. } => Some(*provenance),
            _ => None,
        }
    }
}

pub(crate) fn attach_provenance(statements: &mut [Statement], provenance: Provenance) {
    for statement in statements {
        attach_statement(statement, provenance);
    }
}

fn attach_statement(statement: &mut Statement, provenance: Provenance) {
    match statement {
        Statement::Assign {
            variable, assign, ..
        } => {
            attach_expr(variable, provenance);
            attach_expr(assign, provenance);
        }
        Statement::ExprStatement(expression) | Statement::Throw(expression) => {
            attach_expr(expression, provenance)
        }
        Statement::GlobalStore { value, .. } => attach_expr(value, provenance),
        Statement::MemoryStore {
            bytes,
            index,
            value,
            ..
        } => {
            attach_expr(bytes, provenance);
            attach_expr(index, provenance);
            attach_expr(value, provenance);
        }
        Statement::ReferenceStore {
            reference, value, ..
        } => {
            attach_expr(reference, provenance);
            attach_expr(value, provenance);
        }
        Statement::RuntimeCheck(RuntimeCheck::Null(value)) => attach_expr(value, provenance),
        Statement::RuntimeCheck(RuntimeCheck::Assert) => {}
        Statement::Prefetch { value, .. } => attach_expr(value, provenance),
        Statement::Nop => {}
        Statement::Return(expression) => {
            if let Some(expression) = expression {
                attach_expr(expression, provenance);
            }
        }
        Statement::IfElse { cond, if_, else_ } => {
            attach_expr(cond, provenance);
            attach_provenance(if_, provenance);
            attach_provenance(else_, provenance);
        }
        Statement::Switch {
            arg,
            default,
            cases,
        } => {
            attach_expr(arg, provenance);
            attach_provenance(default, provenance);
            for (patterns, statements) in cases {
                for pattern in patterns {
                    attach_expr(pattern, provenance);
                }
                attach_provenance(statements, provenance);
            }
        }
        Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
            attach_expr(cond, provenance);
            attach_provenance(stmts, provenance);
        }
        Statement::Try { stmts } | Statement::Catch { stmts } => {
            attach_provenance(stmts, provenance)
        }
        Statement::TryCatch { try_stmts, catches } => {
            attach_provenance(try_stmts, provenance);
            for catch in catches {
                attach_expr(&mut catch.variable, provenance);
                attach_provenance(&mut catch.stmts, provenance);
            }
        }
        Statement::StateMachine { locals, blocks, .. } => {
            for local in locals {
                attach_expr(local, provenance);
            }
            for block in blocks {
                attach_provenance(&mut block.stmts, provenance);
                match &mut block.terminator {
                    StateTerminator::Branch { cond, .. } => attach_expr(cond, provenance),
                    StateTerminator::Switch { arg, .. } => attach_expr(arg, provenance),
                    StateTerminator::Return(Some(value)) | StateTerminator::Throw(value) => {
                        attach_expr(value, provenance)
                    }
                    StateTerminator::Goto(_)
                    | StateTerminator::Return(None)
                    | StateTerminator::Exit => {}
                }
                if let Some(exception) = &mut block.exception {
                    attach_expr(&mut exception.variable, provenance);
                }
            }
        }
        Statement::Break | Statement::Continue | Statement::Comment(_) => {}
        Statement::UnhandledOpcode { .. } | Statement::Provenanced { .. } => return,
    }
    let original = std::mem::replace(statement, Statement::Comment(String::new()));
    *statement = Statement::Provenanced {
        statement: Box::new(original),
        provenance,
    };
}

fn attach_expr(expression: &mut Expr, provenance: Provenance) {
    match expression {
        Expr::Anonymous(_, fields) => {
            for value in fields.values_mut() {
                attach_expr(value, provenance);
            }
        }
        Expr::Array(array, index) => {
            attach_expr(array, provenance);
            attach_expr(index, provenance);
        }
        Expr::Bytes(_) => {}
        Expr::Call(call) => {
            attach_expr(&mut call.fun, provenance);
            for argument in &mut call.args {
                attach_expr(argument, provenance);
            }
        }
        Expr::Constructor(call) => {
            for argument in &mut call.args {
                attach_expr(argument, provenance);
            }
        }
        Expr::Closure(_, statements) => attach_provenance(statements, provenance),
        Expr::EnumConstr(_, _, arguments) => {
            for argument in arguments {
                attach_expr(argument, provenance);
            }
        }
        Expr::EnumIndex(value) => attach_expr(value, provenance),
        Expr::EnumPattern(_, _, _) => {}
        Expr::Field(receiver, _) => attach_expr(receiver, provenance),
        Expr::MemoryLoad { bytes, index, .. } => {
            attach_expr(bytes, provenance);
            attach_expr(index, provenance);
        }
        Expr::TypeValue { .. } => {}
        Expr::RuntimeType { value, .. } | Expr::TypeId { value, .. } => {
            attach_expr(value, provenance)
        }
        Expr::VirtualClosure { receiver, .. } => attach_expr(receiver, provenance),
        Expr::Reference { value, .. } => attach_expr(value, provenance),
        Expr::Dereference { reference, .. } => attach_expr(reference, provenance),
        Expr::ReferenceData { array, .. } => attach_expr(array, provenance),
        Expr::ReferenceOffset {
            reference, offset, ..
        } => {
            attach_expr(reference, provenance);
            attach_expr(offset, provenance);
        }
        Expr::IfElse { cond, if_, else_ } => {
            attach_expr(cond, provenance);
            attach_provenance(if_, provenance);
            attach_provenance(else_, provenance);
        }
        Expr::Op(operation) => attach_operation(operation, provenance),
        Expr::Constant(_) | Expr::FunRef(_) | Expr::Unknown(_) | Expr::Variable(_, _) => {}
        Expr::Provenanced { .. } => return,
    }
    let original = std::mem::replace(expression, Expr::Unknown(String::new()));
    *expression = Expr::Provenanced {
        expression: Box::new(original),
        provenance,
    };
}

fn attach_operation(operation: &mut Operation, provenance: Provenance) {
    use Operation::*;
    match operation {
        Add(a, b)
        | Sub(a, b)
        | Mul(a, b)
        | Div(a, b)
        | Mod(a, b)
        | Shl(a, b)
        | Shr(a, b)
        | And(a, b)
        | Or(a, b)
        | Xor(a, b)
        | Eq(a, b)
        | NotEq(a, b)
        | Gt(a, b)
        | Gte(a, b)
        | Lt(a, b)
        | Lte(a, b) => {
            attach_expr(a, provenance);
            attach_expr(b, provenance);
        }
        Neg(expression) | Not(expression) | Incr(expression) | Decr(expression) => {
            attach_expr(expression, provenance)
        }
    }
}

/// Create an expression statement
pub fn stmt(e: Expr) -> Statement {
    Statement::ExprStatement(e)
}

pub fn comment(comment: impl Into<String>) -> Statement {
    Statement::Comment(comment.into())
}

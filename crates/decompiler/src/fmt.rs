use std::fmt;
use std::fmt::{Display, Formatter};

use hlbc::fmt::{BytecodeFmt, EnhancedFmt};
use hlbc::types::{Function, RefField, Type};
use hlbc::Str;
use hlbc::{Bytecode, Resolve};

use crate::ast::{
    Class, Constant, ConstructorCall, Expr, MemoryType, Method, Operation, RuntimeCheck,
    StateTerminator, Statement,
};

impl MemoryType {
    fn intrinsic_name(self) -> String {
        match self {
            Self::U8 => "u8".to_owned(),
            Self::U16 => "u16".to_owned(),
            Self::Typed(ty) => format!("t{}", ty.0),
        }
    }
}

#[derive(Clone)]
pub struct FormatOptions {
    indent: usize,
    inc_indent: usize,
}

impl FormatOptions {
    pub fn new(inc_indent: usize) -> Self {
        Self {
            indent: 0,
            inc_indent,
        }
    }

    pub fn inc_nesting(&self) -> Self {
        FormatOptions {
            indent: self.indent.saturating_add(self.inc_indent),
            ..*self
        }
    }
}

impl Display for FormatOptions {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for _ in 0..self.indent {
            f.write_str(" ")?;
        }
        Ok(())
    }
}

fn to_haxe_type<'a>(ty: &Type, ctx: &'a Bytecode) -> impl Display + 'a {
    use crate::Type::*;
    match ty {
        Void => Str::from_static("Void"),
        I32 => Str::from_static("Int"),
        F64 => Str::from_static("Float"),
        Bool => Str::from_static("Bool"),
        Bytes => Str::from_static("hl.Bytes"),
        Dyn => Str::from_static("Dynamic"),
        Fun(_) => Str::from_static("Function"),
        Obj(obj) => ctx.get(obj.name),
        _ => Str::from_static("other"),
    }
}

impl Class {
    pub fn display<'a>(&'a self, ctx: &'a Bytecode, opts: &'a FormatOptions) -> impl Display + 'a {
        let new_opts = opts.inc_nesting();
        fmtools::fmt! { move
            {opts}"class "{self.name} if let Some(parent) = self.parent.as_ref() { " extends "{parent} } " {\n"
            for f in &self.fields {
                {new_opts} if f.static_ { "static " } "var "{f.name}": "{to_haxe_type(&ctx[f.ty], ctx)}";\n"
            }
            for m in &self.methods {
                "\n"
                {m.display(ctx, &new_opts)}
            }
            {opts}"}"
        }
    }
}

impl Method {
    pub fn display<'a>(&'a self, ctx: &'a Bytecode, opts: &'a FormatOptions) -> impl Display + 'a {
        let new_opts = opts.inc_nesting();
        fmtools::fmt! { move
            match self.fun.as_fn(ctx) {
                Some(fun) => {
                    {opts} if self.static_ { "static " } if self.dynamic { "dynamic " }
                    "function "{fun.name(ctx)}"("
                    {fmtools::join(", ", fun.args(ctx).iter().enumerate().skip(if self.static_ { 0 } else { 1 })
                        .map(move |(i, arg)| fmtools::fmt! {move
                            {fun.arg_name(ctx, i).unwrap_or(Str::from("_"))}": "{to_haxe_type(&ctx[*arg], ctx)}
                        }))}
                    ")" if !fun.ty(ctx).ret.is_void() { ": "{to_haxe_type(fun.ret(ctx), ctx)} } " {"

                    if self.statements.is_empty() {
                        "}"
                    } else {
                        "\n"
                        for stmt in &self.statements {
                            {new_opts}{stmt.display(&new_opts, ctx, fun)}"\n"
                        }
                        {opts}"}"
                    }
                    "\n"
                }
                None => {{opts}"// invalid function reference "{self.fun.0}"\n"}
            }
        }
    }
}

impl Constant {
    fn fmt(&self, f: &mut Formatter, code: &Bytecode) -> fmt::Result {
        use Constant::*;
        match *self {
            InlineInt(c) => Display::fmt(&c, f),
            Int(c) => EnhancedFmt.fmt_refint(f, code, c),
            Float(c) => EnhancedFmt.fmt_reffloat(f, code, c),
            String(c) => {
                write!(f, "\"{}\"", code[c])
            }
            Bool(c) => Display::fmt(&c, f),
            Null => f.write_str("null"),
            This => f.write_str("this"),
        }
    }
}

impl Operation {
    pub fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        f: &'a Function,
    ) -> impl Display + 'a {
        use Operation::*;
        macro_rules! disp {
            ($e:ident) => {
                $e.display(indent, code, f)
            };
        }
        fmtools::fmt! { move
            match self {
                Add(e1, e2) => {{disp!(e1)}" + "{disp!(e2)}}
                Sub(e1, e2) => {{disp!(e1)}" - "{disp!(e2)}}
                Mul(e1, e2) => {{disp!(e1)}" * "{disp!(e2)}}
                Div(e1, e2) => {{disp!(e1)}" / "{disp!(e2)}}
                Mod(e1, e2) => {{disp!(e1)}" % "{disp!(e2)}}
                Shl(e1, e2) => {{disp!(e1)}" << "{disp!(e2)}}
                Shr(e1, e2) => {{disp!(e1)}" >> "{disp!(e2)}}
                And(e1, e2) => {{disp!(e1)}" && "{disp!(e2)}}
                Or(e1, e2) => {{disp!(e1)}" || "{disp!(e2)}}
                Xor(e1, e2) => {{disp!(e1)}" ^ "{disp!(e2)}}
                Neg(expr) => {"-"{disp!(expr)}}
                Not(expr) => {"!"{disp!(expr)}}
                Incr(expr) => {{disp!(expr)}"++"}
                Decr(expr) => {{disp!(expr)}"--"}
                Eq(e1, e2) => {{disp!(e1)}" == "{disp!(e2)}}
                NotEq(e1, e2) => {{disp!(e1)}" != "{disp!(e2)}}
                Gt(e1, e2) => {{disp!(e1)}" > "{disp!(e2)}}
                Gte(e1, e2) => {{disp!(e1)}" >= "{disp!(e2)}}
                Lt(e1, e2) => {{disp!(e1)}" < "{disp!(e2)}}
                Lte(e1, e2) => {{disp!(e1)}" <= "{disp!(e2)}}
            }
        }
    }
}

impl Expr {
    pub fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        f: &'a Function,
    ) -> impl Display + 'a {
        macro_rules! disp {
            ($e:expr) => {
                $e.display(indent, code, f)
            };
        }
        fmtools::fmt! { move
            match self {
                Expr::Anonymous(ty, values) => match &code[*ty] {
                    Type::Virtual { fields } => {
                        "{"{ fmtools::join(", ", fields
                            .iter()
                            .enumerate()
                            .map(|(i, f)| {
                                fmtools::fmt! { move
                                    {f.name(code)}": "
                                    if let Some(value) = values.get(&RefField(i)) { {disp!(value)} } else { "null" }
                                }
                            })) }"}"
                    }
                    _ => "[invalid anonymous type]",
                },
                Expr::Array(array, index) => {
                    {disp!(array)}"["{disp!(index)}"]"
                }
                Expr::Bytes(literal) => {
                    "__hl_bytes("{literal.reference.0}", \""
                    {literal.bytes.iter().map(|byte| format!("{byte:02x}")).collect::<String>()}
                    "\")"
                }
                Expr::Call(call) => {
                    {disp!(call.fun)}"("{fmtools::join(", ", call.args.iter().map(|e| disp!(e)))}")"
                }
                Expr::Constant(c) => {|f| c.fmt(f, code)?;},
                Expr::Constructor(ConstructorCall { ty, args }) => {
                    "new "{ty.display::<EnhancedFmt>(code)}"("{fmtools::join(", ", args.iter().map(|e| disp!(e)))}")"
                }
                Expr::Closure(f, stmts) => {
                    match f.as_fn(code) {
                        Some(fun) => {
                            "("{fmtools::join(", ", fun.ty(code).args.iter().enumerate().map(move |(i, arg)|
                                fmtools::fmt! { move
                                    {fun.arg_name(code, i).unwrap_or(Str::from("_"))}": "{to_haxe_type(&code[*arg], code)}
                                }
                            ))}") -> {\n"
                            let indent2 = indent.inc_nesting();
                            for stmt in stmts {
                                {indent2}{stmt.display(&indent2, code, fun)}"\n"
                            }
                            {indent}"}"
                        }
                        None => "[invalid closure function]"
                    }
                }
                Expr::EnumConstr(ty, constr, args) => {
                    {constr.display::<EnhancedFmt>(code, &code[*ty])}"("{fmtools::join(", ", args.iter().map(|e| disp!(e)))}")"
                }
                Expr::EnumIndex(value) => {
                    "Type.enumIndex("{disp!(value)}")"
                }
                Expr::EnumPattern(ty, constr, arity) => {
                    {constr.display::<EnhancedFmt>(code, &code[*ty])}
                    if *arity > 0 {
                        "("{fmtools::join(", ", (0..*arity).map(|_| "_"))}")"
                    }
                }
                Expr::Field(receiver, name) => {
                    {disp!(receiver)}"."{name}
                }
                Expr::FunRef(fun) => {{fun.name(code)}},
                Expr::MemoryLoad { memory_type, bytes, index, .. } => {
                    "__hl_mem_get_"{memory_type.intrinsic_name()}"("{disp!(bytes)}", "{disp!(index)}")"
                }
                Expr::TypeValue { ty, .. } => {
                    "__hl_type("{ty.0}" /* "{ty.display::<EnhancedFmt>(code)}" */)"
                }
                Expr::RuntimeType { value, .. } => {
                    "__hl_get_type("{disp!(value)}")"
                }
                Expr::TypeId { value, .. } => {
                    "__hl_type_id("{disp!(value)}")"
                }
                Expr::VirtualClosure { receiver, method, .. } => {
                    {disp!(receiver)}"."{method}
                }
                Expr::Reference { source, value, .. } => {
                    "__hl_ref("{disp!(value)}" /* r"{source.0}" */)"
                }
                Expr::Dereference { reference, .. } => {
                    "__hl_deref("{disp!(reference)}")"
                }
                Expr::ReferenceData { array, .. } => {
                    "__hl_array_data("{disp!(array)}")"
                }
                Expr::ReferenceOffset { reference, offset, .. } => {
                    "__hl_ref_offset("{disp!(reference)}", "{disp!(offset)}")"
                }
                Expr::IfElse { cond, if_, else_ } => {
                    "if ("{disp!(cond)}") {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in if_ {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"} else {\n"
                    for stmt in else_ {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                }
                Expr::Op(op) => {{disp!(op)}},
                Expr::Unknown(msg) => {
                     "(cast null : Dynamic) /* "{msg}" */"
                }
                Expr::Variable(x, name) => {{
                    if let Some(name) = name {
                        name.clone()
                    } else {
                        Str::from(x.to_string())
                    }
                }}
                Expr::Provenanced { expression, .. } => {{disp!(expression)}}
            }
        }
    }
}

impl Statement {
    pub fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        f: &'a Function,
    ) -> impl Display + 'a {
        macro_rules! disp {
            ($e:expr) => {
                $e.display(indent, code, f)
            };
        }
        fmtools::fmt! { move
            match self {
                Statement::Assign {
                    declaration,
                    variable,
                    assign,
                } => {
                    if *declaration { "var " } else { "" }{disp!(variable)}" = "{disp!(assign)}";"
                }
                Statement::ExprStatement(expr) => {
                    {disp!(expr)}";"
                }
                Statement::GlobalStore { global, global_type, value } => {
                    "__hl_set_global("{global.0}", "{disp!(value)}"); // t"{global_type.0}
                }
                Statement::MemoryStore { memory_type, bytes, index, value, .. } => {
                    "__hl_mem_set_"{memory_type.intrinsic_name()}"("{disp!(bytes)}", "
                    {disp!(index)}", "{disp!(value)}");"
                }
                Statement::ReferenceStore { reference, value, .. } => {
                    "__hl_ref_set("{disp!(reference)}", "{disp!(value)}");"
                }
                Statement::RuntimeCheck(RuntimeCheck::Null(value)) => {
                    "// hl.NullCheck("{disp!(value)}"): may throw"
                }
                Statement::RuntimeCheck(RuntimeCheck::Assert) => {
                    "// hl.Assert: debugger break followed by an assertion exception"
                }
                Statement::Prefetch { value, field, mode } => {
                    "// hl.Prefetch(value="{disp!(value)}", field="{field.0}", mode="{mode}")"
                }
                Statement::Nop => {
                    "// hl.Nop"
                }
                Statement::Return(expr) => {
                    "return" if let Some(e) = expr { " "{disp!(e)} } ";"
                }
                Statement::IfElse { cond, if_, else_ } => {
                    "if ("{disp!(cond)}") {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in if_ {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                    if !else_.is_empty() {
                        if else_.len() == 1 && statement_is_if(&else_[0]) {
                            " else "{else_[0].display(indent, code, f)}
                        } else {
                            " else {\n"
                            for stmt in else_ {
                                {indent2}{stmt.display(&indent2, code, f)}"\n"
                            }
                            {indent}"}"
                        }
                    }
                }
                Statement::Switch {arg, default, cases} => {
                    "switch ("{disp!(arg)}") {\n"
                    let indent2 = indent.inc_nesting();
                    let indent3 = indent2.inc_nesting();
                    {indent2}"default:\n"
                    if default.is_empty() {
                        {indent3}"{}\n"
                    } else {
                        for stmt in default {
                            {indent3}{stmt.display(&indent3, code, f)}"\n"
                        }
                    }
                    for (patterns, stmts) in cases {
                        {indent2}"case "
                        {fmtools::join(", ", patterns.iter().map(move |pattern| pattern.display(indent, code, f)))}
                        ":\n"
                        for stmt in stmts {
                            {indent3}{stmt.display(&indent3, code, f)}"\n"
                        }
                    }
                    {indent}"}"
                }
                Statement::While { cond, stmts } => {
                    "while ("{disp!(cond)}") {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in stmts {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                }
                Statement::DoWhile { cond, stmts } => {
                    "do {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in stmts {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"} while ("{disp!(cond)}");"
                }
                Statement::Break => {
                    "break;"
                }
                Statement::Continue => {
                    "continue;"
                }
                Statement::Throw(exc) => {
                    "throw "{disp!(exc)}";"
                }
                Statement::Try { stmts } => {
                    "try {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in stmts {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                }
                Statement::Catch { stmts } => {
                    "catch () {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in stmts {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                }
                Statement::TryCatch { try_stmts, catches } => {
                    "try {\n"
                    let indent2 = indent.inc_nesting();
                    for stmt in try_stmts {
                        {indent2}{stmt.display(&indent2, code, f)}"\n"
                    }
                    {indent}"}"
                    for catch in catches {
                        " catch ("{catch.variable.display(indent, code, f)}": "
                        {to_haxe_type(&code[catch.variable_type], code)}") {\n"
                        for stmt in &catch.stmts {
                            {indent2}{stmt.display(&indent2, code, f)}"\n"
                        }
                        {indent}"}"
                    }
                }
                Statement::StateMachine { entry_state, locals, blocks } => {
                    for local in locals {
                        "var "{local.display(indent, code, f)}": Dynamic = null;\n"
                        {indent}
                    }
                    "var __hl_state = "{entry_state}";\n"
                    {indent}"var __hl_running = true;\n"
                    {indent}"while (__hl_running) {\n"
                    let indent2 = indent.inc_nesting();
                    let indent3 = indent2.inc_nesting();
                    let indent4 = indent3.inc_nesting();
                    {indent2}"switch (__hl_state) {\n"
                    for block in blocks {
                        {indent3}"case "{block.state}":\n"
                        if let Some(exception) = &block.exception {
                            {indent4}"try {\n"
                            let indent5 = indent4.inc_nesting();
                            for stmt in &block.stmts {
                                {indent5}{stmt.display(&indent5, code, f)}"\n"
                            }
                            {indent5}{block.terminator.display(&indent5, code, f)}"\n"
                            {indent4}"}"
                            for variable_type in &exception.variable_types {
                                " catch (__hl_caught_"{block.state}": "
                                {to_haxe_type(&code[*variable_type], code)}") {\n"
                                {indent5}{exception.variable.display(indent, code, f)}" = __hl_caught_"{block.state}";\n"
                                {indent5}"__hl_state = "{exception.handler_state}";\n"
                                {indent4}"}"
                            }
                            "\n"
                        } else {
                            for stmt in &block.stmts {
                                {indent4}{stmt.display(&indent4, code, f)}"\n"
                            }
                            {indent4}{block.terminator.display(&indent4, code, f)}"\n"
                        }
                    }
                    {indent3}"default:\n"
                    {indent4}"__hl_running = false;\n"
                    {indent2}"}\n"
                    {indent}"}"
                }
                Statement::Comment(comment) => {
                    "// "{comment}
                }
                Statement::UnhandledOpcode { opcode, provenance } => {
                    "// unsupported opcode f"{provenance.function_index}" @"
                    {provenance.opcode_start}": "{opcode.name()}" "{format!("{:?}", opcode)}
                }
                Statement::Provenanced { statement, .. } => {
                    {statement.display(indent, code, f)}
                }
            }
        }
    }
}

impl StateTerminator {
    fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        f: &'a Function,
    ) -> impl Display + 'a {
        macro_rules! disp {
            ($e:expr) => {
                $e.display(indent, code, f)
            };
        }
        fmtools::fmt! { move
            match self {
                StateTerminator::Goto(state) => {
                    "__hl_state = "{state}";"
                }
                StateTerminator::Branch { cond, taken, fallthrough } => {
                    "if ("{disp!(cond)}") {\n"
                    let indent2 = indent.inc_nesting();
                    {indent2}"__hl_state = "{taken}";\n"
                    {indent}"} else {\n"
                    {indent2}"__hl_state = "{fallthrough}";\n"
                    {indent}"}"
                }
                StateTerminator::Switch { arg, cases, default } => {
                    "switch ("{disp!(arg)}") {\n"
                    let indent2 = indent.inc_nesting();
                    for (case, state) in cases {
                        {indent2}"case "{case}": __hl_state = "{state}";\n"
                    }
                    {indent2}"default: __hl_state = "{default}";\n"
                    {indent}"}"
                }
                StateTerminator::Return(value) => {
                    "return" if let Some(value) = value { " "{disp!(value)} } ";"
                }
                StateTerminator::Throw(value) => {
                    "throw "{disp!(value)}";"
                }
                StateTerminator::Exit => {
                    "__hl_running = false;"
                }
            }
        }
    }
}

fn statement_is_if(mut statement: &Statement) -> bool {
    loop {
        match statement {
            Statement::IfElse { .. } => return true,
            Statement::Provenanced {
                statement: inner, ..
            } => statement = inner,
            _ => return false,
        }
    }
}

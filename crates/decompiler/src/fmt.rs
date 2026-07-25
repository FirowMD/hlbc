use std::collections::BTreeSet;
use std::fmt;
use std::fmt::{Display, Formatter, Write};

use hlbc::types::{FunPtr, Function, RefType, Type, TypeFun};
use hlbc::{Bytecode, Resolve};

use crate::ast::{
    CallKind, Class, Constant, ConstructorCall, Expr, MemoryType, Method, Operation, RuntimeCheck,
    SourceFile, StateTerminator, Statement, StringPart,
};

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
        Self {
            indent: self.indent.saturating_add(self.inc_indent),
            ..self.clone()
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

/// A syntactically valid Haxe representation of every HashLink type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HaxeType {
    Void,
    Int,
    I64,
    F32,
    Float,
    Bool,
    Bytes,
    Dynamic,
    RuntimeType,
    Named(String),
    Function(Vec<HaxeType>, Box<HaxeType>),
    Array(Box<HaxeType>),
    NativeArray(Box<HaxeType>),
    Reference(Box<HaxeType>),
    Anonymous(Vec<(String, HaxeType)>),
    Nullable(Box<HaxeType>),
    /// A source-only HashLink wrapper such as `packed`.
    Wrapped(Box<HaxeType>),
    /// The VM type has no public Haxe name. The diagnostic is retained in the
    /// model while formatting falls back to the explicit `Dynamic` type.
    Diagnostic(String),
}

impl Display for HaxeType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => f.write_str("Void"),
            Self::Int => f.write_str("Int"),
            Self::I64 => f.write_str("hl.I64"),
            Self::F32 => f.write_str("hl.F32"),
            Self::Float => f.write_str("Float"),
            Self::Bool => f.write_str("Bool"),
            Self::Bytes => f.write_str("hl.Bytes"),
            Self::Dynamic => f.write_str("Dynamic"),
            Self::RuntimeType => f.write_str("hl.Type"),
            Self::Named(name) => f.write_str(name),
            Self::Function(arguments, result) => {
                f.write_str("(")?;
                for (index, argument) in arguments.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(argument, f)?;
                }
                write!(f, ") -> {result}")
            }
            Self::Array(element) => write!(f, "Array<{element}>"),
            Self::NativeArray(element) => write!(f, "hl.NativeArray<{element}>"),
            Self::Reference(inner) => write!(f, "hl.Ref<{inner}>"),
            Self::Anonymous(fields) => {
                f.write_str("{ ")?;
                for (index, (name, ty)) in fields.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{name}: {ty}")?;
                }
                f.write_str(" }")
            }
            Self::Nullable(inner) => write!(f, "Null<{inner}>"),
            Self::Wrapped(inner) => Display::fmt(inner, f),
            Self::Diagnostic(message) => write!(f, "Dynamic /* {} */", escape_comment(message)),
        }
    }
}

/// Map a bytecode type reference to a valid, explicit Haxe type.
pub fn haxe_type(code: &Bytecode, reference: RefType) -> HaxeType {
    haxe_type_inner(code, reference, &mut BTreeSet::new())
}

fn haxe_type_inner(code: &Bytecode, reference: RefType, active: &mut BTreeSet<usize>) -> HaxeType {
    if reference.0 == 13 {
        return HaxeType::Named("String".to_owned());
    }
    if !active.insert(reference.0) {
        return HaxeType::Diagnostic(format!("recursive HashLink type t{}", reference.0));
    }
    let Some(ty) = code.types.get(reference.0) else {
        active.remove(&reference.0);
        return HaxeType::Diagnostic(format!("invalid HashLink type t{}", reference.0));
    };
    let result = haxe_type_value(code, ty, active);
    active.remove(&reference.0);
    result
}

fn haxe_type_value(code: &Bytecode, ty: &Type, active: &mut BTreeSet<usize>) -> HaxeType {
    match ty {
        Type::Void => HaxeType::Void,
        Type::UI8 | Type::UI16 | Type::I32 => HaxeType::Int,
        Type::I64 => HaxeType::I64,
        Type::F32 => HaxeType::F32,
        Type::F64 => HaxeType::Float,
        Type::Bool => HaxeType::Bool,
        Type::Bytes => HaxeType::Bytes,
        Type::Dyn | Type::DynObj => HaxeType::Dynamic,
        Type::Fun(signature) | Type::Method(signature) => {
            haxe_function_type(code, signature, active)
        }
        Type::Obj(object) | Type::Struct(object) => {
            let name = code.get(object.name);
            haxe_named_type(name.as_str())
        }
        Type::Array => HaxeType::NativeArray(Box::new(HaxeType::Dynamic)),
        Type::Type => HaxeType::RuntimeType,
        Type::Ref(inner) => HaxeType::Reference(Box::new(haxe_type_inner(code, *inner, active))),
        Type::Virtual { fields } => HaxeType::Anonymous(
            fields
                .iter()
                .map(|field| {
                    (
                        escape_identifier(field.name(code).as_str()),
                        haxe_type_inner(code, field.t, active),
                    )
                })
                .collect(),
        ),
        Type::Abstract { name } => {
            let name = code.get(*name);
            haxe_abstract_type(name.as_str())
        }
        Type::Enum { name, .. } => HaxeType::Named(source_type_name(code.get(*name).as_str())),
        Type::Null(inner) => HaxeType::Nullable(Box::new(haxe_type_inner(code, *inner, active))),
        Type::Packed(inner) => HaxeType::Wrapped(Box::new(haxe_type_inner(code, *inner, active))),
    }
}

fn haxe_function_type(
    code: &Bytecode,
    signature: &TypeFun,
    active: &mut BTreeSet<usize>,
) -> HaxeType {
    HaxeType::Function(
        signature
            .args
            .iter()
            .map(|argument| haxe_type_inner(code, *argument, active))
            .collect(),
        Box::new(haxe_type_inner(code, signature.ret, active)),
    )
}

fn haxe_default_value(code: &Bytecode, reference: RefType) -> &'static str {
    match code.types.get(reference.0) {
        Some(Type::UI8 | Type::UI16 | Type::I32 | Type::I64) => "0",
        Some(Type::F32 | Type::F64) => "0.0",
        Some(Type::Bool) => "false",
        _ => "null",
    }
}

fn haxe_named_type(name: &str) -> HaxeType {
    match name {
        "String" => HaxeType::Named("String".to_owned()),
        name if name.starts_with("hl.types.Array") => haxe_array_wrapper_type(name),
        name if name.starts_with("hl.NativeArrayIterator_") => {
            HaxeType::Diagnostic(format!("internal runtime type {name}"))
        }
        name if name.is_empty() || name == "<none>" || name == "dynobj" => {
            HaxeType::Diagnostic(format!("unnamed HashLink object type {name:?}"))
        }
        name => HaxeType::Named(source_type_name(name)),
    }
}

fn haxe_abstract_type(name: &str) -> HaxeType {
    match name {
        "i64" => HaxeType::I64,
        "f32" => HaxeType::F32,
        name if name.starts_with("hl.types.Array") => haxe_array_wrapper_type(name),
        "hl_symbol" => HaxeType::Diagnostic("internal HashLink symbol type".to_owned()),
        name if !name.is_empty() && name != "<none>" => HaxeType::Named(source_type_name(name)),
        name => HaxeType::Diagnostic(format!("unnamed HashLink abstract type {name:?}")),
    }
}

fn haxe_array_wrapper_type(name: &str) -> HaxeType {
    let element = if name.ends_with("_Int") {
        HaxeType::Int
    } else if name.ends_with("_Float") {
        HaxeType::Float
    } else if name.ends_with("_hl_F32") {
        HaxeType::F32
    } else if name.ends_with("_hl_UI16") {
        HaxeType::Int
    } else {
        HaxeType::Dynamic
    };
    HaxeType::Array(Box::new(element))
}

pub fn escape_identifier(identifier: &str) -> String {
    let mut escaped = String::with_capacity(identifier.len().max(1));
    for (index, character) in identifier.chars().enumerate() {
        let valid = if index == 0 {
            character == '_' || character.is_alphabetic()
        } else {
            character == '_' || character.is_alphanumeric()
        };
        if valid {
            escaped.push(character);
        } else {
            escaped.push('_');
        }
    }
    if escaped.is_empty() {
        escaped.push('_');
    }
    if is_haxe_keyword(&escaped) {
        escaped.push('_');
    }
    escaped
}

pub fn escape_type_path(path: &str) -> String {
    path.split('.')
        .filter(|part| !part.is_empty())
        .map(escape_identifier)
        .collect::<Vec<_>>()
        .join(".")
}

fn simple_type_name(path: &str) -> String {
    escape_identifier(path.rsplit('.').next().unwrap_or(path))
}

fn source_type_name(path: &str) -> String {
    if should_emit_supporting_type(path) {
        simple_type_name(path)
    } else {
        escape_type_path(path)
    }
}

fn is_haxe_keyword(identifier: &str) -> bool {
    matches!(
        identifier,
        "abstract"
            | "break"
            | "case"
            | "cast"
            | "catch"
            | "class"
            | "continue"
            | "default"
            | "do"
            | "dynamic"
            | "else"
            | "enum"
            | "extends"
            | "extern"
            | "false"
            | "final"
            | "for"
            | "from"
            | "function"
            | "if"
            | "implements"
            | "import"
            | "in"
            | "inline"
            | "interface"
            | "macro"
            | "new"
            | "null"
            | "operator"
            | "overload"
            | "override"
            | "package"
            | "private"
            | "public"
            | "return"
            | "static"
            | "switch"
            | "this"
            | "throw"
            | "to"
            | "true"
            | "try"
            | "typedef"
            | "untyped"
            | "using"
            | "var"
            | "while"
    )
}

pub fn escape_haxe_string(value: &str, quote: char) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            '\u{0008}' => escaped.push_str("\\b"),
            '\u{000c}' => escaped.push_str("\\f"),
            '\0' => escaped.push_str("\\x00"),
            '"' if quote == '"' => escaped.push_str("\\\""),
            '\'' if quote == '\'' => escaped.push_str("\\'"),
            '$' if quote == '\'' => escaped.push_str("$$"),
            character
                if character.is_control() || character == '\u{2028}' || character == '\u{2029}' =>
            {
                write!(&mut escaped, "\\u{{{:x}}}", character as u32).unwrap();
            }
            character => escaped.push(character),
        }
    }
    escaped
}

fn escape_comment(value: &str) -> String {
    value
        .chars()
        .map(|character| match character {
            '\r' | '\n' => ' ',
            character => character,
        })
        .collect::<String>()
        .replace("*/", "* /")
}

impl Class {
    pub fn display<'a>(&'a self, code: &'a Bytecode, opts: &'a FormatOptions) -> impl Display + 'a {
        ClassDisplay {
            class: self,
            code,
            opts,
            project_types: None,
        }
    }

    /// Format a class for a project which owns `project_types` in separate
    /// source files. Those declarations are not repeated as local stubs, while
    /// native/free-function/runtime helpers remain module-local and compilable.
    pub fn display_for_project<'a>(
        &'a self,
        code: &'a Bytecode,
        opts: &'a FormatOptions,
        project_types: &'a BTreeSet<usize>,
    ) -> impl Display + 'a {
        ClassDisplay {
            class: self,
            code,
            opts,
            project_types: Some(project_types),
        }
    }
}

impl SourceFile {
    pub fn display<'a>(&'a self, code: &'a Bytecode, opts: &'a FormatOptions) -> impl Display + 'a {
        self.class.display(code, opts)
    }
}

struct ClassDisplay<'a> {
    class: &'a Class,
    code: &'a Bytecode,
    opts: &'a FormatOptions,
    project_types: Option<&'a BTreeSet<usize>>,
}

impl Display for ClassDisplay<'_> {
    fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
        let class_name = simple_type_name(self.class.name.as_str());
        write!(out, "{}class {}", self.opts, class_name)?;
        if let Some(parent) = &self.class.parent {
            write!(out, " extends {}", source_type_name(parent.as_str()))?;
        }
        writeln!(out, " {{")?;
        let nested = self.opts.inc_nesting();
        for field in &self.class.fields {
            write!(out, "{nested}")?;
            if self.project_types.is_some() {
                out.write_str("public ")?;
            }
            if field.static_ {
                out.write_str("static ")?;
            }
            writeln!(
                out,
                "var {}: {};",
                escape_identifier(field.name.as_str()),
                haxe_type(self.code, field.ty)
            )?;
        }
        for method in &self.class.methods {
            writeln!(out)?;
            if self.project_types.is_some() {
                Display::fmt(&method.display_for_project(self.code, &nested), out)?;
            } else {
                Display::fmt(&method.display(self.code, &nested), out)?;
            }
        }
        write!(out, "{}}}", self.opts)?;

        let mut support = SupportingDeclarations::collect(self.class, self.code);
        if let Some(project_types) = self.project_types {
            support
                .types
                .retain(|type_index| !project_types.contains(type_index));
        }
        support.fmt(out, self.class, self.code, self.opts)
    }
}

#[derive(Default)]
struct SupportingDeclarations {
    types: BTreeSet<usize>,
    visited_types: BTreeSet<usize>,
    current_types: BTreeSet<usize>,
    native_functions: BTreeSet<usize>,
    free_functions: BTreeSet<usize>,
    runtime: bool,
}

impl SupportingDeclarations {
    fn collect(class: &Class, code: &Bytecode) -> Self {
        let mut support = Self::default();
        let current = class.ty.map(|ty| ty.0);
        if let Some(ty) = class.ty {
            support.current_types.insert(ty.0);
            if let Some(static_type) = code
                .types
                .get(ty.0)
                .and_then(Type::get_type_obj)
                .and_then(|object| object.global.0.checked_sub(1))
                .and_then(|global| code.globals.get(global))
            {
                support.current_types.insert(static_type.0);
            }
            support.add_type(code, ty, current);
        }
        for field in &class.fields {
            support.add_type(code, field.ty, current);
        }
        for method in &class.methods {
            if let Some(function) = method.fun.as_fn(code) {
                support.add_type(code, function.t, current);
                support.collect_statements(code, &method.statements, current);
            }
        }
        for current in support.current_types.clone() {
            support.types.remove(&current);
        }
        support
    }

    fn add_type(&mut self, code: &Bytecode, reference: RefType, _current: Option<usize>) {
        if reference.0 >= code.types.len() || !self.visited_types.insert(reference.0) {
            return;
        }
        match &code.types[reference.0] {
            Type::Obj(object) | Type::Struct(object) => {
                let name = object.name(code);
                if !self.current_types.contains(&reference.0)
                    && should_emit_supporting_type(name.as_str())
                {
                    self.types.insert(reference.0);
                }
                if let Some(parent) = object.super_ {
                    self.add_type(code, parent, _current);
                }
                for field in &object.own_fields {
                    self.add_type(code, field.t, _current);
                }
                for prototype in &object.protos {
                    if let Some(target) = code.safe_get_ref_fun(prototype.findex) {
                        let signature = match target {
                            FunPtr::Fun(function) => function.ty(code),
                            FunPtr::Native(native) => native.ty(code),
                        };
                        for argument in &signature.args {
                            self.add_type(code, *argument, _current);
                        }
                        self.add_type(code, signature.ret, _current);
                    }
                }
                for constructor in code.functions.iter().filter(|function| {
                    function.name(code).as_str() == "__constructor__"
                        && function.ty(code).args.first() == Some(&reference)
                }) {
                    for argument in &constructor.ty(code).args {
                        self.add_type(code, *argument, _current);
                    }
                    self.add_type(code, constructor.ty(code).ret, _current);
                }
            }
            Type::Enum {
                name, constructs, ..
            } => {
                if should_emit_supporting_type(code.get(*name).as_str()) {
                    self.types.insert(reference.0);
                }
                for constructor in constructs {
                    for parameter in &constructor.params {
                        self.add_type(code, *parameter, _current);
                    }
                }
            }
            Type::Fun(signature) | Type::Method(signature) => {
                for argument in &signature.args {
                    self.add_type(code, *argument, _current);
                }
                self.add_type(code, signature.ret, _current);
            }
            Type::Ref(inner) | Type::Null(inner) | Type::Packed(inner) => {
                self.add_type(code, *inner, _current)
            }
            Type::Virtual { fields } => {
                for field in fields {
                    self.add_type(code, field.t, _current);
                }
            }
            Type::Abstract { name } => {
                if should_emit_supporting_type(code.get(*name).as_str()) {
                    self.types.insert(reference.0);
                }
            }
            _ => {}
        }
    }

    fn add_function(
        &mut self,
        code: &Bytecode,
        reference: hlbc::types::RefFun,
        current: Option<usize>,
    ) {
        match code.safe_get_ref_fun(reference) {
            Some(FunPtr::Native(native)) => {
                self.native_functions.insert(reference.0);
                self.add_type(code, native.t, current);
            }
            Some(FunPtr::Fun(function)) => {
                self.add_type(code, function.t, current);
                if let Some(parent) = function.parent {
                    self.add_type(code, parent, current);
                } else if function.name(code).as_str() != "trace" {
                    self.free_functions.insert(reference.0);
                }
            }
            None => {
                self.free_functions.insert(reference.0);
            }
        }
    }

    fn collect_statements(
        &mut self,
        code: &Bytecode,
        statements: &[Statement],
        current: Option<usize>,
    ) {
        for statement in statements {
            match statement {
                Statement::VarDecl {
                    variable_type,
                    value,
                    ..
                } => {
                    self.add_type(code, *variable_type, current);
                    if let Some(value) = value {
                        self.collect_expr(code, value, current);
                    }
                }
                Statement::Assign {
                    variable, assign, ..
                } => {
                    self.collect_expr(code, variable, current);
                    self.collect_expr(code, assign, current);
                }
                Statement::ExprStatement(expression)
                | Statement::Throw(expression)
                | Statement::RuntimeCheck(RuntimeCheck::Null(expression)) => {
                    self.collect_expr(code, expression, current)
                }
                Statement::GlobalStore {
                    global_type, value, ..
                } => {
                    self.runtime = true;
                    self.add_type(code, *global_type, current);
                    self.collect_expr(code, value, current);
                }
                Statement::DynamicFieldStore { object, value, .. } => {
                    self.collect_expr(code, object, current);
                    self.collect_expr(code, value, current);
                }
                Statement::MemoryStore {
                    memory_type,
                    bytes,
                    index,
                    value,
                    value_type,
                } => {
                    if matches!(memory_type, MemoryType::Typed(ty) if !matches!(code.types.get(ty.0), Some(Type::I32 | Type::F32 | Type::F64)))
                    {
                        self.runtime = true;
                    }
                    self.add_type(code, *value_type, current);
                    self.collect_expr(code, bytes, current);
                    self.collect_expr(code, index, current);
                    self.collect_expr(code, value, current);
                }
                Statement::ReferenceStore {
                    reference,
                    value,
                    value_type,
                } => {
                    self.add_type(code, *value_type, current);
                    self.collect_expr(code, reference, current);
                    self.collect_expr(code, value, current);
                }
                Statement::Prefetch { value, .. } => self.collect_expr(code, value, current),
                Statement::Return(Some(value)) => self.collect_expr(code, value, current),
                Statement::IfElse { cond, if_, else_ } => {
                    self.collect_expr(code, cond, current);
                    self.collect_statements(code, if_, current);
                    self.collect_statements(code, else_, current);
                }
                Statement::Switch {
                    arg,
                    default,
                    cases,
                } => {
                    self.collect_expr(code, arg, current);
                    self.collect_statements(code, default, current);
                    for (patterns, body) in cases {
                        for pattern in patterns {
                            self.collect_expr(code, pattern, current);
                        }
                        self.collect_statements(code, body, current);
                    }
                }
                Statement::While { cond, stmts } | Statement::DoWhile { cond, stmts } => {
                    self.collect_expr(code, cond, current);
                    self.collect_statements(code, stmts, current);
                }
                Statement::ForEach {
                    iterable, stmts, ..
                } => {
                    self.collect_expr(code, iterable, current);
                    self.collect_statements(code, stmts, current);
                }
                Statement::ForRange {
                    start, end, stmts, ..
                } => {
                    self.collect_expr(code, start, current);
                    self.collect_expr(code, end, current);
                    self.collect_statements(code, stmts, current);
                }
                Statement::Try { stmts } | Statement::Catch { stmts } => {
                    self.collect_statements(code, stmts, current)
                }
                Statement::TryCatch { try_stmts, catches } => {
                    self.collect_statements(code, try_stmts, current);
                    for catch in catches {
                        self.add_type(code, catch.variable_type, current);
                        self.collect_statements(code, &catch.stmts, current);
                    }
                }
                Statement::StateMachine {
                    locals: _, blocks, ..
                } => {
                    for block in blocks {
                        self.collect_statements(code, &block.stmts, current);
                        match &block.terminator {
                            StateTerminator::Branch { cond, .. } => {
                                self.collect_expr(code, cond, current)
                            }
                            StateTerminator::Switch { arg, .. } => {
                                self.collect_expr(code, arg, current)
                            }
                            StateTerminator::Return(Some(value))
                            | StateTerminator::Throw(value) => {
                                self.collect_expr(code, value, current)
                            }
                            _ => {}
                        }
                    }
                }
                Statement::Provenanced {
                    statement: inner, ..
                } => self.collect_statements(code, std::slice::from_ref(inner), current),
                Statement::RuntimeCheck(RuntimeCheck::Assert)
                | Statement::Nop
                | Statement::Return(None)
                | Statement::Break
                | Statement::Continue
                | Statement::Comment(_)
                | Statement::UnhandledOpcode { .. } => {}
            }
        }
    }

    fn collect_expr(&mut self, code: &Bytecode, expression: &Expr, current: Option<usize>) {
        match raw_expression(expression) {
            Expr::Anonymous(ty, values) => {
                self.add_type(code, *ty, current);
                for value in values.values() {
                    self.collect_expr(code, value, current);
                }
            }
            Expr::Array(array, index) => {
                self.collect_expr(code, array, current);
                self.collect_expr(code, index, current);
            }
            Expr::ArrayLiteral {
                elements,
                element_type,
                ..
            } => {
                if let Some(element_type) = element_type {
                    self.add_type(code, *element_type, current);
                }
                for element in elements {
                    self.collect_expr(code, element, current);
                }
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    self.collect_expr(code, key, current);
                    self.collect_expr(code, value, current);
                }
            }
            Expr::ArrayAlloc {
                length,
                element_type,
                native,
            } => {
                if !native {
                    self.runtime = true;
                }
                if let Some(element_type) = element_type {
                    self.add_type(code, *element_type, current);
                }
                self.collect_expr(code, length, current);
            }
            Expr::Bytes(_) => self.runtime = true,
            Expr::Call(call) => {
                if let CallKind::Static(reference)
                | CallKind::Instance(reference)
                | CallKind::Virtual(reference) = call.kind
                {
                    self.add_function(code, reference, current);
                }
                self.collect_expr(code, &call.fun, current);
                for argument in &call.args {
                    self.collect_expr(code, argument, current);
                }
            }
            Expr::Constructor(call) => {
                self.add_type(code, call.ty, current);
                for argument in &call.args {
                    self.collect_expr(code, argument, current);
                }
            }
            Expr::Closure(reference, _, captures, statements) => {
                self.add_function(code, *reference, current);
                for (_, value) in captures {
                    self.collect_expr(code, value, current);
                }
                self.collect_statements(code, statements, current);
            }
            Expr::EnumConstr(ty, _, arguments) | Expr::EnumPatternBinding(ty, _, arguments) => {
                self.add_type(code, *ty, current);
                for argument in arguments {
                    self.collect_expr(code, argument, current);
                }
            }
            Expr::EnumPattern(ty, _, _) => self.add_type(code, *ty, current),
            Expr::EnumField {
                value, result_type, ..
            } => {
                self.add_type(code, *result_type, current);
                self.collect_expr(code, value, current);
            }
            Expr::FunRef(reference) => self.add_function(code, *reference, current),
            Expr::MemoryLoad {
                memory_type,
                bytes,
                index,
                result_type,
            } => {
                if matches!(memory_type, MemoryType::Typed(ty) if !matches!(code.types.get(ty.0), Some(Type::I32 | Type::F32 | Type::F64)))
                {
                    self.runtime = true;
                }
                self.add_type(code, *result_type, current);
                self.collect_expr(code, bytes, current);
                self.collect_expr(code, index, current);
            }
            Expr::TypeValue {
                ty, result_type, ..
            } => {
                self.add_type(code, *ty, current);
                self.add_type(code, *result_type, current);
            }
            Expr::RuntimeType { value, result_type } | Expr::TypeId { value, result_type } => {
                self.add_type(code, *result_type, current);
                self.collect_expr(code, value, current);
            }
            Expr::SafeCast { value, target_type } => {
                self.add_type(code, *target_type, current);
                self.collect_expr(code, value, current);
            }
            Expr::VirtualClosure {
                receiver,
                target,
                callable_type,
                target_type,
                ..
            } => {
                self.add_function(code, *target, current);
                self.add_type(code, *callable_type, current);
                self.add_type(code, *target_type, current);
                self.collect_expr(code, receiver, current);
            }
            Expr::Reference {
                value,
                reference_type,
                ..
            }
            | Expr::ReferenceOffset {
                reference: value,
                reference_type,
                ..
            } => {
                self.add_type(code, *reference_type, current);
                self.collect_expr(code, value, current);
            }
            Expr::ReferenceData {
                array,
                reference_type,
            } => {
                self.runtime = true;
                self.add_type(code, *reference_type, current);
                self.collect_expr(code, array, current);
            }
            Expr::Dereference {
                reference,
                result_type,
            } => {
                self.add_type(code, *result_type, current);
                self.collect_expr(code, reference, current);
            }
            Expr::IfElse { cond, if_, else_ } => {
                self.collect_expr(code, cond, current);
                self.collect_statements(code, if_, current);
                self.collect_statements(code, else_, current);
            }
            Expr::Op(operation) => self.collect_operation(code, operation, current),
            Expr::StringConcat(expressions) => {
                for expression in expressions {
                    self.collect_expr(code, expression, current);
                }
            }
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let StringPart::Expression(expression) = part {
                        self.collect_expr(code, expression, current);
                    }
                }
            }
            Expr::ToString(expression)
            | Expr::EnumIndex(expression)
            | Expr::Field(expression, _)
            | Expr::DynamicField(expression, _) => self.collect_expr(code, expression, current),
            Expr::SuperCall(arguments) => {
                for argument in arguments {
                    self.collect_expr(code, argument, current);
                }
            }
            Expr::SuperMethod {
                args,
                target,
                return_type,
                ..
            } => {
                self.add_function(code, *target, current);
                self.add_type(code, *return_type, current);
                for argument in args {
                    self.collect_expr(code, argument, current);
                }
            }
            Expr::Provenanced { .. } => unreachable!("raw_expression removes provenance"),
            Expr::GlobalLoad { result_type, .. } => {
                self.runtime = true;
                self.add_type(code, *result_type, current);
            }
            Expr::Constant(_) | Expr::Capture(_) | Expr::Unknown(_) | Expr::Variable(_, _) => {}
        }
    }

    fn collect_operation(
        &mut self,
        code: &Bytecode,
        operation: &Operation,
        current: Option<usize>,
    ) {
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
                self.collect_expr(code, left, current);
                self.collect_expr(code, right, current);
            }
            Operation::Neg(value)
            | Operation::Not(value)
            | Operation::Incr(value)
            | Operation::Decr(value) => self.collect_expr(code, value, current),
        }
    }

    fn fmt(
        &self,
        out: &mut Formatter<'_>,
        class: &Class,
        code: &Bytecode,
        opts: &FormatOptions,
    ) -> fmt::Result {
        for type_index in &self.types {
            writeln!(out)?;
            writeln!(out)?;
            fmt_supporting_type(out, code, RefType(*type_index), opts)?;
        }
        if !self.native_functions.is_empty() {
            writeln!(out)?;
            writeln!(out)?;
            fmt_native_externs(out, code, &self.native_functions, opts)?;
        }
        if !self.free_functions.is_empty() {
            writeln!(out)?;
            writeln!(out)?;
            fmt_free_function_stubs(out, code, &self.free_functions, opts)?;
        }
        if self.runtime {
            writeln!(out)?;
            writeln!(out)?;
            fmt_runtime_helpers(out, opts)?;
        }
        let _ = class;
        Ok(())
    }
}

fn should_emit_supporting_type(name: &str) -> bool {
    !name.is_empty()
        && name != "<none>"
        && !is_runtime_type_path(name)
        && !matches!(
            name,
            "String" | "Array" | "Date" | "EReg" | "Math" | "Reflect" | "Std" | "Type" | "Xml"
        )
        && !name.starts_with("hl_")
}

fn is_runtime_type_path(name: &str) -> bool {
    [
        "cpp.", "cs.", "eval.", "flash.", "haxe.", "hl.", "java.", "js.", "lua.", "neko.", "php.",
        "python.", "sys.", "wasi.",
    ]
    .iter()
    .any(|prefix| name.starts_with(prefix))
}

fn fmt_supporting_type(
    out: &mut Formatter<'_>,
    code: &Bytecode,
    reference: RefType,
    opts: &FormatOptions,
) -> fmt::Result {
    match &code.types[reference.0] {
        Type::Enum {
            name, constructs, ..
        } => {
            writeln!(
                out,
                "{opts}enum {} {{",
                simple_type_name(code.get(*name).as_str())
            )?;
            let nested = opts.inc_nesting();
            for constructor in constructs {
                write!(
                    out,
                    "{nested}{}",
                    escape_identifier(constructor.name(code).as_str())
                )?;
                if !constructor.params.is_empty() {
                    out.write_str("(")?;
                    for (index, parameter) in constructor.params.iter().enumerate() {
                        if index > 0 {
                            out.write_str(", ")?;
                        }
                        write!(out, "arg{index}: {}", haxe_type(code, *parameter))?;
                    }
                    out.write_str(")")?;
                }
                writeln!(out, ";")?;
            }
            write!(out, "{opts}}}")
        }
        Type::Obj(object) | Type::Struct(object) => {
            write!(
                out,
                "{opts}class {}",
                simple_type_name(object.name(code).as_str())
            )?;
            if let Some(parent) = object.super_ {
                write!(out, " extends {}", haxe_type(code, parent))?;
            }
            writeln!(out, " {{")?;
            let nested = opts.inc_nesting();
            for field in &object.own_fields {
                writeln!(
                    out,
                    "{nested}public var {}: {};",
                    escape_identifier(field.name(code).as_str()),
                    haxe_type(code, field.t)
                )?;
            }
            let constructor = code.functions.iter().find(|function| {
                function.name(code).as_str() == "__constructor__"
                    && function.ty(code).args.first() == Some(&reference)
            });
            write!(out, "{nested}public function new(")?;
            if let Some(constructor) = constructor {
                fmt_signature_arguments(out, code, constructor.ty(code), 1)?;
            }
            writeln!(out, ") {{}}")?;
            let mut emitted_methods = BTreeSet::new();
            for prototype in &object.protos {
                let name = escape_identifier(prototype.name(code).as_str());
                if name == "new" || !emitted_methods.insert(name.clone()) {
                    continue;
                }
                let Some(target) = code.safe_get_ref_fun(prototype.findex) else {
                    continue;
                };
                let signature = match target {
                    FunPtr::Fun(function) => function.ty(code),
                    FunPtr::Native(native) => native.ty(code),
                };
                write!(out, "{nested}public function {name}(")?;
                fmt_signature_arguments(out, code, signature, 1)?;
                write!(out, "): {}", haxe_type(code, signature.ret))?;
                if signature.ret.is_void() {
                    writeln!(out, " {{}}")?;
                } else {
                    writeln!(out, " return cast null;")?;
                }
            }
            write!(out, "{opts}}}")
        }
        Type::Abstract { name } => write!(
            out,
            "{opts}typedef {} = Dynamic;",
            simple_type_name(code.get(*name).as_str())
        ),
        _ => write!(
            out,
            "{opts}typedef __HlType{} = {};",
            reference.0,
            haxe_type(code, reference)
        ),
    }
}

/// Display one bytecode type as a standalone Haxe declaration.
///
/// This is primarily used by project emitters; primitive and internal types
/// are represented by an explicit typedef fallback.
pub fn display_type_declaration<'a>(
    code: &'a Bytecode,
    reference: RefType,
    opts: &'a FormatOptions,
) -> impl Display + 'a {
    TypeDeclarationDisplay {
        code,
        reference,
        opts,
    }
}

struct TypeDeclarationDisplay<'a> {
    code: &'a Bytecode,
    reference: RefType,
    opts: &'a FormatOptions,
}

impl Display for TypeDeclarationDisplay<'_> {
    fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
        if self.reference.0 >= self.code.types.len() {
            return write!(
                out,
                "{}typedef __HlInvalidType{} = Dynamic;",
                self.opts, self.reference.0
            );
        }
        fmt_supporting_type(out, self.code, self.reference, self.opts)
    }
}

fn fmt_native_externs(
    out: &mut Formatter<'_>,
    code: &Bytecode,
    references: &BTreeSet<usize>,
    opts: &FormatOptions,
) -> fmt::Result {
    writeln!(out, "{opts}private extern class __HlNatives {{")?;
    let nested = opts.inc_nesting();
    for reference in references {
        let Some(FunPtr::Native(native)) = code.safe_get_ref_fun(hlbc::types::RefFun(*reference))
        else {
            continue;
        };
        writeln!(
            out,
            "{nested}@:hlNative(\"{}\", \"{}\")",
            escape_haxe_string(native.lib(code).as_str(), '"'),
            escape_haxe_string(native.name(code).as_str(), '"')
        )?;
        write!(out, "{nested}public static function f{reference}(")?;
        fmt_signature_arguments(out, code, native.ty(code), 0)?;
        writeln!(out, "): {};", haxe_type(code, native.ty(code).ret))?;
    }
    write!(out, "{opts}}}")
}

fn fmt_free_function_stubs(
    out: &mut Formatter<'_>,
    code: &Bytecode,
    references: &BTreeSet<usize>,
    opts: &FormatOptions,
) -> fmt::Result {
    writeln!(out, "{opts}private class __HlFunctions {{")?;
    let nested = opts.inc_nesting();
    for reference in references {
        let Some(FunPtr::Fun(function)) = code.safe_get_ref_fun(hlbc::types::RefFun(*reference))
        else {
            writeln!(
                out,
                "{nested}public static function f{reference}(...args: Dynamic): Dynamic return null;"
            )?;
            continue;
        };
        write!(out, "{nested}public static function f{reference}(")?;
        fmt_signature_arguments(out, code, function.ty(code), 0)?;
        write!(out, "): {}", haxe_type(code, function.ty(code).ret))?;
        if function.ty(code).ret.is_void() {
            writeln!(out, " {{}}")?;
        } else {
            writeln!(out, " return cast null;")?;
        }
    }
    write!(out, "{opts}}}")
}

fn fmt_signature_arguments(
    out: &mut Formatter<'_>,
    code: &Bytecode,
    signature: &TypeFun,
    skip: usize,
) -> fmt::Result {
    for (index, argument) in signature.args.iter().skip(skip).enumerate() {
        if index > 0 {
            out.write_str(", ")?;
        }
        write!(out, "arg{index}: {}", haxe_type(code, *argument))?;
    }
    Ok(())
}

fn fmt_runtime_helpers(out: &mut Formatter<'_>, opts: &FormatOptions) -> fmt::Result {
    let nested = opts.inc_nesting();
    writeln!(out, "{opts}private class __HlRuntime {{")?;
    writeln!(
        out,
        "{nested}public static function setGlobal(index: Int, value: Dynamic): Void {{}}"
    )?;
    writeln!(
        out,
        "{nested}public static function getGlobal(index: Int): Dynamic throw 'HashLink global $index requires a runtime binding';"
    )?;
    writeln!(
        out,
        "{nested}public static function getMem(bytes: hl.Bytes, index: Int): Dynamic return null;"
    )?;
    writeln!(
        out,
        "{nested}public static function setMem(bytes: hl.Bytes, index: Int, value: Dynamic): Void {{}}"
    )?;
    writeln!(
        out,
        "{nested}public static function array(length: Int): Array<Dynamic> return [for (_ in 0...length) null];"
    )?;
    writeln!(
        out,
        "{nested}public static function arrayData(value: Dynamic): Dynamic return null;"
    )?;
    writeln!(
        out,
        "{nested}public static function bytes(hex: String): hl.Bytes {{"
    )?;
    let body = nested.inc_nesting();
    writeln!(out, "{body}var result = new hl.Bytes(hex.length >> 1);")?;
    writeln!(
        out,
        "{body}for (i in 0...(hex.length >> 1)) result[i] = Std.parseInt(\"0x\" + hex.substr(i << 1, 2));"
    )?;
    writeln!(out, "{body}return result;")?;
    writeln!(out, "{nested}}}")?;
    write!(out, "{opts}}}")
}

impl Method {
    pub fn display<'a>(&'a self, code: &'a Bytecode, opts: &'a FormatOptions) -> impl Display + 'a {
        MethodDisplay {
            method: self,
            code,
            opts,
            public_: false,
        }
    }

    fn display_for_project<'a>(
        &'a self,
        code: &'a Bytecode,
        opts: &'a FormatOptions,
    ) -> impl Display + 'a {
        MethodDisplay {
            method: self,
            code,
            opts,
            public_: true,
        }
    }
}

struct MethodDisplay<'a> {
    method: &'a Method,
    code: &'a Bytecode,
    opts: &'a FormatOptions,
    public_: bool,
}

impl Display for MethodDisplay<'_> {
    fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
        let Some(function) = self.method.fun.as_fn(self.code) else {
            return writeln!(
                out,
                "{}// invalid function reference {}",
                self.opts, self.method.fun.0
            );
        };
        write!(out, "{}", self.opts)?;
        if self.public_ {
            out.write_str("public ")?;
        }
        if self.method.override_ {
            out.write_str("override ")?;
        }
        if self.method.static_ {
            out.write_str("static ")?;
        }
        if self.method.dynamic {
            out.write_str("dynamic ")?;
        }
        let method_name = if self.method.constructor {
            "new".to_owned()
        } else {
            escape_identifier(function.name(self.code).as_str())
        };
        write!(out, "function {method_name}(")?;

        let skip_receiver = usize::from(!self.method.static_);
        let arguments = function.ty(self.code).args.iter().skip(skip_receiver);
        for (position, argument) in arguments.enumerate() {
            if position > 0 {
                out.write_str(", ")?;
            }
            let name = function
                .arg_name(self.code, position)
                .map(|name| escape_identifier(name.as_str()))
                .unwrap_or_else(|| format!("arg{position}"));
            write!(out, "{name}: {}", haxe_type(self.code, *argument))?;
        }
        out.write_str(")")?;
        if !self.method.constructor && !function.ty(self.code).ret.is_void() {
            write!(
                out,
                ": {}",
                haxe_type(self.code, function.ty(self.code).ret)
            )?;
        }
        if self.method.statements.is_empty() {
            return writeln!(out, " {{}}");
        }

        writeln!(out, " {{")?;
        let nested = self.opts.inc_nesting();
        for statement in &self.method.statements {
            write!(out, "{nested}")?;
            Display::fmt(&statement.display(&nested, self.code, function), out)?;
            writeln!(out)?;
        }
        writeln!(out, "{}}}", self.opts)
    }
}

impl Expr {
    pub fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        function: &'a Function,
    ) -> impl Display + 'a {
        ExprDisplay {
            expression: self,
            indent,
            code,
            function,
        }
    }
}

struct ExprDisplay<'a> {
    expression: &'a Expr,
    indent: &'a FormatOptions,
    code: &'a Bytecode,
    function: &'a Function,
}

impl Display for ExprDisplay<'_> {
    fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
        fmt_expr(
            out,
            self.expression,
            self.indent,
            self.code,
            self.function,
            ParentExpr::Root,
        )
    }
}

#[derive(Clone, Copy)]
enum ParentExpr {
    Root,
    Left(u8),
    Right(u8),
    Unary(u8),
    Postfix(u8),
}

const PREC_IF: u8 = 1;
const PREC_OR: u8 = 2;
const PREC_AND: u8 = 3;
const PREC_BIT_OR: u8 = 4;
const PREC_XOR: u8 = 5;
const PREC_BIT_AND: u8 = 6;
const PREC_COMPARE: u8 = 7;
const PREC_SHIFT: u8 = 8;
const PREC_ADD: u8 = 9;
const PREC_MUL: u8 = 10;
const PREC_UNARY: u8 = 11;
const PREC_POSTFIX: u8 = 12;
const PREC_PRIMARY: u8 = 13;

fn raw_expression(mut expression: &Expr) -> &Expr {
    while let Expr::Provenanced {
        expression: inner, ..
    } = expression
    {
        expression = inner;
    }
    expression
}

fn expression_precedence(expression: &Expr) -> u8 {
    match raw_expression(expression) {
        Expr::IfElse { .. } => PREC_IF,
        Expr::Op(operation) => operation_precedence(operation),
        Expr::StringConcat(_) => PREC_ADD,
        Expr::Array(..)
        | Expr::Call(..)
        | Expr::Field(..)
        | Expr::DynamicField(..)
        | Expr::SuperMethod { .. }
        | Expr::VirtualClosure { .. }
        | Expr::EnumField { .. } => PREC_POSTFIX,
        _ => PREC_PRIMARY,
    }
}

fn operation_precedence(operation: &Operation) -> u8 {
    match operation {
        Operation::Or(..) => PREC_OR,
        Operation::And(..) => PREC_AND,
        Operation::BitOr(..) => PREC_BIT_OR,
        Operation::Xor(..) => PREC_XOR,
        Operation::BitAnd(..) => PREC_BIT_AND,
        Operation::Eq(..)
        | Operation::NotEq(..)
        | Operation::Gt(..)
        | Operation::Gte(..)
        | Operation::Lt(..)
        | Operation::Lte(..) => PREC_COMPARE,
        Operation::Shl(..) | Operation::Shr(..) => PREC_SHIFT,
        Operation::Add(..) | Operation::Sub(..) => PREC_ADD,
        Operation::Mul(..) | Operation::Div(..) | Operation::Mod(..) => PREC_MUL,
        Operation::Neg(..) | Operation::Not(..) => PREC_UNARY,
        Operation::Incr(..) | Operation::Decr(..) => PREC_POSTFIX,
    }
}

fn needs_parentheses(precedence: u8, parent: ParentExpr) -> bool {
    match parent {
        ParentExpr::Root => false,
        ParentExpr::Left(required) => precedence < required,
        ParentExpr::Right(required) => precedence <= required,
        ParentExpr::Unary(required) => precedence <= required,
        ParentExpr::Postfix(required) => precedence < required,
    }
}

fn fmt_expr(
    out: &mut Formatter<'_>,
    expression: &Expr,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
    parent: ParentExpr,
) -> fmt::Result {
    let expression = raw_expression(expression);
    let precedence = expression_precedence(expression);
    let parentheses = needs_parentheses(precedence, parent);
    if parentheses {
        out.write_str("(")?;
    }
    match expression {
        Expr::Anonymous(ty, values) => {
            if !matches!(code.types.get(ty.0), Some(Type::Virtual { .. })) {
                out.write_str("(cast {} : Dynamic)")?;
                if parentheses {
                    out.write_str(")")?;
                }
                return Ok(());
            }
            out.write_str("{")?;
            if let Some(Type::Virtual { fields }) = code.types.get(ty.0) {
                for (index, field) in fields.iter().enumerate() {
                    if index > 0 {
                        out.write_str(", ")?;
                    }
                    write!(out, "{}: ", escape_identifier(field.name(code).as_str()))?;
                    if let Some(value) = values.get(&hlbc::types::RefField(index)) {
                        fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
                    } else {
                        out.write_str("null")?;
                    }
                }
            }
            out.write_str("}")?;
        }
        Expr::Array(array, index) => {
            fmt_expr(
                out,
                array,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str("[")?;
            fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
            out.write_str("]")?;
        }
        Expr::ArrayLiteral {
            elements,
            element_type,
            native,
        } => {
            if *native {
                let ty = element_type
                    .map(|ty| haxe_type(code, ty))
                    .unwrap_or(HaxeType::Dynamic);
                out.write_str("{ var __a = new hl.NativeArray<")?;
                write!(out, "{ty}>({}); ", elements.len())?;
                for (index, element) in elements.iter().enumerate() {
                    write!(out, "__a[{index}] = ")?;
                    fmt_expr(out, element, indent, code, function, ParentExpr::Root)?;
                    out.write_str("; ")?;
                }
                out.write_str("__a; }")?;
            } else {
                out.write_str("[")?;
                fmt_expression_list(out, elements, indent, code, function)?;
                out.write_str("]")?;
            }
        }
        Expr::MapLiteral { entries } => {
            out.write_str("[")?;
            for (index, (key, value)) in entries.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                fmt_expr(out, key, indent, code, function, ParentExpr::Root)?;
                out.write_str(" => ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            }
            out.write_str("]")?;
        }
        Expr::ArrayAlloc {
            length,
            element_type,
            native,
        } => {
            if *native {
                let ty = element_type
                    .map(|ty| haxe_type(code, ty))
                    .unwrap_or(HaxeType::Dynamic);
                write!(out, "new hl.NativeArray<{ty}>(")?;
                fmt_expr(out, length, indent, code, function, ParentExpr::Root)?;
                out.write_str(")")?;
            } else {
                out.write_str("__HlRuntime.array(")?;
                fmt_expr(out, length, indent, code, function, ParentExpr::Root)?;
                out.write_str(")")?;
            }
        }
        Expr::Bytes(literal) => {
            write!(out, "__HlRuntime.bytes(\"")?;
            for byte in &literal.bytes {
                write!(out, "{byte:02x}")?;
            }
            out.write_str("\")")?;
        }
        Expr::Call(call) => {
            let positioned_trace = matches!(
                raw_expression(&call.fun),
                Expr::FunRef(reference)
                    if reference
                        .as_fn(code)
                        .is_some_and(|target| target.name(code).as_str() == "trace")
            ) && call.args.len() == 2;
            if positioned_trace {
                out.write_str("haxe.Log.trace")?;
            } else {
                fmt_expr(
                    out,
                    &call.fun,
                    indent,
                    code,
                    function,
                    ParentExpr::Postfix(PREC_POSTFIX),
                )?;
            }
            out.write_str("(")?;
            fmt_expression_list(out, &call.args, indent, code, function)?;
            out.write_str(")")?;
        }
        Expr::Constant(constant) => fmt_constant(out, constant, code)?,
        Expr::Constructor(ConstructorCall { ty, args }) => {
            write!(out, "new {}(", haxe_type(code, *ty))?;
            fmt_expression_list(out, args, indent, code, function)?;
            out.write_str(")")?;
        }
        Expr::Closure(reference, bound_arguments, captures, statements) => {
            if let Some(closure) = reference.as_fn(code) {
                let closure_indent = if captures.is_empty() {
                    indent.clone()
                } else {
                    out.write_str("{")?;
                    let nested = indent.inc_nesting();
                    for (name, value) in captures {
                        writeln!(out)?;
                        write!(out, "{nested}var {} = ", escape_identifier(name.as_str()))?;
                        fmt_expr(out, value, &nested, code, function, ParentExpr::Root)?;
                        out.write_str(";")?;
                    }
                    writeln!(out)?;
                    write!(out, "{nested}")?;
                    nested
                };
                out.write_str("(")?;
                for (index, argument) in closure
                    .ty(code)
                    .args
                    .iter()
                    .skip(*bound_arguments)
                    .enumerate()
                {
                    if index > 0 {
                        out.write_str(", ")?;
                    }
                    let argument_index = index + *bound_arguments;
                    let name = closure
                        .arg_name(code, argument_index)
                        .map(|name| escape_identifier(name.as_str()))
                        .unwrap_or_else(|| format!("__hl_r{argument_index}"));
                    write!(out, "{name}: {}", haxe_type(code, *argument))?;
                }
                out.write_str(") -> {")?;
                if !statements.is_empty() {
                    writeln!(out)?;
                    let nested = closure_indent.inc_nesting();
                    for statement in statements {
                        write!(out, "{nested}")?;
                        Display::fmt(&statement.display(&nested, code, closure), out)?;
                        writeln!(out)?;
                    }
                    write!(out, "{closure_indent}")?;
                }
                out.write_str("}")?;
                if !captures.is_empty() {
                    writeln!(out, ";")?;
                    write!(out, "{indent}}}")?;
                }
            } else {
                out.write_str("(cast null : Dynamic)")?;
            }
        }
        Expr::EnumConstr(ty, constructor, arguments) => {
            let name = enum_constructor_name(code, *ty, constructor.0);
            out.write_str(&name)?;
            if !arguments.is_empty() {
                out.write_str("(")?;
                fmt_expression_list(out, arguments, indent, code, function)?;
                out.write_str(")")?;
            }
        }
        Expr::EnumIndex(value) => {
            out.write_str("Type.enumIndex(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::EnumPattern(ty, constructor, arity) => {
            out.write_str(&enum_constructor_name(code, *ty, constructor.0))?;
            if *arity > 0 {
                out.write_str("(")?;
                for index in 0..*arity {
                    if index > 0 {
                        out.write_str(", ")?;
                    }
                    out.write_str("_")?;
                }
                out.write_str(")")?;
            }
        }
        Expr::EnumPatternBinding(ty, constructor, variables) => {
            out.write_str(&enum_constructor_name(code, *ty, constructor.0))?;
            if !variables.is_empty() {
                out.write_str("(")?;
                fmt_expression_list(out, variables, indent, code, function)?;
                out.write_str(")")?;
            }
        }
        Expr::EnumField {
            value,
            field,
            result_type,
            ..
        } => {
            write!(out, "(cast Type.enumParameters(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            write!(out, ")[{}] : {})", field.0, haxe_type(code, *result_type))?;
        }
        Expr::Field(receiver, name) => {
            fmt_expr(
                out,
                receiver,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            write!(out, ".{}", escape_identifier(name.as_str()))?;
        }
        Expr::DynamicField(receiver, name) => {
            out.write_str("Reflect.field(")?;
            fmt_expr(out, receiver, indent, code, function, ParentExpr::Root)?;
            let name = code
                .strings
                .get(name.0)
                .map(|name| name.as_str())
                .unwrap_or("");
            write!(out, ", \"{}\")", escape_haxe_string(name, '"'))?;
        }
        Expr::FunRef(reference) => {
            out.write_str(&function_reference_name(code, function, *reference))?;
        }
        Expr::GlobalLoad {
            global,
            result_type,
        } => {
            write!(
                out,
                "(cast __HlRuntime.getGlobal({}) : {})",
                global.0,
                haxe_type(code, *result_type)
            )?;
        }
        Expr::SuperCall(arguments) => {
            out.write_str("super(")?;
            fmt_expression_list(out, arguments, indent, code, function)?;
            out.write_str(")")?;
        }
        Expr::SuperMethod { method, args, .. } => {
            write!(out, "super.{}(", escape_identifier(method.as_str()))?;
            fmt_expression_list(out, args, indent, code, function)?;
            out.write_str(")")?;
        }
        Expr::MemoryLoad {
            memory_type,
            bytes,
            index,
            ..
        } => fmt_memory_load(out, *memory_type, bytes, index, indent, code, function)?,
        Expr::TypeValue { ty, .. } => {
            write!(out, "hl.Type.get((cast null : {}))", haxe_type(code, *ty))?;
        }
        Expr::RuntimeType { value, .. } => {
            out.write_str("hl.Type.getDynamic(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::TypeId { value, .. } => {
            out.write_str("(cast ")?;
            fmt_expr(
                out,
                value,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".kind : Int)")?;
        }
        Expr::SafeCast { value, target_type } => {
            out.write_str("cast(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            write!(out, ", {})", haxe_type(code, *target_type))?;
        }
        Expr::VirtualClosure {
            receiver, method, ..
        } => {
            fmt_expr(
                out,
                receiver,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            write!(out, ".{}", escape_identifier(method.as_str()))?;
        }
        Expr::Reference {
            value,
            reference_type,
            ..
        } => {
            let inner = match code.types.get(reference_type.0) {
                Some(Type::Ref(inner)) => haxe_type(code, *inner),
                _ => HaxeType::Dynamic,
            };
            write!(out, "new hl.Ref<{inner}>(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::Dereference { reference, .. } => {
            fmt_expr(
                out,
                reference,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".get()")?;
        }
        Expr::ReferenceData { array, .. } => {
            out.write_str("__HlRuntime.arrayData(")?;
            fmt_expr(out, array, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::ReferenceOffset {
            reference, offset, ..
        } => {
            fmt_expr(
                out,
                reference,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".offset(")?;
            fmt_expr(out, offset, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::IfElse { cond, if_, else_ } => {
            out.write_str("if (")?;
            fmt_expr(out, cond, indent, code, function, ParentExpr::Root)?;
            out.write_str(") {")?;
            fmt_expression_block(out, if_, indent, code, function)?;
            out.write_str(" else {")?;
            fmt_expression_block(out, else_, indent, code, function)?;
        }
        Expr::Op(operation) => fmt_operation(out, operation, indent, code, function)?,
        Expr::StringConcat(expressions) => {
            for (index, expression) in expressions.iter().enumerate() {
                if index > 0 {
                    out.write_str(" + ")?;
                }
                let parent = if index == 0 {
                    ParentExpr::Left(PREC_ADD)
                } else {
                    ParentExpr::Right(PREC_ADD)
                };
                fmt_expr(out, expression, indent, code, function, parent)?;
            }
        }
        Expr::StringInterpolation(parts) => {
            out.write_str("'")?;
            for part in parts {
                match part {
                    StringPart::Literal(text) => {
                        out.write_str(&escape_haxe_string(text, '\''))?;
                    }
                    StringPart::Expression(expression) => {
                        out.write_str("${")?;
                        fmt_expr(out, expression, indent, code, function, ParentExpr::Root)?;
                        out.write_str("}")?;
                    }
                }
            }
            out.write_str("'")?;
        }
        Expr::ToString(expression) => {
            out.write_str("Std.string(")?;
            fmt_expr(out, expression, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")?;
        }
        Expr::Capture(name) => out.write_str(&escape_identifier(name.as_str()))?,
        Expr::Unknown(message) => {
            write!(
                out,
                "(cast null : Dynamic) /* {} */",
                escape_comment(message)
            )?;
        }
        Expr::Variable(register, name) => {
            if let Some(name) = name {
                out.write_str(&escape_identifier(name.as_str()))?;
            } else {
                write!(out, "__hl_r{}", register.0)?;
            }
        }
        Expr::Provenanced { .. } => unreachable!("raw_expression removes provenance"),
    }
    if parentheses {
        out.write_str(")")?;
    }
    Ok(())
}

fn fmt_expression_list(
    out: &mut Formatter<'_>,
    expressions: &[Expr],
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    for (index, expression) in expressions.iter().enumerate() {
        if index > 0 {
            out.write_str(", ")?;
        }
        fmt_expr(out, expression, indent, code, function, ParentExpr::Root)?;
    }
    Ok(())
}

fn fmt_expression_block(
    out: &mut Formatter<'_>,
    statements: &[Statement],
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    writeln!(out)?;
    let nested = indent.inc_nesting();
    for statement in statements {
        write!(out, "{nested}")?;
        Display::fmt(&statement.display(&nested, code, function), out)?;
        writeln!(out)?;
    }
    write!(out, "{indent}}}")
}

fn fmt_constant(out: &mut Formatter<'_>, constant: &Constant, code: &Bytecode) -> fmt::Result {
    match constant {
        Constant::InlineInt(value) => Display::fmt(value, out),
        Constant::SignedInt(value) => Display::fmt(value, out),
        Constant::Int(reference) => match code.ints.get(reference.0) {
            Some(value) => Display::fmt(value, out),
            None => write!(out, "0 /* invalid int {} */", reference.0),
        },
        Constant::Float(reference) => match code.floats.get(reference.0).copied() {
            Some(value) if value.is_nan() => out.write_str("Math.NaN"),
            Some(value) if value == f64::INFINITY => out.write_str("Math.POSITIVE_INFINITY"),
            Some(value) if value == f64::NEG_INFINITY => out.write_str("Math.NEGATIVE_INFINITY"),
            Some(value) => Display::fmt(&value, out),
            None => write!(out, "0.0 /* invalid float {} */", reference.0),
        },
        Constant::String(reference) => {
            let value = code
                .strings
                .get(reference.0)
                .map(|value| value.as_str())
                .unwrap_or("");
            write!(out, "\"{}\"", escape_haxe_string(value, '"'))
        }
        Constant::Bool(value) => Display::fmt(value, out),
        Constant::Null => out.write_str("null"),
        Constant::This => out.write_str("this"),
    }
}

fn fmt_operation(
    out: &mut Formatter<'_>,
    operation: &Operation,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    macro_rules! binary {
        ($left:expr, $operator:expr, $right:expr) => {{
            let precedence = operation_precedence(operation);
            fmt_expr(
                out,
                $left,
                indent,
                code,
                function,
                ParentExpr::Left(precedence),
            )?;
            write!(out, " {} ", $operator)?;
            fmt_expr(
                out,
                $right,
                indent,
                code,
                function,
                ParentExpr::Right(precedence),
            )
        }};
    }
    match operation {
        Operation::Add(left, right) => binary!(left, "+", right),
        Operation::Sub(left, right) => binary!(left, "-", right),
        Operation::Mul(left, right) => binary!(left, "*", right),
        Operation::Div(left, right) => binary!(left, "/", right),
        Operation::Mod(left, right) => binary!(left, "%", right),
        Operation::Shl(left, right) => binary!(left, "<<", right),
        Operation::Shr(left, right) => binary!(left, ">>", right),
        Operation::And(left, right) => binary!(left, "&&", right),
        Operation::Or(left, right) => binary!(left, "||", right),
        Operation::BitAnd(left, right) => binary!(left, "&", right),
        Operation::BitOr(left, right) => binary!(left, "|", right),
        Operation::Xor(left, right) => binary!(left, "^", right),
        Operation::Eq(left, right) => binary!(left, "==", right),
        Operation::NotEq(left, right) => binary!(left, "!=", right),
        Operation::Gt(left, right) => binary!(left, ">", right),
        Operation::Gte(left, right) => binary!(left, ">=", right),
        Operation::Lt(left, right) => binary!(left, "<", right),
        Operation::Lte(left, right) => binary!(left, "<=", right),
        Operation::Neg(expression) => {
            out.write_str("-")?;
            fmt_expr(
                out,
                expression,
                indent,
                code,
                function,
                ParentExpr::Unary(PREC_UNARY),
            )
        }
        Operation::Not(expression) => {
            out.write_str("!")?;
            fmt_expr(
                out,
                expression,
                indent,
                code,
                function,
                ParentExpr::Unary(PREC_UNARY),
            )
        }
        Operation::Incr(expression) => {
            fmt_expr(
                out,
                expression,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str("++")
        }
        Operation::Decr(expression) => {
            fmt_expr(
                out,
                expression,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str("--")
        }
    }
}

fn enum_constructor_name(code: &Bytecode, ty: RefType, index: usize) -> String {
    match code.types.get(ty.0) {
        Some(Type::Enum { constructs, .. }) => constructs
            .get(index)
            .map(|constructor| escape_identifier(constructor.name(code).as_str()))
            .unwrap_or_else(|| format!("__InvalidEnumConstructor{index}")),
        _ => format!("__InvalidEnumConstructor{index}"),
    }
}

fn function_reference_name(
    code: &Bytecode,
    current: &Function,
    reference: hlbc::types::RefFun,
) -> String {
    match code.safe_get_ref_fun(reference) {
        Some(FunPtr::Native(_)) => format!("__HlNatives.f{}", reference.0),
        Some(FunPtr::Fun(target)) => {
            let name = target.name(code);
            if name == "trace" {
                return "trace".to_owned();
            }
            if target.parent == current.parent {
                return escape_identifier(name.as_str());
            }
            if let Some(parent) = target
                .parent
                .and_then(|parent| code.types.get(parent.0))
                .and_then(Type::get_type_obj)
            {
                let parent_name = source_type_name(parent.name(code).as_str());
                if !parent_name.is_empty() && !name.is_empty() && name != "<none>" {
                    return format!("{parent_name}.{}", escape_identifier(name.as_str()));
                }
            }
            format!("__HlFunctions.f{}", reference.0)
        }
        None => format!("__HlFunctions.f{}", reference.0),
    }
}

fn fmt_memory_load(
    out: &mut Formatter<'_>,
    memory_type: MemoryType,
    bytes: &Expr,
    index: &Expr,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    match memory_type {
        MemoryType::U8 => {
            fmt_expr(
                out,
                bytes,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str("[")?;
            fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
            out.write_str("]")
        }
        MemoryType::U16 => {
            fmt_expr(
                out,
                bytes,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".getUI16(")?;
            fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
            out.write_str(")")
        }
        MemoryType::Typed(ty) => {
            let method = match code.types.get(ty.0) {
                Some(Type::I32) => Some("getI32"),
                Some(Type::F32) => Some("getF32"),
                Some(Type::F64) => Some("getF64"),
                _ => None,
            };
            if let Some(method) = method {
                fmt_expr(
                    out,
                    bytes,
                    indent,
                    code,
                    function,
                    ParentExpr::Postfix(PREC_POSTFIX),
                )?;
                write!(out, ".{method}(")?;
                fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
                out.write_str(")")
            } else {
                out.write_str("__HlRuntime.getMem(")?;
                fmt_expr(out, bytes, indent, code, function, ParentExpr::Root)?;
                out.write_str(", ")?;
                fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
                out.write_str(")")
            }
        }
    }
}

impl Statement {
    pub fn display<'a>(
        &'a self,
        indent: &'a FormatOptions,
        code: &'a Bytecode,
        function: &'a Function,
    ) -> impl Display + 'a {
        StatementDisplay {
            statement: self,
            indent,
            code,
            function,
        }
    }
}

struct StatementDisplay<'a> {
    statement: &'a Statement,
    indent: &'a FormatOptions,
    code: &'a Bytecode,
    function: &'a Function,
}

impl Display for StatementDisplay<'_> {
    fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
        fmt_statement(out, self.statement, self.indent, self.code, self.function)
    }
}

fn fmt_statement(
    out: &mut Formatter<'_>,
    statement: &Statement,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    match statement {
        Statement::VarDecl {
            variable,
            variable_type,
            value,
        } => {
            out.write_str("var ")?;
            fmt_expr(out, variable, indent, code, function, ParentExpr::Root)?;
            write!(out, ": {}", haxe_type(code, *variable_type))?;
            if let Some(value) = value {
                out.write_str(" = ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            }
            out.write_str(";")
        }
        Statement::Assign {
            declaration,
            variable,
            assign,
        } => {
            if *declaration {
                out.write_str("var ")?;
            }
            fmt_expr(out, variable, indent, code, function, ParentExpr::Root)?;
            out.write_str(" = ")?;
            fmt_expr(out, assign, indent, code, function, ParentExpr::Root)?;
            out.write_str(";")
        }
        Statement::ExprStatement(expression) => {
            fmt_expr(out, expression, indent, code, function, ParentExpr::Root)?;
            out.write_str(";")
        }
        Statement::GlobalStore { global, value, .. } => {
            write!(out, "__HlRuntime.setGlobal({}, ", global.0)?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(");")
        }
        Statement::DynamicFieldStore {
            object,
            field,
            value,
        } => {
            out.write_str("Reflect.setField(")?;
            fmt_expr(out, object, indent, code, function, ParentExpr::Root)?;
            let field = code
                .strings
                .get(field.0)
                .map(|field| field.as_str())
                .unwrap_or("");
            write!(out, ", \"{}\", ", escape_haxe_string(field, '"'))?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(");")
        }
        Statement::MemoryStore {
            memory_type,
            bytes,
            index,
            value,
            ..
        } => fmt_memory_store(
            out,
            *memory_type,
            bytes,
            index,
            value,
            indent,
            code,
            function,
        ),
        Statement::ReferenceStore {
            reference, value, ..
        } => {
            fmt_expr(
                out,
                reference,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".set(")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(");")
        }
        Statement::RuntimeCheck(RuntimeCheck::Null(value)) => {
            out.write_str("// HashLink null check: ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)
        }
        Statement::RuntimeCheck(RuntimeCheck::Assert) => {
            out.write_str("// HashLink assertion check")
        }
        Statement::Prefetch { value, field, mode } => {
            out.write_str("// HashLink prefetch ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            write!(out, " field={} mode={mode}", field.0)
        }
        Statement::Nop => out.write_str("// HashLink no-op"),
        Statement::Return(value) => {
            out.write_str("return")?;
            if let Some(value) = value {
                out.write_str(" ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            }
            out.write_str(";")
        }
        Statement::IfElse { cond, if_, else_ } => {
            out.write_str("if (")?;
            fmt_expr(out, cond, indent, code, function, ParentExpr::Root)?;
            out.write_str(") {")?;
            fmt_statement_block(out, if_, indent, code, function)?;
            if !else_.is_empty() {
                if else_.len() == 1 && statement_is_if(&else_[0]) {
                    out.write_str(" else ")?;
                    fmt_statement(out, &else_[0], indent, code, function)?;
                } else {
                    out.write_str(" else {")?;
                    fmt_statement_block(out, else_, indent, code, function)?;
                }
            }
            Ok(())
        }
        Statement::Switch {
            arg,
            default,
            cases,
        } => {
            out.write_str("switch (")?;
            fmt_expr(out, arg, indent, code, function, ParentExpr::Root)?;
            writeln!(out, ") {{")?;
            let case_indent = indent.inc_nesting();
            let body_indent = case_indent.inc_nesting();
            for (patterns, statements) in cases {
                write!(out, "{case_indent}case ")?;
                fmt_expression_list(out, patterns, indent, code, function)?;
                writeln!(out, ":")?;
                if statements.is_empty() {
                    writeln!(out, "{body_indent}{{}}")?;
                } else {
                    for statement in statements {
                        write!(out, "{body_indent}")?;
                        fmt_statement(out, statement, &body_indent, code, function)?;
                        writeln!(out)?;
                    }
                }
            }
            if !default.is_empty() {
                writeln!(out, "{case_indent}default:")?;
                for statement in default {
                    write!(out, "{body_indent}")?;
                    fmt_statement(out, statement, &body_indent, code, function)?;
                    writeln!(out)?;
                }
            }
            write!(out, "{indent}}}")
        }
        Statement::While { cond, stmts } => {
            out.write_str("while (")?;
            fmt_expr(out, cond, indent, code, function, ParentExpr::Root)?;
            out.write_str(") {")?;
            fmt_statement_block(out, stmts, indent, code, function)
        }
        Statement::DoWhile { cond, stmts } => {
            out.write_str("do {")?;
            fmt_statement_block(out, stmts, indent, code, function)?;
            out.write_str(" while (")?;
            fmt_expr(out, cond, indent, code, function, ParentExpr::Root)?;
            out.write_str(");")
        }
        Statement::ForEach {
            variable,
            iterable,
            stmts,
        } => {
            out.write_str("for (")?;
            fmt_expr(out, variable, indent, code, function, ParentExpr::Root)?;
            out.write_str(" in ")?;
            fmt_expr(out, iterable, indent, code, function, ParentExpr::Root)?;
            out.write_str(") {")?;
            fmt_statement_block(out, stmts, indent, code, function)
        }
        Statement::ForRange {
            variable,
            start,
            end,
            stmts,
        } => {
            out.write_str("for (")?;
            fmt_expr(out, variable, indent, code, function, ParentExpr::Root)?;
            out.write_str(" in ")?;
            fmt_expr(out, start, indent, code, function, ParentExpr::Root)?;
            out.write_str("...")?;
            fmt_expr(out, end, indent, code, function, ParentExpr::Root)?;
            out.write_str(") {")?;
            fmt_statement_block(out, stmts, indent, code, function)
        }
        Statement::Break => out.write_str("break;"),
        Statement::Continue => out.write_str("continue;"),
        Statement::Throw(value) => {
            out.write_str("throw ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(";")
        }
        Statement::Try { stmts } => {
            out.write_str("try {")?;
            fmt_statement_block(out, stmts, indent, code, function)
        }
        Statement::Catch { stmts } => {
            out.write_str("catch (__caught: Dynamic) {")?;
            fmt_statement_block(out, stmts, indent, code, function)
        }
        Statement::TryCatch { try_stmts, catches } => {
            out.write_str("try {")?;
            fmt_statement_block(out, try_stmts, indent, code, function)?;
            for catch in catches {
                out.write_str(" catch (")?;
                fmt_expr(
                    out,
                    &catch.variable,
                    indent,
                    code,
                    function,
                    ParentExpr::Root,
                )?;
                write!(out, ": {}) {{", haxe_type(code, catch.variable_type))?;
                fmt_statement_block(out, &catch.stmts, indent, code, function)?;
            }
            Ok(())
        }
        Statement::StateMachine {
            entry_state,
            locals,
            blocks,
        } => {
            for local in locals {
                out.write_str("var ")?;
                fmt_expr(out, local, indent, code, function, ParentExpr::Root)?;
                let variable_type = match raw_expression(local) {
                    Expr::Variable(register, _) => function.regtype(*register),
                    _ => RefType(9),
                };
                writeln!(
                    out,
                    ": {} = {};",
                    haxe_type(code, variable_type),
                    haxe_default_value(code, variable_type)
                )?;
                write!(out, "{indent}")?;
            }
            writeln!(out, "var __hl_state = {entry_state};")?;
            writeln!(out, "{indent}var __hl_running = true;")?;
            writeln!(out, "{indent}while (__hl_running) {{")?;
            let switch_indent = indent.inc_nesting();
            let case_indent = switch_indent.inc_nesting();
            let body_indent = case_indent.inc_nesting();
            writeln!(out, "{switch_indent}switch (__hl_state) {{")?;
            for block in blocks {
                writeln!(out, "{case_indent}case {}:", block.state)?;
                if let Some(exception) = &block.exception {
                    writeln!(out, "{body_indent}try {{")?;
                    let try_indent = body_indent.inc_nesting();
                    for statement in &block.stmts {
                        write!(out, "{try_indent}")?;
                        fmt_statement(out, statement, &try_indent, code, function)?;
                        writeln!(out)?;
                    }
                    write!(out, "{try_indent}")?;
                    fmt_terminator(out, &block.terminator, &try_indent, code, function)?;
                    writeln!(out)?;
                    write!(out, "{body_indent}}}")?;
                    for variable_type in &exception.variable_types {
                        writeln!(
                            out,
                            " catch (__hl_caught_{}: {}) {{",
                            block.state,
                            haxe_type(code, *variable_type)
                        )?;
                        write!(out, "{try_indent}")?;
                        fmt_expr(
                            out,
                            &exception.variable,
                            indent,
                            code,
                            function,
                            ParentExpr::Root,
                        )?;
                        writeln!(out, " = __hl_caught_{};", block.state)?;
                        writeln!(out, "{try_indent}__hl_state = {};", exception.handler_state)?;
                        write!(out, "{body_indent}}}")?;
                    }
                    writeln!(out)?;
                } else {
                    for statement in &block.stmts {
                        write!(out, "{body_indent}")?;
                        fmt_statement(out, statement, &body_indent, code, function)?;
                        writeln!(out)?;
                    }
                    write!(out, "{body_indent}")?;
                    fmt_terminator(out, &block.terminator, &body_indent, code, function)?;
                    writeln!(out)?;
                }
            }
            writeln!(out, "{case_indent}default:")?;
            writeln!(out, "{body_indent}__hl_running = false;")?;
            writeln!(out, "{switch_indent}}}")?;
            write!(out, "{indent}}}")
        }
        Statement::Comment(comment) => write!(out, "// {}", escape_comment(comment)),
        Statement::UnhandledOpcode { opcode, provenance } => write!(
            out,
            "// unsupported opcode f{} @{}: {} {}",
            provenance.function_index,
            provenance.opcode_start,
            opcode.name(),
            escape_comment(&format!("{opcode:?}"))
        ),
        Statement::Provenanced {
            statement: inner, ..
        } => fmt_statement(out, inner, indent, code, function),
    }
}

fn fmt_statement_block(
    out: &mut Formatter<'_>,
    statements: &[Statement],
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    if statements.is_empty() {
        return out.write_str("}");
    }
    writeln!(out)?;
    let nested = indent.inc_nesting();
    for statement in statements {
        write!(out, "{nested}")?;
        fmt_statement(out, statement, &nested, code, function)?;
        writeln!(out)?;
    }
    write!(out, "{indent}}}")
}

#[allow(clippy::too_many_arguments)]
fn fmt_memory_store(
    out: &mut Formatter<'_>,
    memory_type: MemoryType,
    bytes: &Expr,
    index: &Expr,
    value: &Expr,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    match memory_type {
        MemoryType::U8 => {
            fmt_expr(
                out,
                bytes,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str("[")?;
            fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
            out.write_str("] = ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(";")
        }
        MemoryType::U16 => {
            fmt_expr(
                out,
                bytes,
                indent,
                code,
                function,
                ParentExpr::Postfix(PREC_POSTFIX),
            )?;
            out.write_str(".setUI16(")?;
            fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
            out.write_str(", ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(");")
        }
        MemoryType::Typed(ty) => {
            let method = match code.types.get(ty.0) {
                Some(Type::I32) => Some("setI32"),
                Some(Type::F32) => Some("setF32"),
                Some(Type::F64) => Some("setF64"),
                _ => None,
            };
            if let Some(method) = method {
                fmt_expr(
                    out,
                    bytes,
                    indent,
                    code,
                    function,
                    ParentExpr::Postfix(PREC_POSTFIX),
                )?;
                write!(out, ".{method}(")?;
                fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
                out.write_str(", ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
                out.write_str(");")
            } else {
                out.write_str("__HlRuntime.setMem(")?;
                fmt_expr(out, bytes, indent, code, function, ParentExpr::Root)?;
                out.write_str(", ")?;
                fmt_expr(out, index, indent, code, function, ParentExpr::Root)?;
                out.write_str(", ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
                out.write_str(");")
            }
        }
    }
}

fn fmt_terminator(
    out: &mut Formatter<'_>,
    terminator: &StateTerminator,
    indent: &FormatOptions,
    code: &Bytecode,
    function: &Function,
) -> fmt::Result {
    match terminator {
        StateTerminator::Goto(state) => write!(out, "__hl_state = {state};"),
        StateTerminator::Branch {
            cond,
            taken,
            fallthrough,
        } => {
            out.write_str("if (")?;
            fmt_expr(out, cond, indent, code, function, ParentExpr::Root)?;
            writeln!(out, ") {{")?;
            let nested = indent.inc_nesting();
            writeln!(out, "{nested}__hl_state = {taken};")?;
            writeln!(out, "{indent}}} else {{")?;
            writeln!(out, "{nested}__hl_state = {fallthrough};")?;
            write!(out, "{indent}}}")
        }
        StateTerminator::Switch {
            arg,
            cases,
            default,
        } => {
            out.write_str("switch (")?;
            fmt_expr(out, arg, indent, code, function, ParentExpr::Root)?;
            writeln!(out, ") {{")?;
            let nested = indent.inc_nesting();
            for (case, state) in cases {
                writeln!(out, "{nested}case {case}: __hl_state = {state};")?;
            }
            writeln!(out, "{nested}default: __hl_state = {default};")?;
            write!(out, "{indent}}}")
        }
        StateTerminator::Return(value) => {
            out.write_str("return")?;
            if let Some(value) = value {
                out.write_str(" ")?;
                fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            }
            out.write_str(";")
        }
        StateTerminator::Throw(value) => {
            out.write_str("throw ")?;
            fmt_expr(out, value, indent, code, function, ParentExpr::Root)?;
            out.write_str(";")
        }
        StateTerminator::Exit => out.write_str("__hl_running = false;"),
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

#[cfg(test)]
mod tests {
    use std::fmt::{self, Display, Formatter};

    use hlbc::types::Reg;

    use super::*;

    struct ExpressionDisplay<'a> {
        expression: &'a Expr,
        code: &'a Bytecode,
        function: &'a Function,
    }

    impl Display for ExpressionDisplay<'_> {
        fn fmt(&self, out: &mut Formatter<'_>) -> fmt::Result {
            fmt_expr(
                out,
                self.expression,
                &FormatOptions::new(4),
                self.code,
                self.function,
                ParentExpr::Root,
            )
        }
    }

    fn fixture() -> (Bytecode, Function) {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let function = code.function_by_name("main").unwrap().clone();
        (code, function)
    }

    fn variable(register: u32, name: &str) -> Expr {
        Expr::Variable(Reg(register), Some(name.into()))
    }

    fn render(expression: &Expr, code: &Bytecode, function: &Function) -> String {
        ExpressionDisplay {
            expression,
            code,
            function,
        }
        .to_string()
    }

    #[test]
    fn expression_precedence_uses_only_required_parentheses() {
        let (code, function) = fixture();
        let a = variable(0, "a");
        let b = variable(1, "b");
        let c = variable(2, "c");

        let multiply_sum = Expr::Op(Operation::Mul(
            Box::new(Expr::Op(Operation::Add(
                Box::new(a.clone()),
                Box::new(b.clone()),
            ))),
            Box::new(c.clone()),
        ));
        assert_eq!(render(&multiply_sum, &code, &function), "(a + b) * c");

        let mixed = Expr::Op(Operation::Add(
            Box::new(a.clone()),
            Box::new(Expr::Op(Operation::Mul(
                Box::new(b.clone()),
                Box::new(c.clone()),
            ))),
        ));
        assert_eq!(render(&mixed, &code, &function), "a + b * c");

        let right_nested_subtraction = Expr::Op(Operation::Sub(
            Box::new(a),
            Box::new(Expr::Op(Operation::Sub(Box::new(b), Box::new(c)))),
        ));
        assert_eq!(
            render(&right_nested_subtraction, &code, &function),
            "a - (b - c)"
        );
    }

    #[test]
    fn haxe_escaping_handles_keywords_controls_and_interpolation() {
        assert_eq!(escape_identifier("class"), "class_");
        assert_eq!(escape_identifier("9-value"), "__value");
        assert_eq!(source_type_name("game.model.Player"), "Player");
        assert_eq!(source_type_name("haxe.io.Bytes"), "haxe.io.Bytes");
        assert_eq!(
            escape_haxe_string("\0\n'$\\\u{2028}", '\''),
            "\\x00\\n\\'$$\\\\\\u{2028}"
        );
    }

    #[test]
    fn every_fixture_type_has_an_explicit_haxe_representation() {
        let (code, _) = fixture();
        for index in 0..code.types.len() {
            let formatted = haxe_type(&code, RefType(index)).to_string();
            assert!(!formatted.is_empty(), "type t{index} formatted as empty");
            assert!(
                !formatted.contains("other"),
                "type t{index} used the removed generic representation: {formatted}"
            );
        }
        assert!(matches!(
            haxe_type(&code, RefType(usize::MAX)),
            HaxeType::Diagnostic(_)
        ));
    }

    #[test]
    fn recursive_hashlink_types_use_an_explicit_non_recursive_fallback() {
        let mut code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let reference = RefType(code.types.len());
        code.types.push(Type::Ref(reference));
        let rendered = haxe_type(&code, reference).to_string();
        assert!(rendered.contains("hl.Ref<Dynamic"));
        assert!(rendered.contains("recursive HashLink type"));
    }
}

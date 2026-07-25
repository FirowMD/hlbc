//! Project-level dependency discovery and deterministic Haxe package emission.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use hlbc::types::{Function, RefField, RefType, Type, TypeObj};
use hlbc::{Bytecode, Resolve};
use serde::{Deserialize, Serialize};

use crate::ast::{Class, ClassField, Method};
use crate::cache::AnalysisCache;
use crate::diagnostics::{
    DecompileOptions, Diagnostic, Provenance, RecoveredConstruct, RecoveryAnnotation,
};
use crate::fmt::{display_type_declaration, escape_identifier, FormatOptions};
use crate::interprocedural::{analyze_program_with_cache, AnalysisConfig, ProgramAnalysis};
use crate::method_overrides_parent;
use crate::parallel::{
    decompile_functions_parallel, FunctionArtifact, ParallelDecompilation, ParallelOptions,
};

pub const PROJECT_SCHEMA_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProjectOptions {
    pub decompile: DecompileOptions,
    pub analysis: AnalysisConfig,
    pub parallel: ParallelOptions,
    /// HashLink function indices used as discovery roots. Empty means the
    /// bytecode entrypoint.
    pub roots: BTreeSet<usize>,
    pub include_all_types: bool,
    pub write_reports: bool,
}

impl Default for ProjectOptions {
    fn default() -> Self {
        Self {
            decompile: DecompileOptions::default(),
            analysis: AnalysisConfig::default(),
            parallel: ParallelOptions::default(),
            roots: BTreeSet::new(),
            include_all_types: false,
            write_reports: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DeclarationKind {
    Class,
    Struct,
    Enum,
    Abstract,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProjectUnit {
    pub type_index: usize,
    pub kind: DeclarationKind,
    pub qualified_name: String,
    pub package: String,
    pub declaration_name: String,
    pub relative_path: PathBuf,
    pub dependencies: BTreeSet<usize>,
    pub functions: BTreeSet<usize>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct ProjectGraph {
    pub units: BTreeMap<usize, ProjectUnit>,
    /// Dependencies precede dependants; cycles are broken by type index.
    pub stable_order: Vec<usize>,
    pub reachable_functions: BTreeSet<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GeneratedFile {
    pub relative_path: PathBuf,
    pub bytes: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct ProjectOutput {
    pub output_directory: PathBuf,
    pub graph: ProjectGraph,
    pub analysis: ProgramAnalysis,
    pub diagnostics: Vec<Diagnostic>,
    pub recovery_annotations: Vec<RecoveryAnnotation>,
    pub generated_files: Vec<GeneratedFile>,
    pub workers_used: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum ProjectError {
    #[error("project output I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("project report serialization failed: {0}")]
    Json(#[from] serde_json::Error),
    #[error("project discovery failed: {0}")]
    Discovery(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ProjectManifest {
    schema_version: u32,
    generated_files: Vec<PathBuf>,
}

pub fn discover_project(code: &Bytecode, options: &ProjectOptions) -> ProjectGraph {
    let analysis = crate::interprocedural::analyze_program(code, options.analysis);
    discover_project_with_analysis(code, options, &analysis)
}

pub fn decompile_project(
    code: &Bytecode,
    output_directory: impl AsRef<Path>,
    options: ProjectOptions,
) -> Result<ProjectOutput, ProjectError> {
    let mut cache = AnalysisCache::new();
    decompile_project_with_cache(code, output_directory, options, &mut cache)
}

pub fn decompile_project_with_cache(
    code: &Bytecode,
    output_directory: impl AsRef<Path>,
    options: ProjectOptions,
    cache: &mut AnalysisCache,
) -> Result<ProjectOutput, ProjectError> {
    let output_directory = output_directory.as_ref().to_path_buf();
    let analysis = analyze_program_with_cache(code, options.analysis, cache);
    let graph = discover_project_with_analysis(code, &options, &analysis);
    let parallel = decompile_functions_parallel(
        code,
        &graph.reachable_functions,
        options.decompile,
        options.parallel,
    );
    let mut diagnostics = parallel.diagnostics.clone();
    let mut recovery_annotations = parallel.recovery_annotations.clone();
    recovery_annotations.extend(analysis.annotations.iter().cloned());
    for unit in graph.units.values() {
        recovery_annotations.push(RecoveryAnnotation::exact(
            RecoveredConstruct::GeneratedDeclaration,
            Provenance::new(unit.functions.iter().next().copied().unwrap_or(0), 0, 0),
            "project-layout",
        ));
    }
    sort_diagnostics(&mut diagnostics);
    sort_annotations(&mut recovery_annotations);

    fs::create_dir_all(&output_directory)?;
    remove_stale_generated_files(&output_directory)?;
    let source_root = output_directory.join("src");
    fs::create_dir_all(&source_root)?;
    validate_unique_paths(&graph)?;
    let project_types: BTreeSet<_> = graph.units.keys().copied().collect();
    let static_owners = static_type_owners(code);
    let mut generated = Vec::new();

    for type_index in &graph.stable_order {
        let Some(unit) = graph.units.get(type_index) else {
            continue;
        };
        let source = match code.types.get(*type_index) {
            Some(Type::Obj(object)) | Some(Type::Struct(object)) => {
                let class = assemble_class(
                    code,
                    RefType(*type_index),
                    object,
                    &parallel,
                    &static_owners,
                    &mut diagnostics,
                )?;
                render_class_source(code, unit, &class, &project_types)
            }
            Some(Type::Enum { .. }) | Some(Type::Abstract { .. }) => render_type_source(code, unit),
            _ => continue,
        };
        write_generated(
            &output_directory,
            &unit.relative_path,
            source.as_bytes(),
            &mut generated,
        )?;
    }

    let compile_all = render_compile_all(&graph);
    write_generated(
        &output_directory,
        Path::new("src").join("__Bytesto4tCompileAll.hx").as_path(),
        compile_all.as_bytes(),
        &mut generated,
    )?;
    let hxml = "-cp src\n-main __Bytesto4tCompileAll\n-hl recovered.hl\n";
    write_generated(
        &output_directory,
        Path::new("build.hxml"),
        hxml.as_bytes(),
        &mut generated,
    )?;

    sort_diagnostics(&mut diagnostics);
    if options.write_reports {
        let analysis_json = serde_json::to_vec_pretty(&analysis)?;
        write_generated(
            &output_directory,
            Path::new("reports").join("analysis.json").as_path(),
            &analysis_json,
            &mut generated,
        )?;
        let diagnostics_json = serde_json::to_vec_pretty(&diagnostics)?;
        write_generated(
            &output_directory,
            Path::new("reports").join("diagnostics.json").as_path(),
            &diagnostics_json,
            &mut generated,
        )?;
        let graph_json = serde_json::to_vec_pretty(&graph)?;
        write_generated(
            &output_directory,
            Path::new("reports").join("project-graph.json").as_path(),
            &graph_json,
            &mut generated,
        )?;
    }
    generated.sort_by(|left, right| left.relative_path.cmp(&right.relative_path));
    let manifest = ProjectManifest {
        schema_version: PROJECT_SCHEMA_VERSION,
        generated_files: generated
            .iter()
            .map(|file| file.relative_path.clone())
            .collect(),
    };
    let manifest_bytes = serde_json::to_vec_pretty(&manifest)?;
    fs::write(
        output_directory.join("bytesto4t-project.json"),
        &manifest_bytes,
    )?;
    generated.push(GeneratedFile {
        relative_path: PathBuf::from("bytesto4t-project.json"),
        bytes: manifest_bytes.len(),
    });
    generated.sort_by(|left, right| left.relative_path.cmp(&right.relative_path));

    Ok(ProjectOutput {
        output_directory,
        graph,
        analysis,
        diagnostics,
        recovery_annotations,
        generated_files: generated,
        workers_used: parallel.workers_used,
    })
}

fn discover_project_with_analysis(
    code: &Bytecode,
    options: &ProjectOptions,
    analysis: &ProgramAnalysis,
) -> ProjectGraph {
    let static_owners = static_type_owners(code);
    let mut functions = if options.roots.is_empty() {
        BTreeSet::from([code.main().map_or(code.entrypoint.0, |main| main.findex.0)])
    } else {
        options.roots.clone()
    };
    functions.retain(|index| analysis.functions.contains_key(index));
    let mut types = BTreeSet::new();

    if options.include_all_types {
        for type_index in 0..code.types.len() {
            if declaration_info(code, RefType(type_index), &static_owners).is_some() {
                types.insert(type_index);
            }
        }
    }

    loop {
        let before = (functions.len(), types.len());
        for function_index in functions.clone() {
            let Some(function) = function_by_index(code, function_index) else {
                continue;
            };
            if let Some(parent) = function.parent {
                types.insert(normalize_type(parent.0, &static_owners));
            }
            for reference in function
                .regs
                .iter()
                .copied()
                .chain(std::iter::once(function.t))
            {
                collect_declaration_types(code, reference, &static_owners, &mut types);
            }
            if let Some(summary) = analysis.function(function_index) {
                functions.extend(
                    summary
                        .direct_dependencies
                        .iter()
                        .copied()
                        .filter(|target| analysis.functions.contains_key(target)),
                );
            }
        }
        for type_index in types.clone() {
            collect_type_dependencies(code, RefType(type_index), &static_owners, &mut types);
            functions.extend(type_functions(code, RefType(type_index)));
        }
        functions.retain(|index| analysis.functions.contains_key(index));
        if before == (functions.len(), types.len()) {
            break;
        }
    }

    let mut units = BTreeMap::new();
    for type_index in types {
        let reference = RefType(type_index);
        let Some((kind, qualified_name)) = declaration_info(code, reference, &static_owners) else {
            continue;
        };
        let (package, declaration_name) = split_type_path(&qualified_name);
        let mut dependencies = BTreeSet::new();
        collect_direct_type_dependencies(code, reference, &static_owners, &mut dependencies);
        dependencies.retain(|dependency| *dependency != type_index);
        let unit_functions = type_functions(code, reference)
            .into_iter()
            .filter(|function| functions.contains(function))
            .collect();
        let relative_path = source_path(&package, &declaration_name);
        units.insert(
            type_index,
            ProjectUnit {
                type_index,
                kind,
                qualified_name,
                package,
                declaration_name,
                relative_path,
                dependencies,
                functions: unit_functions,
            },
        );
    }
    let owned: BTreeSet<_> = units.keys().copied().collect();
    for unit in units.values_mut() {
        unit.dependencies
            .retain(|dependency| owned.contains(dependency));
    }
    let stable_order = stable_unit_order(&units);
    ProjectGraph {
        units,
        stable_order,
        reachable_functions: functions,
    }
}

fn declaration_info(
    code: &Bytecode,
    reference: RefType,
    static_owners: &BTreeMap<usize, usize>,
) -> Option<(DeclarationKind, String)> {
    if static_owners.contains_key(&reference.0) {
        return None;
    }
    let (kind, name) = match code.types.get(reference.0)? {
        Type::Obj(object) => (DeclarationKind::Class, object.name(code).to_string()),
        Type::Struct(object) => (DeclarationKind::Struct, object.name(code).to_string()),
        Type::Enum { name, .. } => (DeclarationKind::Enum, code.get(*name).to_string()),
        Type::Abstract { name } => (DeclarationKind::Abstract, code.get(*name).to_string()),
        _ => return None,
    };
    is_project_type_name(&name).then_some((kind, name))
}

fn is_project_type_name(name: &str) -> bool {
    let declaration = name.rsplit('.').next().unwrap_or(name);
    !name.is_empty()
        && name != "<none>"
        && !name.starts_with("hl_")
        && declaration
            .chars()
            .next()
            .map_or(false, |character| character.is_uppercase())
        && ![
            "cpp.", "cs.", "eval.", "flash.", "haxe.", "hl.", "java.", "js.", "lua.", "neko.",
            "php.", "python.", "sys.", "wasi.",
        ]
        .iter()
        .any(|prefix| name.starts_with(prefix))
        && !matches!(
            name,
            "Any"
                | "Array"
                | "Class"
                | "Date"
                | "EReg"
                | "Enum"
                | "EnumValue"
                | "IntIterator"
                | "Lambda"
                | "List"
                | "Map"
                | "Math"
                | "Reflect"
                | "Std"
                | "String"
                | "StringBuf"
                | "StringTools"
                | "Sys"
                | "Type"
                | "UInt"
                | "UnicodeString"
                | "ValueType"
                | "Xml"
                | "XmlType"
        )
}

fn static_type_owners(code: &Bytecode) -> BTreeMap<usize, usize> {
    let mut owners = BTreeMap::new();
    for (type_index, ty) in code.types.iter().enumerate() {
        let Some(object) = ty.get_type_obj() else {
            continue;
        };
        let Some(global) = object.global.0.checked_sub(1) else {
            continue;
        };
        if let Some(static_type) = code.globals.get(global) {
            owners.insert(static_type.0, type_index);
        }
    }
    owners
}

fn normalize_type(type_index: usize, static_owners: &BTreeMap<usize, usize>) -> usize {
    static_owners
        .get(&type_index)
        .copied()
        .unwrap_or(type_index)
}

fn collect_declaration_types(
    code: &Bytecode,
    reference: RefType,
    static_owners: &BTreeMap<usize, usize>,
    output: &mut BTreeSet<usize>,
) {
    let normalized = normalize_type(reference.0, static_owners);
    if declaration_info(code, RefType(normalized), static_owners).is_some() {
        output.insert(normalized);
    }
    match code.types.get(reference.0) {
        Some(Type::Ref(inner) | Type::Null(inner) | Type::Packed(inner)) => {
            collect_declaration_types(code, *inner, static_owners, output)
        }
        Some(Type::Fun(signature) | Type::Method(signature)) => {
            for argument in &signature.args {
                collect_declaration_types(code, *argument, static_owners, output);
            }
            collect_declaration_types(code, signature.ret, static_owners, output);
        }
        Some(Type::Virtual { fields }) => {
            for field in fields {
                collect_declaration_types(code, field.t, static_owners, output);
            }
        }
        _ => {}
    }
}

fn collect_type_dependencies(
    code: &Bytecode,
    reference: RefType,
    static_owners: &BTreeMap<usize, usize>,
    output: &mut BTreeSet<usize>,
) {
    let mut direct = BTreeSet::new();
    collect_direct_type_dependencies(code, reference, static_owners, &mut direct);
    output.extend(direct);
}

fn collect_direct_type_dependencies(
    code: &Bytecode,
    reference: RefType,
    static_owners: &BTreeMap<usize, usize>,
    output: &mut BTreeSet<usize>,
) {
    match code.types.get(reference.0) {
        Some(Type::Obj(object) | Type::Struct(object)) => {
            if let Some(parent) = object.super_ {
                collect_declaration_types(code, parent, static_owners, output);
            }
            for field in &object.own_fields {
                collect_declaration_types(code, field.t, static_owners, output);
            }
            for function_index in type_functions(code, reference) {
                if let Some(function) = function_by_index(code, function_index) {
                    collect_declaration_types(code, function.t, static_owners, output);
                }
            }
        }
        Some(Type::Enum { constructs, .. }) => {
            for construct in constructs {
                for parameter in &construct.params {
                    collect_declaration_types(code, *parameter, static_owners, output);
                }
            }
        }
        _ => {}
    }
}

fn type_functions(code: &Bytecode, reference: RefType) -> BTreeSet<usize> {
    let mut functions = BTreeSet::new();
    let Some(object) = code.types.get(reference.0).and_then(Type::get_type_obj) else {
        return functions;
    };
    functions.extend(object.bindings.values().map(|function| function.0));
    functions.extend(object.protos.iter().map(|prototype| prototype.findex.0));
    if let Some(static_type) = object.get_static_type(code) {
        functions.extend(static_type.bindings.values().map(|function| function.0));
        functions.extend(
            static_type
                .protos
                .iter()
                .map(|prototype| prototype.findex.0),
        );
    }
    functions.extend(
        code.functions
            .iter()
            .filter(|function| function.parent == Some(reference))
            .map(|function| function.findex.0),
    );
    functions
}

fn stable_unit_order(units: &BTreeMap<usize, ProjectUnit>) -> Vec<usize> {
    fn visit(
        type_index: usize,
        units: &BTreeMap<usize, ProjectUnit>,
        active: &mut BTreeSet<usize>,
        visited: &mut BTreeSet<usize>,
        output: &mut Vec<usize>,
    ) {
        if visited.contains(&type_index) || !active.insert(type_index) {
            return;
        }
        if let Some(unit) = units.get(&type_index) {
            for dependency in &unit.dependencies {
                visit(*dependency, units, active, visited, output);
            }
        }
        active.remove(&type_index);
        if visited.insert(type_index) {
            output.push(type_index);
        }
    }
    let mut output = Vec::new();
    let mut active = BTreeSet::new();
    let mut visited = BTreeSet::new();
    for &type_index in units.keys() {
        visit(type_index, units, &mut active, &mut visited, &mut output);
    }
    output
}

fn assemble_class(
    code: &Bytecode,
    object_type: RefType,
    object: &TypeObj,
    parallel: &ParallelDecompilation,
    static_owners: &BTreeMap<usize, usize>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Class, ProjectError> {
    let static_type = object.get_static_type(code);
    let mut fields = object_fields(code, object, false);
    if let Some(static_type) = static_type {
        fields.extend(object_fields(code, static_type, true));
    }
    let mut method_refs: Vec<_> = object
        .bindings
        .values()
        .copied()
        .map(|function| (function, false, true))
        .collect();
    method_refs.extend(
        object
            .protos
            .iter()
            .map(|prototype| (prototype.findex, false, false)),
    );
    if let Some(static_type) = static_type {
        method_refs.extend(
            static_type
                .bindings
                .values()
                .copied()
                .map(|function| (function, true, false)),
        );
    }
    method_refs.extend(
        code.functions
            .iter()
            .filter(|function| {
                function.parent == Some(object_type)
                    && function.name(code).as_ref() == "__constructor__"
            })
            .map(|function| (function.findex, false, false)),
    );
    method_refs.sort_by_key(|(function, static_, dynamic)| (function.0, *static_, *dynamic));
    method_refs.dedup();
    let mut methods = Vec::new();
    for (reference, static_, dynamic) in method_refs {
        let Some(function) = reference.as_fn(code) else {
            diagnostics.push(Diagnostic::fatal(
                reference.0,
                format!("missing project method function {}", reference.0),
            ));
            continue;
        };
        let Some(artifact) = parallel.function(reference.0) else {
            diagnostics.push(Diagnostic::fatal(
                reference.0,
                format!("project method {} was not decompiled", reference.0),
            ));
            continue;
        };
        methods.push(method_from_artifact(
            code, object, function, artifact, static_, dynamic,
        ));
    }
    let name = code.strings.get(object.name.0).cloned().ok_or_else(|| {
        ProjectError::Discovery(format!(
            "class name string {} is out of bounds",
            object.name.0
        ))
    })?;
    let parent = object.super_.and_then(|reference| {
        let normalized = normalize_type(reference.0, static_owners);
        code.types
            .get(normalized)
            .and_then(Type::get_type_obj)
            .map(|parent| parent.name(code))
    });
    Ok(Class {
        ty: Some(object_type),
        name,
        parent,
        fields,
        methods,
    })
}

fn object_fields(code: &Bytecode, object: &TypeObj, static_: bool) -> Vec<ClassField> {
    object
        .own_fields
        .iter()
        .enumerate()
        .filter_map(|(index, field)| {
            let field_index = if object.fields.len() >= object.own_fields.len() {
                index + object.fields.len() - object.own_fields.len()
            } else {
                index
            };
            (!object.bindings.contains_key(&RefField(field_index))).then(|| ClassField {
                name: field.name(code),
                ty: field.t,
                static_,
            })
        })
        .collect()
}

fn method_from_artifact(
    code: &Bytecode,
    object: &TypeObj,
    function: &Function,
    artifact: &FunctionArtifact,
    static_: bool,
    dynamic: bool,
) -> Method {
    let constructor = function.name(code).as_ref() == "__constructor__";
    Method {
        fun: function.findex,
        static_: static_ && !constructor,
        dynamic,
        constructor,
        override_: !constructor && method_overrides_parent(code, object, function),
        statements: artifact.statements.clone(),
    }
}

fn render_class_source(
    code: &Bytecode,
    unit: &ProjectUnit,
    class: &Class,
    project_types: &BTreeSet<usize>,
) -> String {
    let mut source = package_header(&unit.package);
    source.push_str(
        &class
            .display_for_project(code, &FormatOptions::new(4), project_types)
            .to_string(),
    );
    source.push('\n');
    source
}

fn render_type_source(code: &Bytecode, unit: &ProjectUnit) -> String {
    let mut source = package_header(&unit.package);
    source.push_str(
        &display_type_declaration(code, RefType(unit.type_index), &FormatOptions::new(4))
            .to_string(),
    );
    source.push('\n');
    source
}

fn package_header(package: &str) -> String {
    if package.is_empty() {
        "package;\n\n".to_owned()
    } else {
        format!("package {package};\n\n")
    }
}

fn render_compile_all(graph: &ProjectGraph) -> String {
    let mut source = String::from("package;\n\nclass __Bytesto4tCompileAll {\n");
    for (position, type_index) in graph.stable_order.iter().enumerate() {
        let Some(unit) = graph.units.get(type_index) else {
            continue;
        };
        source.push_str(&format!(
            "    static var type{position}: {};\n",
            unit.qualified_name
                .split('.')
                .map(escape_identifier)
                .collect::<Vec<_>>()
                .join(".")
        ));
    }
    source.push_str("\n    static function main(): Void {}\n}\n");
    source
}

fn split_type_path(qualified_name: &str) -> (String, String) {
    let mut parts: Vec<_> = qualified_name
        .split('.')
        .filter(|part| !part.is_empty())
        .map(escape_identifier)
        .collect();
    let name = parts.pop().unwrap_or_else(|| "_".to_owned());
    (parts.join("."), name)
}

fn source_path(package: &str, declaration_name: &str) -> PathBuf {
    let mut path = PathBuf::from("src");
    for component in package.split('.').filter(|component| !component.is_empty()) {
        path.push(component);
    }
    path.push(format!("{declaration_name}.hx"));
    path
}

fn function_by_index(code: &Bytecode, function_index: usize) -> Option<&Function> {
    code.functions
        .iter()
        .find(|function| function.findex.0 == function_index)
}

fn write_generated(
    output_directory: &Path,
    relative_path: &Path,
    bytes: &[u8],
    generated: &mut Vec<GeneratedFile>,
) -> Result<(), std::io::Error> {
    let path = output_directory.join(relative_path);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&path, bytes)?;
    generated.push(GeneratedFile {
        relative_path: relative_path.to_path_buf(),
        bytes: bytes.len(),
    });
    Ok(())
}

fn validate_unique_paths(graph: &ProjectGraph) -> Result<(), ProjectError> {
    let mut owners = BTreeMap::new();
    for unit in graph.units.values() {
        if let Some(previous) = owners.insert(unit.relative_path.clone(), unit.type_index) {
            return Err(ProjectError::Discovery(format!(
                "types {previous} and {} normalize to the same source path {}",
                unit.type_index,
                unit.relative_path.display()
            )));
        }
    }
    Ok(())
}

fn remove_stale_generated_files(output_directory: &Path) -> Result<(), std::io::Error> {
    let manifest_path = output_directory.join("bytesto4t-project.json");
    let Ok(bytes) = fs::read(&manifest_path) else {
        return Ok(());
    };
    let Ok(manifest) = serde_json::from_slice::<ProjectManifest>(&bytes) else {
        return Ok(());
    };
    for relative in manifest.generated_files {
        if relative.is_absolute()
            || relative
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir))
        {
            continue;
        }
        let path = output_directory.join(relative);
        if path.is_file() {
            fs::remove_file(path)?;
        }
    }
    Ok(())
}

fn sort_diagnostics(diagnostics: &mut [Diagnostic]) {
    diagnostics.sort_by(|left, right| {
        (
            left.function_index,
            left.opcode_index,
            left.severity as u8,
            &left.opcode_name,
            &left.message,
        )
            .cmp(&(
                right.function_index,
                right.opcode_index,
                right.severity as u8,
                &right.opcode_name,
                &right.message,
            ))
    });
}

fn sort_annotations(annotations: &mut Vec<RecoveryAnnotation>) {
    annotations.sort_by(|left, right| {
        (
            left.provenance.function_index,
            left.provenance.opcode_start,
            left.provenance.opcode_end,
            left.construct,
            &left.producer,
        )
            .cmp(&(
                right.provenance.function_index,
                right.provenance.opcode_start,
                right.provenance.opcode_end,
                right.construct,
                &right.producer,
            ))
    });
    annotations.dedup();
}

#[cfg(test)]
mod tests {
    use std::process::Command;

    use hlbc::Bytecode;
    use tempfile::tempdir;

    use super::{decompile_project, discover_project, ProjectOptions};

    #[test]
    fn discovery_and_project_output_are_stable() {
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let options = ProjectOptions {
            include_all_types: true,
            ..ProjectOptions::default()
        };
        let graph = discover_project(&code, &options);
        let directory = tempdir().unwrap();
        let first = decompile_project(&code, directory.path(), options.clone()).unwrap();
        let first_manifest =
            std::fs::read(directory.path().join("bytesto4t-project.json")).unwrap();
        let second = decompile_project(&code, directory.path(), options).unwrap();
        let second_manifest =
            std::fs::read(directory.path().join("bytesto4t-project.json")).unwrap();
        assert_eq!(graph, first.graph);
        assert_eq!(first.graph, second.graph);
        assert_eq!(first_manifest, second_manifest);
        assert!(first
            .generated_files
            .iter()
            .any(|file| file.relative_path.ends_with("build.hxml")));
    }

    #[test]
    fn generated_directory_compiles_when_haxe_is_available() {
        if Command::new("haxe").arg("--version").output().is_err() {
            return;
        }
        let code = Bytecode::from_file("../../data/Empty.hl").unwrap();
        let directory = tempdir().unwrap();
        decompile_project(
            &code,
            directory.path(),
            ProjectOptions {
                include_all_types: false,
                ..ProjectOptions::default()
            },
        )
        .unwrap();
        let output = Command::new("haxe")
            .arg("build.hxml")
            .current_dir(directory.path())
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

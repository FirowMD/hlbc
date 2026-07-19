use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::Duration;

use hlbc::types::{Function, Type, TypeObj};
use hlbc::Bytecode;
use serde::Serialize;
use wait_timeout::ChildExt;

use crate::cfg::ControlFlowGraph;
use crate::diagnostics::Diagnostic;
use crate::fmt::FormatOptions;
use crate::{decompile_class, decompile_function};

#[derive(Debug, thiserror::Error)]
pub enum HarnessError {
    #[error("fixture discovery found zero .hl files in {0}; compile data/*.hx before running the decompiler tests")]
    NoFixtures(PathBuf),
    #[error("{0}")]
    Message(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
}

#[derive(Debug, Clone)]
pub struct HarnessOptions {
    pub fixtures_dir: PathBuf,
    pub output_dir: PathBuf,
    pub goldens_dir: PathBuf,
    pub report_path: PathBuf,
    pub update_goldens: bool,
    pub golden_fixtures: BTreeSet<String>,
    pub hlboot: Option<PathBuf>,
    pub timeout: Duration,
    pub execute: bool,
}

impl Default for HarnessOptions {
    fn default() -> Self {
        Self {
            fixtures_dir: PathBuf::from("data"),
            output_dir: PathBuf::from("target/milestone"),
            goldens_dir: PathBuf::from("crates/decompiler/tests/goldens"),
            report_path: PathBuf::from("target/milestone-report.json"),
            update_goldens: false,
            golden_fixtures: BTreeSet::from(["Empty".to_owned()]),
            hlboot: std::env::var_os("HLBC_HLBOOT").map(PathBuf::from),
            timeout: Duration::from_secs(5),
            execute: true,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FailureRecord {
    pub fixture: String,
    pub message: String,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct FixtureCounts {
    pub discovered: usize,
    pub parsed: usize,
    pub decompiled: usize,
    pub recompiled: usize,
    pub executed: usize,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct OpcodeFidelity {
    pub functions_compared: usize,
    pub exact_matches: usize,
    pub exact_rate: f64,
    pub normalized_similarity: f64,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct RuntimeSummary {
    pub available: bool,
    pub attempted: usize,
    pub matched: usize,
    pub timeouts: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExecutionResult {
    pub stdout: String,
    pub stderr: String,
    pub exception: Option<String>,
    pub exit_status: Option<i32>,
    pub timed_out: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct ExecutionComparison {
    pub original: ExecutionResult,
    pub recompiled: ExecutionResult,
    pub matches: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionFidelity {
    pub function: String,
    pub original_function_index: usize,
    pub recompiled_function_index: Option<usize>,
    pub original_opcode_count: usize,
    pub recompiled_opcode_count: usize,
    pub exact: bool,
    pub similarity: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct FixtureReport {
    pub fixture: String,
    pub parsed: bool,
    pub decompiled: bool,
    pub generated_haxe: Option<String>,
    pub recompiled: bool,
    pub diagnostics: BTreeMap<String, usize>,
    pub opcode_fidelity: Vec<FunctionFidelity>,
    pub execution: Option<ExecutionComparison>,
}

impl FixtureReport {
    fn new(fixture: String) -> Self {
        Self {
            fixture,
            parsed: false,
            decompiled: false,
            generated_haxe: None,
            recompiled: false,
            diagnostics: BTreeMap::new(),
            opcode_fidelity: Vec::new(),
            execution: None,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct StressReport {
    pub supplied: bool,
    pub parsed: bool,
    pub functions: usize,
    pub decompilation_failures: usize,
    pub cfg_failures: usize,
    pub decompilation_failure_details: Vec<FailureRecord>,
    pub cfg_failure_details: Vec<FailureRecord>,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct MilestoneReport {
    pub schema_version: u32,
    pub fixture_counts: FixtureCounts,
    pub parse_failures: Vec<FailureRecord>,
    pub decompilation_failures: Vec<FailureRecord>,
    pub recompilation_failures: Vec<FailureRecord>,
    pub recompilation_rate: f64,
    pub opcode_fidelity: OpcodeFidelity,
    pub runtime: RuntimeSummary,
    pub execution_mismatches: Vec<FailureRecord>,
    pub stress_test: StressReport,
    pub fixtures: Vec<FixtureReport>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GoldenOutput {
    pub ast: String,
    pub haxe: String,
    pub diagnostics: BTreeMap<String, usize>,
}

pub fn discover_fixtures(directory: &Path) -> Result<Vec<PathBuf>, HarnessError> {
    let mut fixtures = Vec::new();
    if directory.is_dir() {
        for entry in fs::read_dir(directory)? {
            let path = entry?.path();
            if path
                .extension()
                .and_then(|extension| extension.to_str())
                .is_some_and(|extension| extension.eq_ignore_ascii_case("hl"))
            {
                fixtures.push(path);
            }
        }
    }
    fixtures.sort_by(|a, b| normalized_path(a).cmp(&normalized_path(b)));
    if fixtures.is_empty() {
        Err(HarnessError::NoFixtures(directory.to_path_buf()))
    } else {
        Ok(fixtures)
    }
}

fn target_object<'a>(code: &'a Bytecode, fixture: &str) -> Option<&'a TypeObj> {
    code.types.iter().find_map(|ty| match ty {
        Type::Obj(object) | Type::Struct(object)
            if code
                .strings
                .get(object.name.0)
                .is_some_and(|name| name.as_str() == fixture) =>
        {
            Some(object)
        }
        _ => None,
    })
}

pub fn render_fixture_golden(path: &Path) -> Result<GoldenOutput, HarnessError> {
    let fixture = path
        .file_stem()
        .and_then(|name| name.to_str())
        .ok_or_else(|| HarnessError::Message(format!("invalid fixture path {}", path.display())))?;
    let code = Bytecode::from_file(path).map_err(|error| {
        HarnessError::Message(format!("failed to parse {}: {error}", path.display()))
    })?;
    render_fixture(&code, fixture)
}

fn render_fixture(code: &Bytecode, fixture: &str) -> Result<GoldenOutput, HarnessError> {
    if let Some(object) = target_object(code, fixture) {
        let decompiled = decompile_class(code, object)
            .map_err(|error| HarnessError::Message(error.to_string()))?;
        let diagnostics = diagnostic_counts(&decompiled.diagnostics);
        let ast = format!("{:#?}\n", decompiled.value);
        let haxe = format!(
            "{}\n",
            decompiled.value.display(code, &FormatOptions::new(4))
        );
        Ok(GoldenOutput {
            ast,
            haxe,
            diagnostics,
        })
    } else if let Some(function) = code.function_by_name("main") {
        let decompiled = decompile_function(code, function)
            .map_err(|error| HarnessError::Message(error.to_string()))?;
        let diagnostics = diagnostic_counts(&decompiled.diagnostics);
        let ast = format!("{:#?}\n", decompiled.value);
        let method = decompiled
            .value
            .display(code, &FormatOptions::new(4))
            .to_string();
        let haxe = format!("class {fixture} {{\n{method}}}\n");
        Ok(GoldenOutput {
            ast,
            haxe,
            diagnostics,
        })
    } else {
        Err(HarnessError::Message(format!(
            "fixture {fixture} has neither a matching class nor main function"
        )))
    }
}

pub fn run_milestone(options: &HarnessOptions) -> Result<MilestoneReport, HarnessError> {
    let fixtures = discover_fixtures(&options.fixtures_dir)?;
    fs::create_dir_all(&options.output_dir)?;
    fs::create_dir_all(&options.goldens_dir)?;

    let runtime_available = options.execute && command_available("hl");
    let mut report = MilestoneReport {
        schema_version: 1,
        fixture_counts: FixtureCounts {
            discovered: fixtures.len(),
            ..FixtureCounts::default()
        },
        parse_failures: Vec::new(),
        decompilation_failures: Vec::new(),
        recompilation_failures: Vec::new(),
        recompilation_rate: 0.0,
        opcode_fidelity: OpcodeFidelity::default(),
        runtime: RuntimeSummary {
            available: runtime_available,
            ..RuntimeSummary::default()
        },
        execution_mismatches: Vec::new(),
        stress_test: StressReport::default(),
        fixtures: Vec::new(),
    };

    let mut similarity_total = 0.0;
    for path in fixtures {
        let fixture = path
            .file_stem()
            .and_then(|name| name.to_str())
            .unwrap_or("invalid")
            .to_owned();
        let mut fixture_report = FixtureReport::new(fixture.clone());
        let code = match Bytecode::from_file(&path) {
            Ok(code) => code,
            Err(error) => {
                report.parse_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
        };
        fixture_report.parsed = true;
        report.fixture_counts.parsed += 1;

        for function in &code.functions {
            if let Err(error) = ControlFlowGraph::build(function) {
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                });
            }
        }

        let golden = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            render_fixture(&code, &fixture)
        })) {
            Ok(Ok(golden)) => golden,
            Ok(Err(error)) => {
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
            Err(_) => {
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: "formatter panicked".to_owned(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
        };
        fixture_report.decompiled = true;
        fixture_report.diagnostics = golden.diagnostics.clone();
        report.fixture_counts.decompiled += 1;

        if options.golden_fixtures.contains(&fixture) {
            check_or_update_golden(options, &fixture, &golden, &mut report)?;
        }

        let fixture_output = options.output_dir.join(&fixture);
        let generated_dir = fixture_output.join("generated");
        let recompiled_dir = fixture_output.join("recompiled");
        fs::create_dir_all(&generated_dir)?;
        fs::create_dir_all(&recompiled_dir)?;
        let generated_path = generated_dir.join(format!("{fixture}.hx"));
        fs::write(&generated_path, golden.haxe.as_bytes())?;
        fixture_report.generated_haxe = Some(format!("{fixture}/generated/{fixture}.hx"));

        let recompiled_path = recompiled_dir.join(format!("{fixture}.hl"));
        let compile = run_command(
            "haxe",
            &[
                "-cp".into(),
                generated_dir.as_os_str().to_owned(),
                "-main".into(),
                fixture.clone().into(),
                "-hl".into(),
                recompiled_path.as_os_str().to_owned(),
            ],
            options.timeout,
        );
        let compile_ok = match compile {
            Ok(result) if !result.timed_out && result.exit_status == Some(0) => true,
            Ok(result) => {
                report.recompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: format!(
                        "Haxe recompilation failed (status {:?}, timeout {}): {}",
                        result.exit_status,
                        result.timed_out,
                        one_line(&result.stderr)
                    ),
                });
                false
            }
            Err(error) => {
                report.recompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: format!("Haxe recompilation could not start: {error}"),
                });
                false
            }
        };

        if compile_ok {
            fixture_report.recompiled = true;
            report.fixture_counts.recompiled += 1;
            match Bytecode::from_file(&recompiled_path) {
                Ok(recompiled) => {
                    fixture_report.opcode_fidelity = compare_opcodes(&code, &recompiled);
                    for fidelity in &fixture_report.opcode_fidelity {
                        report.opcode_fidelity.functions_compared += 1;
                        report.opcode_fidelity.exact_matches += usize::from(fidelity.exact);
                        similarity_total += fidelity.similarity;
                    }

                    if runtime_available {
                        report.runtime.attempted += 1;
                        let original =
                            run_command("hl", &[path.as_os_str().to_owned()], options.timeout);
                        let generated = run_command(
                            "hl",
                            &[recompiled_path.as_os_str().to_owned()],
                            options.timeout,
                        );
                        match (original, generated) {
                            (Ok(original), Ok(recompiled)) => {
                                report.fixture_counts.executed += 1;
                                report.runtime.timeouts +=
                                    usize::from(original.timed_out || recompiled.timed_out);
                                let matches = original == recompiled;
                                report.runtime.matched += usize::from(matches);
                                fixture_report.execution = Some(ExecutionComparison {
                                    original,
                                    recompiled,
                                    matches,
                                });
                                if !matches {
                                    report.execution_mismatches.push(FailureRecord {
                                        fixture: fixture.clone(),
                                        message: "stdout, stderr, exception, timeout, or exit status differs"
                                            .to_owned(),
                                    });
                                }
                            }
                            (Err(error), _) | (_, Err(error)) => {
                                report.execution_mismatches.push(FailureRecord {
                                    fixture: fixture.clone(),
                                    message: error.to_string(),
                                });
                            }
                        }
                    }
                }
                Err(error) => report.parse_failures.push(FailureRecord {
                    fixture: format!("{fixture} (recompiled)"),
                    message: error.to_string(),
                }),
            }
        }
        report.fixtures.push(fixture_report);
    }

    report.recompilation_rate = ratio(
        report.fixture_counts.recompiled,
        report.fixture_counts.decompiled,
    );
    report.opcode_fidelity.exact_rate = ratio(
        report.opcode_fidelity.exact_matches,
        report.opcode_fidelity.functions_compared,
    );
    if report.opcode_fidelity.functions_compared > 0 {
        report.opcode_fidelity.normalized_similarity =
            round6(similarity_total / report.opcode_fidelity.functions_compared as f64);
    }
    report.stress_test = run_stress(options.hlboot.as_deref());

    report
        .parse_failures
        .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
    report
        .decompilation_failures
        .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
    report
        .recompilation_failures
        .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
    report
        .execution_mismatches
        .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
    report.fixtures.sort_by(|a, b| a.fixture.cmp(&b.fixture));

    if let Some(parent) = options.report_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(
        &options.report_path,
        format!("{}\n", serde_json::to_string_pretty(&report)?),
    )?;
    Ok(report)
}

fn check_or_update_golden(
    options: &HarnessOptions,
    fixture: &str,
    golden: &GoldenOutput,
    report: &mut MilestoneReport,
) -> Result<(), HarnessError> {
    let ast_path = options.goldens_dir.join(format!("{fixture}.ast.txt"));
    let haxe_path = options.goldens_dir.join(format!("{fixture}.hx"));
    if options.update_goldens {
        fs::write(ast_path, golden.ast.as_bytes())?;
        fs::write(haxe_path, golden.haxe.as_bytes())?;
        return Ok(());
    }
    for (kind, path, actual) in [
        ("AST", ast_path, golden.ast.as_str()),
        ("Haxe", haxe_path, golden.haxe.as_str()),
    ] {
        match fs::read_to_string(&path) {
            Ok(expected) if expected == actual => {}
            Ok(_) => report.decompilation_failures.push(FailureRecord {
                fixture: fixture.to_owned(),
                message: format!(
                    "{kind} golden differs; run `cargo run -p hlbc-decompiler --bin hlbc-milestone -- --update-goldens`"
                ),
            }),
            Err(error) => report.decompilation_failures.push(FailureRecord {
                fixture: fixture.to_owned(),
                message: format!("{kind} golden {} is unavailable: {error}", path.display()),
            }),
        }
    }
    Ok(())
}

fn compare_opcodes(original: &Bytecode, recompiled: &Bytecode) -> Vec<FunctionFidelity> {
    let mut by_name: BTreeMap<String, Vec<&Function>> = BTreeMap::new();
    for function in &recompiled.functions {
        by_name
            .entry(safe_function_name(recompiled, function))
            .or_default()
            .push(function);
    }
    let mut used: BTreeMap<String, usize> = BTreeMap::new();
    let mut result = Vec::new();
    for function in &original.functions {
        let name = safe_function_name(original, function);
        let position = used.entry(name.clone()).or_default();
        let candidate = by_name
            .get(&name)
            .and_then(|items| items.get(*position))
            .copied();
        *position += 1;
        let original_names: Vec<_> = function.ops.iter().map(|opcode| opcode.name()).collect();
        let recompiled_names: Vec<_> = candidate
            .into_iter()
            .flat_map(|function| &function.ops)
            .map(|opcode| opcode.name())
            .collect();
        result.push(FunctionFidelity {
            function: name,
            original_function_index: function.findex.0,
            recompiled_function_index: candidate.map(|function| function.findex.0),
            original_opcode_count: function.ops.len(),
            recompiled_opcode_count: candidate.map_or(0, |function| function.ops.len()),
            exact: candidate.is_some_and(|candidate| function.ops == candidate.ops),
            similarity: round6(sequence_similarity(&original_names, &recompiled_names)),
        });
    }
    result
}

fn safe_function_name(code: &Bytecode, function: &Function) -> String {
    code.strings
        .get(function.name.0)
        .map(ToString::to_string)
        .unwrap_or_else(|| format!("findex:{}", function.findex.0))
}

fn diagnostic_counts(diagnostics: &[Diagnostic]) -> BTreeMap<String, usize> {
    let mut counts = BTreeMap::new();
    for diagnostic in diagnostics {
        *counts.entry(diagnostic.severity.to_string()).or_insert(0) += 1;
    }
    counts
}

fn sequence_similarity(left: &[&str], right: &[&str]) -> f64 {
    if left.is_empty() && right.is_empty() {
        return 1.0;
    }
    let mut previous = vec![0usize; right.len() + 1];
    for left_item in left {
        let mut current = vec![0usize; right.len() + 1];
        for (index, right_item) in right.iter().enumerate() {
            current[index + 1] = if left_item == right_item {
                previous[index] + 1
            } else {
                current[index].max(previous[index + 1])
            };
        }
        previous = current;
    }
    previous[right.len()] as f64 / left.len().max(right.len()) as f64
}

fn run_stress(path: Option<&Path>) -> StressReport {
    let Some(path) = path else {
        return StressReport::default();
    };
    let mut report = StressReport {
        supplied: true,
        ..StressReport::default()
    };
    match Bytecode::from_file(path) {
        Ok(code) => {
            report.parsed = true;
            report.functions = code.functions.len();
            for function in &code.functions {
                let fixture = format!(
                    "{} ({})",
                    safe_function_name(&code, function),
                    function.findex.0
                );
                if let Err(error) = ControlFlowGraph::build(function) {
                    report.cfg_failures += 1;
                    report.cfg_failure_details.push(FailureRecord {
                        fixture: fixture.clone(),
                        message: error.to_string(),
                    });
                }
                if let Err(error) = crate::decompile_code(&code, function) {
                    report.decompilation_failures += 1;
                    report.decompilation_failure_details.push(FailureRecord {
                        fixture,
                        message: error.to_string(),
                    });
                }
            }
            report
                .cfg_failure_details
                .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
            report
                .decompilation_failure_details
                .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
        }
        Err(error) => report.error = Some(error.to_string()),
    }
    report
}

fn command_available(command: &str) -> bool {
    Command::new(command)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok()
}

fn run_command(
    command: &str,
    arguments: &[std::ffi::OsString],
    timeout: Duration,
) -> Result<ExecutionResult, HarnessError> {
    let mut child = Command::new(command)
        .args(arguments)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| HarnessError::Message(format!("failed to capture stdout for {command}")))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| HarnessError::Message(format!("failed to capture stderr for {command}")))?;
    let stdout_thread = std::thread::spawn(move || read_pipe(stdout));
    let stderr_thread = std::thread::spawn(move || read_pipe(stderr));
    let (status, timed_out) = match child.wait_timeout(timeout)? {
        Some(status) => (Some(status), false),
        None => {
            child.kill()?;
            (Some(child.wait()?), true)
        }
    };
    let stdout = stdout_thread
        .join()
        .map_err(|_| HarnessError::Message("stdout reader panicked".into()))??;
    let stderr = stderr_thread
        .join()
        .map_err(|_| HarnessError::Message("stderr reader panicked".into()))??;
    Ok(execution_result(status, stdout, stderr, timed_out))
}

fn read_pipe(mut pipe: impl Read) -> std::io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    pipe.read_to_end(&mut bytes)?;
    Ok(bytes)
}

fn execution_result(
    status: Option<ExitStatus>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    timed_out: bool,
) -> ExecutionResult {
    let stdout = String::from_utf8_lossy(&stdout).replace("\r\n", "\n");
    let stderr = String::from_utf8_lossy(&stderr).replace("\r\n", "\n");
    let exception = status
        .filter(|status| !status.success())
        .and_then(|_| stderr.lines().find(|line| !line.trim().is_empty()))
        .map(str::to_owned);
    ExecutionResult {
        stdout,
        stderr,
        exception,
        exit_status: status.and_then(|status| status.code()),
        timed_out,
    }
}

fn normalized_path(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn one_line(value: &str) -> String {
    value
        .lines()
        .next()
        .unwrap_or_default()
        .trim()
        .replace('\\', "/")
}

fn ratio(numerator: usize, denominator: usize) -> f64 {
    if denominator == 0 {
        0.0
    } else {
        round6(numerator as f64 / denominator as f64)
    }
}

fn round6(value: f64) -> f64 {
    (value * 1_000_000.0).round() / 1_000_000.0
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use std::panic::{catch_unwind, AssertUnwindSafe};

    use proptest::prelude::*;

    use super::*;

    #[test]
    fn empty_fixture_discovery_is_an_error() {
        let directory = tempfile::tempdir().unwrap();
        let error = discover_fixtures(directory.path()).unwrap_err();
        assert!(error.to_string().contains("zero .hl files"));
        assert!(error.to_string().contains("compile data/*.hx"));
    }

    #[test]
    fn opcode_similarity_is_normalized() {
        assert_eq!(sequence_similarity(&["A", "B"], &["A", "B"]), 1.0);
        assert_eq!(sequence_similarity(&["A", "B"], &["A", "C"]), 0.5);
        assert_eq!(sequence_similarity(&[], &[]), 1.0);
    }

    #[test]
    fn empty_fixture_has_deterministic_ast_and_haxe_goldens() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let fixture = manifest.join("../../data/Empty.hl");
        assert!(
            fixture.is_file(),
            "{} is missing; compile data/*.hx before tests",
            fixture.display()
        );
        let first = render_fixture_golden(&fixture).unwrap();
        let second = render_fixture_golden(&fixture).unwrap();
        assert_eq!(first, second);

        let goldens = manifest.join("tests/goldens");
        assert_eq!(
            fs::read_to_string(goldens.join("Empty.ast.txt")).unwrap(),
            first.ast
        );
        assert_eq!(
            fs::read_to_string(goldens.join("Empty.hx")).unwrap(),
            first.haxe
        );
    }

    #[test]
    fn focused_control_flow_haxe_goldens() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        for fixture in ["LoopWhile", "LoopInfinite", "TryCatch"] {
            let actual = render_fixture_golden(&manifest.join(format!("../../data/{fixture}.hl")))
                .unwrap()
                .haxe;
            let expected =
                fs::read_to_string(manifest.join(format!("tests/goldens/{fixture}.control.hx")))
                    .unwrap()
                    .replace("\r\n", "\n");
            assert_eq!(expected, actual, "{fixture} control-flow golden changed");
        }
    }

    proptest! {
        #[test]
        fn random_byte_streams_never_panic(bytes in prop::collection::vec(any::<u8>(), 0..4096)) {
            let result = catch_unwind(AssertUnwindSafe(|| {
                Bytecode::deserialize(Cursor::new(bytes))
            }));
            prop_assert!(result.is_ok());
        }
    }
}

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

use hlbc::opcodes::Opcode;
use hlbc::types::{Function, Type, TypeObj};
use hlbc::Bytecode;
use serde::{Deserialize, Serialize};
use wait_timeout::ChildExt;

use crate::ast::Statement;
use crate::cfg::ControlFlowGraph;
use crate::diagnostics::{DecompileOptions, Diagnostic, DiagnosticSeverity};
use crate::fmt::FormatOptions;
use crate::{
    decompile_class_with_options, decompile_code_with_options, decompile_function_with_options,
    opcode_coverage, DiagnosticCoverage, LoweringCoverage, OptimizationProfile,
};

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
    pub minimum_fixtures: usize,
    pub minimum_opcode_similarity: f64,
    pub interactive_budget: Duration,
    pub benchmark_iterations: usize,
    pub crashlink: Option<PathBuf>,
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
            minimum_fixtures: 20,
            minimum_opcode_similarity: 0.95,
            interactive_budget: Duration::from_millis(50),
            benchmark_iterations: 5,
            crashlink: std::env::var_os("HLBC_CRASHLINK").map(PathBuf::from),
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
    pub profile: String,
    pub functions_compared: usize,
    pub exact_matches: usize,
    pub mismatches: usize,
    pub exact_rate: f64,
    pub normalized_similarity: f64,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct OpcodeCoverageSummary {
    pub known: usize,
    pub expression: usize,
    pub statement: usize,
    pub structural: usize,
    pub semantic_only: usize,
    pub unsupported_fallback: usize,
    pub unsupported_diagnostics: usize,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PanicSummary {
    pub parser: usize,
    pub cfg: usize,
    pub decompiler: usize,
    pub formatter: usize,
    pub total: usize,
    pub details: Vec<FailureRecord>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct FallbackSummary {
    pub functions: usize,
    pub state_machine_regions: usize,
    pub unhandled_opcodes: usize,
    pub diagnostics: usize,
}

impl FallbackSummary {
    fn add(&mut self, other: &Self) {
        self.functions += other.functions;
        self.state_machine_regions += other.state_machine_regions;
        self.unhandled_opcodes += other.unhandled_opcodes;
        self.diagnostics += other.diagnostics;
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct BenchmarkSample {
    pub fixture: String,
    pub function: String,
    pub function_index: usize,
    pub duration_micros: u64,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct BenchmarkSummary {
    pub release_mode: bool,
    pub iterations: usize,
    pub samples: usize,
    pub percentile: u8,
    pub percentile_micros: u64,
    pub budget_micros: u64,
    pub passed: bool,
    pub measurements: Vec<BenchmarkSample>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ReadabilityMetrics {
    pub structured_constructs: usize,
    pub state_machines: usize,
    pub unhandled_markers: usize,
    pub compiler_temporaries: usize,
    pub non_empty_lines: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadabilityComparison {
    pub category: String,
    pub fixture: String,
    pub hlbc: ReadabilityMetrics,
    pub crashlink: ReadabilityMetrics,
    pub equivalent_or_better: bool,
    pub message: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ReadabilitySummary {
    pub crashlink_supplied: bool,
    pub crashlink_available: bool,
    pub comparisons: Vec<ReadabilityComparison>,
    pub passed: bool,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct AcceptanceCriterion {
    pub requirement: u32,
    pub name: String,
    pub passed: bool,
    pub measured: String,
    pub threshold: String,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct AcceptanceSummary {
    pub passed: bool,
    pub criteria: Vec<AcceptanceCriterion>,
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
    pub raw_matches: bool,
    pub matches: bool,
    pub normalizations: Vec<String>,
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
    pub divergences: Vec<crate::divergence::OpcodeDivergence>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FixtureReport {
    pub fixture: String,
    pub parsed: bool,
    pub decompiled: bool,
    pub generated_haxe: Option<String>,
    pub readable_haxe: Option<String>,
    pub recompiled: bool,
    pub diagnostics: BTreeMap<String, usize>,
    pub diagnostic_details: Vec<Diagnostic>,
    pub fallback_usage: FallbackSummary,
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
            readable_haxe: None,
            recompiled: false,
            diagnostics: BTreeMap::new(),
            diagnostic_details: Vec::new(),
            fallback_usage: FallbackSummary::default(),
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
    pub classes: usize,
    pub functions_checked: usize,
    pub classes_checked: usize,
    pub workers: usize,
    pub decompilation_failures: usize,
    pub cfg_failures: usize,
    pub fallback_usage: FallbackSummary,
    pub panics: PanicSummary,
    pub elapsed_millis: u64,
    pub decompilation_failure_details: Vec<FailureRecord>,
    pub cfg_failure_details: Vec<FailureRecord>,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct MilestoneReport {
    pub schema_version: u32,
    pub acceptance: AcceptanceSummary,
    pub fixture_counts: FixtureCounts,
    pub opcode_coverage: OpcodeCoverageSummary,
    pub panics: PanicSummary,
    pub fallback_usage: FallbackSummary,
    pub parse_failures: Vec<FailureRecord>,
    pub decompilation_failures: Vec<FailureRecord>,
    pub recompilation_failures: Vec<FailureRecord>,
    pub recompilation_rate: f64,
    pub opcode_fidelity: OpcodeFidelity,
    pub runtime: RuntimeSummary,
    pub benchmark: BenchmarkSummary,
    pub readability: ReadabilitySummary,
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

struct RenderedFixture {
    golden: GoldenOutput,
    diagnostics: Vec<Diagnostic>,
    fallback_usage: FallbackSummary,
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
    render_fixture_with_options(code, fixture, DecompileOptions::default())
        .map(|rendered| rendered.golden)
}

fn render_fixture_with_options(
    code: &Bytecode,
    fixture: &str,
    options: DecompileOptions,
) -> Result<RenderedFixture, HarnessError> {
    if let Some(object) = target_object(code, fixture) {
        let decompiled = decompile_class_with_options(code, object, options)
            .map_err(|error| HarnessError::Message(error.to_string()))?;
        let diagnostics = diagnostic_counts(&decompiled.diagnostics);
        let fallback_usage = fallback_usage(
            decompiled
                .value
                .methods
                .iter()
                .map(|method| method.statements.as_slice()),
            &decompiled.diagnostics,
        );
        let ast = format!("{:#?}\n", decompiled.value);
        let haxe = format!(
            "{}\n",
            decompiled.value.display(code, &FormatOptions::new(4))
        );
        Ok(RenderedFixture {
            golden: GoldenOutput {
                ast,
                haxe,
                diagnostics,
            },
            diagnostics: decompiled.diagnostics,
            fallback_usage,
        })
    } else if let Some(function) = code.function_by_name("main") {
        let decompiled = decompile_function_with_options(code, function, options)
            .map_err(|error| HarnessError::Message(error.to_string()))?;
        let diagnostics = diagnostic_counts(&decompiled.diagnostics);
        let fallback_usage = fallback_usage(
            std::iter::once(decompiled.value.statements.as_slice()),
            &decompiled.diagnostics,
        );
        let ast = format!("{:#?}\n", decompiled.value);
        let method = decompiled
            .value
            .display(code, &FormatOptions::new(4))
            .to_string();
        let haxe = format!("class {fixture} {{\n{method}}}\n");
        Ok(RenderedFixture {
            golden: GoldenOutput {
                ast,
                haxe,
                diagnostics,
            },
            diagnostics: decompiled.diagnostics,
            fallback_usage,
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
        schema_version: 2,
        acceptance: AcceptanceSummary::default(),
        fixture_counts: FixtureCounts {
            discovered: fixtures.len(),
            ..FixtureCounts::default()
        },
        opcode_coverage: opcode_coverage_summary(),
        panics: PanicSummary::default(),
        fallback_usage: FallbackSummary::default(),
        parse_failures: Vec::new(),
        decompilation_failures: Vec::new(),
        recompilation_failures: Vec::new(),
        recompilation_rate: 0.0,
        opcode_fidelity: OpcodeFidelity::default(),
        runtime: RuntimeSummary {
            available: runtime_available,
            ..RuntimeSummary::default()
        },
        benchmark: BenchmarkSummary {
            release_mode: !cfg!(debug_assertions),
            iterations: options.benchmark_iterations,
            percentile: 95,
            budget_micros: duration_micros(options.interactive_budget),
            ..BenchmarkSummary::default()
        },
        readability: ReadabilitySummary::default(),
        execution_mismatches: Vec::new(),
        stress_test: StressReport::default(),
        fixtures: Vec::new(),
    };

    let mut similarity_total = 0.0;
    let mut benchmark_measurements = Vec::new();
    for path in fixtures {
        let fixture = path
            .file_stem()
            .and_then(|name| name.to_str())
            .unwrap_or("invalid")
            .to_owned();
        let mut fixture_report = FixtureReport::new(fixture.clone());
        let code = match catch_unwind(AssertUnwindSafe(|| Bytecode::from_file(&path))) {
            Ok(Ok(code)) => code,
            Ok(Err(error)) => {
                report.parse_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
            Err(payload) => {
                record_panic(
                    &mut report.panics,
                    "parser",
                    &fixture,
                    panic_message(payload),
                );
                report.parse_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: "parser panicked; see panic details".to_owned(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
        };
        fixture_report.parsed = true;
        report.fixture_counts.parsed += 1;

        for function in &code.functions {
            match catch_unwind(AssertUnwindSafe(|| ControlFlowGraph::build(function))) {
                Ok(Ok(_)) => {}
                Ok(Err(error)) => report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                }),
                Err(payload) => {
                    record_panic(
                        &mut report.panics,
                        "cfg",
                        &format!("{fixture}:{}", safe_function_name(&code, function)),
                        panic_message(payload),
                    );
                    report.decompilation_failures.push(FailureRecord {
                        fixture: fixture.clone(),
                        message: format!(
                            "CFG panicked for {}",
                            safe_function_name(&code, function)
                        ),
                    });
                }
            }
        }

        let rendered = match catch_unwind(AssertUnwindSafe(|| {
            render_fixture_with_options(
                &code,
                &fixture,
                DecompileOptions {
                    optimization_profile: OptimizationProfile::Fidelity,
                    ..DecompileOptions::default()
                },
            )
        })) {
            Ok(Ok(rendered)) => rendered,
            Ok(Err(error)) => {
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: error.to_string(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
            Err(payload) => {
                record_panic(
                    &mut report.panics,
                    "formatter",
                    &fixture,
                    panic_message(payload),
                );
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: "formatter panicked".to_owned(),
                });
                report.fixtures.push(fixture_report);
                continue;
            }
        };
        let RenderedFixture {
            golden,
            diagnostics,
            fallback_usage: rendered_fallback,
        } = rendered;
        fixture_report.decompiled = true;
        fixture_report.diagnostics = golden.diagnostics.clone();
        fixture_report.diagnostic_details = diagnostics;
        fixture_report.fallback_usage = rendered_fallback;
        report.fallback_usage.add(&fixture_report.fallback_usage);
        report.fixture_counts.decompiled += 1;

        if options.golden_fixtures.contains(&fixture) {
            let balanced_golden =
                render_fixture_with_options(&code, &fixture, DecompileOptions::default())?;
            check_or_update_golden(options, &fixture, &balanced_golden.golden, &mut report)?;
        }

        let fixture_output = options.output_dir.join(&fixture);
        let generated_dir = fixture_output.join("generated");
        let readable_dir = fixture_output.join("readable");
        let recompiled_dir = fixture_output.join("recompiled");
        fs::create_dir_all(&generated_dir)?;
        fs::create_dir_all(&readable_dir)?;
        fs::create_dir_all(&recompiled_dir)?;
        let generated_path = generated_dir.join(format!("{fixture}.hx"));
        fs::write(&generated_path, golden.haxe.as_bytes())?;
        fixture_report.generated_haxe = Some(format!("{fixture}/generated/{fixture}.hx"));
        match catch_unwind(AssertUnwindSafe(|| {
            render_fixture_with_options(
                &code,
                &fixture,
                DecompileOptions {
                    optimization_profile: OptimizationProfile::Readability,
                    include_unreachable: false,
                    ..DecompileOptions::default()
                },
            )
        })) {
            Ok(Ok(readable)) => {
                fs::write(
                    readable_dir.join(format!("{fixture}.hx")),
                    readable.golden.haxe.as_bytes(),
                )?;
                fixture_report.readable_haxe = Some(format!("{fixture}/readable/{fixture}.hx"));
            }
            Ok(Err(error)) => report.decompilation_failures.push(FailureRecord {
                fixture: fixture.clone(),
                message: format!("readability profile failed: {error}"),
            }),
            Err(payload) => {
                record_panic(
                    &mut report.panics,
                    "formatter",
                    &format!("{fixture} (readability)"),
                    panic_message(payload),
                );
                report.decompilation_failures.push(FailureRecord {
                    fixture: fixture.clone(),
                    message: "readability profile panicked".to_owned(),
                });
            }
        }

        if let Some(function) = code.function_by_name("main") {
            for _ in 0..options.benchmark_iterations {
                let started = Instant::now();
                let result = catch_unwind(AssertUnwindSafe(|| {
                    decompile_code_with_options(
                        &code,
                        function,
                        DecompileOptions {
                            optimization_profile: OptimizationProfile::Balanced,
                            ..DecompileOptions::default()
                        },
                    )
                }));
                let duration = duration_micros(started.elapsed());
                match result {
                    Ok(Ok(_)) => benchmark_measurements.push(BenchmarkSample {
                        fixture: fixture.clone(),
                        function: safe_function_name(&code, function),
                        function_index: function.findex.0,
                        duration_micros: duration,
                    }),
                    Ok(Err(error)) => report.decompilation_failures.push(FailureRecord {
                        fixture: fixture.clone(),
                        message: format!("benchmark decompilation failed: {error}"),
                    }),
                    Err(payload) => record_panic(
                        &mut report.panics,
                        "decompiler",
                        &format!("{fixture}:main benchmark"),
                        panic_message(payload),
                    ),
                }
            }
        }

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
                                let raw_matches = original == recompiled;
                                let (matches, normalizations) =
                                    compare_execution_results(&original, &recompiled);
                                report.runtime.matched += usize::from(matches);
                                fixture_report.execution = Some(ExecutionComparison {
                                    original,
                                    recompiled,
                                    raw_matches,
                                    matches,
                                    normalizations,
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
    report.opcode_fidelity.profile = OptimizationProfile::Fidelity.to_string();
    report.opcode_fidelity.mismatches = report
        .opcode_fidelity
        .functions_compared
        .saturating_sub(report.opcode_fidelity.exact_matches);
    if report.opcode_fidelity.functions_compared > 0 {
        report.opcode_fidelity.normalized_similarity =
            round6(similarity_total / report.opcode_fidelity.functions_compared as f64);
    }
    finish_benchmark(
        &mut report.benchmark,
        benchmark_measurements,
        options.interactive_budget,
    );
    report.readability = run_readability_comparison(options);
    report.stress_test = run_stress(options.hlboot.as_deref());
    report
        .fallback_usage
        .add(&report.stress_test.fallback_usage);
    report.panics.parser += report.stress_test.panics.parser;
    report.panics.cfg += report.stress_test.panics.cfg;
    report.panics.decompiler += report.stress_test.panics.decompiler;
    report.panics.formatter += report.stress_test.panics.formatter;
    report
        .panics
        .details
        .extend(report.stress_test.panics.details.iter().cloned());
    report.panics.total = report.panics.parser
        + report.panics.cfg
        + report.panics.decompiler
        + report.panics.formatter;
    report.acceptance = acceptance_summary(&report, options);

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
    report
        .panics
        .details
        .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));

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
    let focused_haxe = match fixture {
        "HaxeRecovery" => Some(options.goldens_dir.join("HaxeRecovery.recovery.hx")),
        "MapLiteral" => Some(options.goldens_dir.join("MapLiteral.recovery.hx")),
        _ => None,
    };
    if options.update_goldens {
        if let Some(path) = focused_haxe {
            fs::write(path, golden.haxe.as_bytes())?;
            return Ok(());
        }
    }
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

pub fn compare_opcodes(original: &Bytecode, recompiled: &Bytecode) -> Vec<FunctionFidelity> {
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
        let divergences = crate::ir::TypedIr::build(original, function)
            .ok()
            .and_then(|ir| {
                crate::decompile_code(original, function).ok().map(|body| {
                    crate::divergence::attribute_opcode_divergences(
                        function,
                        candidate,
                        &ir.value,
                        &body.value,
                    )
                })
            })
            .unwrap_or_else(|| {
                crate::divergence::opcode_diff(
                    &function.ops,
                    candidate
                        .map(|function| function.ops.as_slice())
                        .unwrap_or(&[]),
                )
            });
        result.push(FunctionFidelity {
            function: name,
            original_function_index: function.findex.0,
            recompiled_function_index: candidate.map(|function| function.findex.0),
            original_opcode_count: function.ops.len(),
            recompiled_opcode_count: candidate.map_or(0, |function| function.ops.len()),
            exact: candidate.is_some_and(|candidate| function.ops == candidate.ops),
            similarity: round6(sequence_similarity(&original_names, &recompiled_names)),
            divergences,
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

fn fallback_usage<'a>(
    functions: impl IntoIterator<Item = &'a [Statement]>,
    diagnostics: &[Diagnostic],
) -> FallbackSummary {
    let mut summary = FallbackSummary::default();
    for statements in functions {
        let before = summary.state_machine_regions + summary.unhandled_opcodes;
        count_statement_fallbacks(statements, &mut summary);
        let after = summary.state_machine_regions + summary.unhandled_opcodes;
        summary.functions += usize::from(after > before);
    }
    summary.diagnostics = diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                diagnostic.severity,
                DiagnosticSeverity::Unsupported | DiagnosticSeverity::Fatal
            ) || diagnostic.message.contains("state-machine")
                || diagnostic.message.contains("irreducible")
                || diagnostic.message.contains("fallback")
        })
        .count();
    if summary.state_machine_regions > 0 {
        summary.diagnostics = summary.diagnostics.max(
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Information)
                .count(),
        );
    }
    if summary.functions == 0 && summary.diagnostics > 0 {
        summary.functions = 1;
    }
    summary
}

fn count_statement_fallbacks(statements: &[Statement], summary: &mut FallbackSummary) {
    for statement in statements {
        match statement {
            Statement::StateMachine { blocks, .. } => {
                summary.state_machine_regions += 1;
                for block in blocks {
                    count_statement_fallbacks(&block.stmts, summary);
                }
            }
            Statement::UnhandledOpcode { .. } => summary.unhandled_opcodes += 1,
            Statement::Provenanced { statement, .. } => {
                count_statement_fallbacks(std::slice::from_ref(statement), summary)
            }
            Statement::IfElse { if_, else_, .. } => {
                count_statement_fallbacks(if_, summary);
                count_statement_fallbacks(else_, summary);
            }
            Statement::Switch { default, cases, .. } => {
                count_statement_fallbacks(default, summary);
                for (_, body) in cases {
                    count_statement_fallbacks(body, summary);
                }
            }
            Statement::While { stmts, .. }
            | Statement::DoWhile { stmts, .. }
            | Statement::ForEach { stmts, .. }
            | Statement::ForRange { stmts, .. }
            | Statement::Try { stmts }
            | Statement::Catch { stmts } => count_statement_fallbacks(stmts, summary),
            Statement::TryCatch { try_stmts, catches } => {
                count_statement_fallbacks(try_stmts, summary);
                for catch in catches {
                    count_statement_fallbacks(&catch.stmts, summary);
                }
            }
            Statement::VarDecl { .. }
            | Statement::Assign { .. }
            | Statement::ExprStatement(_)
            | Statement::GlobalStore { .. }
            | Statement::DynamicFieldStore { .. }
            | Statement::MemoryStore { .. }
            | Statement::ReferenceStore { .. }
            | Statement::RuntimeCheck(_)
            | Statement::Prefetch { .. }
            | Statement::Nop
            | Statement::Return(_)
            | Statement::Break
            | Statement::Continue
            | Statement::Throw(_)
            | Statement::Comment(_) => {}
        }
    }
}

fn opcode_coverage_summary() -> OpcodeCoverageSummary {
    let mut summary = OpcodeCoverageSummary {
        known: Opcode::COUNT,
        ..OpcodeCoverageSummary::default()
    };
    for opcode in Opcode::all_defaults() {
        let coverage = opcode_coverage(&opcode);
        match coverage.lowering {
            LoweringCoverage::Expression => summary.expression += 1,
            LoweringCoverage::Statement => summary.statement += 1,
            LoweringCoverage::Structural => summary.structural += 1,
            LoweringCoverage::SemanticOnly => summary.semantic_only += 1,
            LoweringCoverage::UnsupportedFallback => summary.unsupported_fallback += 1,
        }
        summary.unsupported_diagnostics +=
            usize::from(coverage.diagnostics == DiagnosticCoverage::Unsupported);
    }
    summary
}

fn duration_micros(duration: Duration) -> u64 {
    duration.as_micros().min(u128::from(u64::MAX)) as u64
}

fn finish_benchmark(
    summary: &mut BenchmarkSummary,
    mut measurements: Vec<BenchmarkSample>,
    budget: Duration,
) {
    let mut durations: Vec<_> = measurements
        .iter()
        .map(|sample| sample.duration_micros)
        .collect();
    durations.sort_unstable();
    summary.samples = durations.len();
    summary.percentile_micros = if durations.is_empty() {
        0
    } else {
        let rank = ((durations.len() * usize::from(summary.percentile)) + 99) / 100;
        durations[rank.saturating_sub(1).min(durations.len() - 1)]
    };
    summary.budget_micros = duration_micros(budget);
    summary.passed = summary.release_mode
        && !durations.is_empty()
        && summary.percentile_micros <= summary.budget_micros;
    measurements.sort_by(|left, right| {
        (
            &left.fixture,
            &left.function,
            left.function_index,
            left.duration_micros,
        )
            .cmp(&(
                &right.fixture,
                &right.function,
                right.function_index,
                right.duration_micros,
            ))
    });
    summary.measurements = measurements;
}

fn panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    payload
        .downcast_ref::<&str>()
        .map(|message| (*message).to_owned())
        .or_else(|| payload.downcast_ref::<String>().cloned())
        .unwrap_or_else(|| "non-string panic payload".to_owned())
}

fn record_panic(summary: &mut PanicSummary, stage: &str, fixture: &str, message: String) {
    match stage {
        "parser" => summary.parser += 1,
        "cfg" => summary.cfg += 1,
        "formatter" => summary.formatter += 1,
        _ => summary.decompiler += 1,
    }
    summary.total += 1;
    summary.details.push(FailureRecord {
        fixture: fixture.to_owned(),
        message: format!("{stage}: {message}"),
    });
}

fn run_readability_comparison(options: &HarnessOptions) -> ReadabilitySummary {
    let Some(crashlink) = options.crashlink.as_deref() else {
        return ReadabilitySummary {
            error: Some("crashlink was not supplied; use --crashlink or HLBC_CRASHLINK".to_owned()),
            ..ReadabilitySummary::default()
        };
    };
    let mut summary = ReadabilitySummary {
        crashlink_supplied: true,
        ..ReadabilitySummary::default()
    };
    let script =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../scripts/compare-crashlink.py");
    if !crashlink.is_dir() || !script.is_file() {
        summary.error = Some(format!(
            "crashlink directory {} or comparison script {} is unavailable",
            crashlink.display(),
            script.display()
        ));
        return summary;
    }
    let output = Command::new("python")
        .arg(script)
        .arg("--crashlink")
        .arg(crashlink)
        .arg("--fixtures")
        .arg(&options.fixtures_dir)
        .arg("--hlbc-output")
        .arg(&options.output_dir)
        .output();
    match output {
        Ok(output) if output.status.success() => {
            match serde_json::from_slice::<ReadabilitySummary>(&output.stdout) {
                Ok(mut comparison) => {
                    comparison.crashlink_supplied = true;
                    comparison.crashlink_available = true;
                    comparison
                }
                Err(error) => {
                    summary.error = Some(format!("invalid crashlink comparison JSON: {error}"));
                    summary
                }
            }
        }
        Ok(output) => {
            summary.error = Some(format!(
                "crashlink comparison failed: {}",
                one_line(&String::from_utf8_lossy(&output.stderr))
            ));
            summary
        }
        Err(error) => {
            summary.error = Some(format!("could not start crashlink comparison: {error}"));
            summary
        }
    }
}

fn acceptance_summary(report: &MilestoneReport, options: &HarnessOptions) -> AcceptanceSummary {
    let recompilation_passed = report.fixture_counts.discovered >= options.minimum_fixtures
        && report.fixture_counts.parsed == report.fixture_counts.discovered
        && report.fixture_counts.decompiled == report.fixture_counts.discovered
        && report.fixture_counts.recompiled == report.fixture_counts.discovered
        && report.parse_failures.is_empty()
        && report.decompilation_failures.is_empty()
        && report.recompilation_failures.is_empty();
    let stress_passed = report.stress_test.supplied
        && report.stress_test.parsed
        && report.stress_test.functions_checked == report.stress_test.functions
        && report.stress_test.classes_checked == report.stress_test.classes
        && report.stress_test.cfg_failures == 0
        && report.stress_test.decompilation_failures == 0
        && report.stress_test.panics.total == 0
        && report.stress_test.error.is_none();
    let fidelity_passed = report.opcode_fidelity.functions_compared > 0
        && report.opcode_fidelity.normalized_similarity >= options.minimum_opcode_similarity;
    let fallback_contract = report.opcode_coverage.unsupported_fallback
        == report.opcode_coverage.unsupported_diagnostics
        && report.fallback_usage.diagnostics
            >= report.fallback_usage.unhandled_opcodes
                + report.fallback_usage.state_machine_regions;
    let mut criteria = vec![
        AcceptanceCriterion {
            requirement: 96,
            name: "fixture decompile and recompile".to_owned(),
            passed: recompilation_passed,
            measured: format!(
                "{}/{} recompiled",
                report.fixture_counts.recompiled, report.fixture_counts.discovered
            ),
            threshold: format!(
                "all discovered fixtures, at least {}",
                options.minimum_fixtures
            ),
        },
        AcceptanceCriterion {
            requirement: 97,
            name: "hlboot panic-free stress".to_owned(),
            passed: stress_passed,
            measured: format!(
                "{}/{} functions, {}/{} classes, {} panics",
                report.stress_test.functions_checked,
                report.stress_test.functions,
                report.stress_test.classes_checked,
                report.stress_test.classes,
                report.stress_test.panics.total
            ),
            threshold: "all supplied functions and classes; zero panics/failures".to_owned(),
        },
        AcceptanceCriterion {
            requirement: 98,
            name: "fidelity opcode similarity".to_owned(),
            passed: fidelity_passed,
            measured: format!(
                "{:.6} across {} functions with {} explicit mismatches",
                report.opcode_fidelity.normalized_similarity,
                report.opcode_fidelity.functions_compared,
                report.opcode_fidelity.mismatches
            ),
            threshold: format!("{:.6}", options.minimum_opcode_similarity),
        },
        AcceptanceCriterion {
            requirement: 99,
            name: "crashlink readability comparison".to_owned(),
            passed: report.readability.crashlink_available && report.readability.passed,
            measured: format!(
                "{}/{} categories equivalent or better",
                report
                    .readability
                    .comparisons
                    .iter()
                    .filter(|comparison| comparison.equivalent_or_better)
                    .count(),
                report.readability.comparisons.len()
            ),
            threshold: "arrays, enums, loops, switches, and strings all equivalent or better"
                .to_owned(),
        },
        AcceptanceCriterion {
            requirement: 100,
            name: "interactive release performance".to_owned(),
            passed: report.benchmark.passed,
            measured: format!(
                "p{}={}us across {} samples",
                report.benchmark.percentile,
                report.benchmark.percentile_micros,
                report.benchmark.samples
            ),
            threshold: format!("<={}us in release mode", report.benchmark.budget_micros),
        },
        AcceptanceCriterion {
            requirement: 101,
            name: "explicit compilable fallback contract".to_owned(),
            passed: fallback_contract && recompilation_passed,
            measured: format!(
                "{} state machines, {} unhandled opcodes, {} fallback diagnostics",
                report.fallback_usage.state_machine_regions,
                report.fallback_usage.unhandled_opcodes,
                report.fallback_usage.diagnostics
            ),
            threshold: "every fallback has declared diagnostic coverage and output recompiles"
                .to_owned(),
        },
    ];
    let complete = criteria.iter().all(|criterion| criterion.passed);
    criteria.push(AcceptanceCriterion {
        requirement: 102,
        name: "complete machine-readable acceptance report".to_owned(),
        passed: complete,
        measured: "schema_version=2".to_owned(),
        threshold: "requirements 96-101 pass in one report".to_owned(),
    });
    AcceptanceSummary {
        passed: criteria.iter().all(|criterion| criterion.passed),
        criteria,
    }
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

#[derive(Default)]
struct StressItem {
    checked: bool,
    cfg_failure: Option<FailureRecord>,
    decompilation_failure: Option<FailureRecord>,
    fallback_usage: FallbackSummary,
    panics: PanicSummary,
}

fn stress_worker_count(items: usize) -> usize {
    let configured = std::env::var("HLBC_STRESS_THREADS")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0);
    configured
        .unwrap_or_else(|| {
            std::thread::available_parallelism()
                .map(usize::from)
                .unwrap_or(1)
                .min(8)
        })
        .min(items.max(1))
}

fn stress_functions_parallel(code: &Bytecode, workers: usize) -> Result<Vec<StressItem>, String> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    let trace_items = std::env::var_os("HLBC_STRESS_TRACE").is_some();
    let next = AtomicUsize::new(0);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for worker in 0..workers {
            let name = format!("hlbc-stress-functions-{worker}");
            let next = &next;
            let handle = std::thread::Builder::new()
                .name(name)
                .stack_size(64 * 1024 * 1024)
                .spawn_scoped(scope, move || {
                    let mut items = Vec::new();
                    loop {
                        let position = next.fetch_add(1, Ordering::Relaxed);
                        let Some(function) = code.functions.get(position) else {
                            break;
                        };
                        if trace_items {
                            eprintln!(
                                "hlboot stress: worker {worker} function {position} {} ({})",
                                safe_function_name(code, function),
                                function.findex.0
                            );
                        }
                        let item = match catch_unwind(AssertUnwindSafe(|| {
                            stress_function(code, function)
                        })) {
                            Ok(item) => item,
                            Err(payload) => {
                                let fixture = format!(
                                    "{} ({})",
                                    safe_function_name(code, function),
                                    function.findex.0
                                );
                                let mut item = StressItem::default();
                                record_panic(
                                    &mut item.panics,
                                    "decompiler",
                                    &fixture,
                                    panic_message(payload),
                                );
                                item
                            }
                        };
                        items.push((position, item));
                    }
                    items
                })
                .map_err(|error| format!("could not spawn function stress worker: {error}"))?;
            handles.push(handle);
        }
        let mut indexed = Vec::with_capacity(code.functions.len());
        for handle in handles {
            indexed.extend(handle.join().map_err(|payload| {
                format!(
                    "function stress worker panicked: {}",
                    panic_message(payload)
                )
            })?);
        }
        indexed.sort_by_key(|(position, _)| *position);
        Ok(indexed.into_iter().map(|(_, item)| item).collect())
    })
}

fn stress_function(code: &Bytecode, function: &Function) -> StressItem {
    let fixture = format!(
        "{} ({})",
        safe_function_name(code, function),
        function.findex.0
    );
    let trace_target = std::env::var("HLBC_STRESS_TRACE_FINDEX")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        == Some(function.findex.0);
    let mut item = StressItem::default();
    if trace_target {
        eprintln!("hlboot stress: {fixture} CFG start");
    }
    match catch_unwind(AssertUnwindSafe(|| ControlFlowGraph::build(function))) {
        Ok(Ok(_)) => {}
        Ok(Err(error)) => {
            item.cfg_failure = Some(FailureRecord {
                fixture: fixture.clone(),
                message: error.to_string(),
            })
        }
        Err(payload) => record_panic(&mut item.panics, "cfg", &fixture, panic_message(payload)),
    }
    if trace_target {
        eprintln!("hlboot stress: {fixture} CFG complete, decompiler start");
    }
    match catch_unwind(AssertUnwindSafe(|| {
        decompile_function_with_options(
            code,
            function,
            DecompileOptions {
                optimization_profile: OptimizationProfile::Fidelity,
                ..DecompileOptions::default()
            },
        )
    })) {
        Ok(Ok(decompiled)) => {
            if trace_target {
                eprintln!(
                    "hlboot stress: {fixture} decompiler complete, statement depth {}, formatter start",
                    statement_nesting_depth(&decompiled.value.statements)
                );
            }
            item.checked = true;
            item.fallback_usage = fallback_usage(
                std::iter::once(decompiled.value.statements.as_slice()),
                &decompiled.diagnostics,
            );
            if let Err(payload) = catch_unwind(AssertUnwindSafe(|| {
                decompiled
                    .value
                    .display(code, &FormatOptions::new(4))
                    .to_string()
            })) {
                record_panic(
                    &mut item.panics,
                    "formatter",
                    &fixture,
                    panic_message(payload),
                );
            }
            if trace_target {
                eprintln!("hlboot stress: {fixture} formatter complete");
            }
        }
        Ok(Err(error)) => {
            item.decompilation_failure = Some(FailureRecord {
                fixture,
                message: error.to_string(),
            })
        }
        Err(payload) => record_panic(
            &mut item.panics,
            "decompiler",
            &fixture,
            panic_message(payload),
        ),
    }
    item
}

fn statement_nesting_depth(statements: &[Statement]) -> usize {
    let mut depth = 0;
    let mut stack = vec![(statements, 1usize)];
    while let Some((statements, current)) = stack.pop() {
        depth = depth.max(current);
        for statement in statements {
            match statement {
                Statement::IfElse { if_, else_, .. } => {
                    stack.push((if_, current + 1));
                    stack.push((else_, current + 1));
                }
                Statement::Switch { default, cases, .. } => {
                    stack.push((default, current + 1));
                    for (_, body) in cases {
                        stack.push((body, current + 1));
                    }
                }
                Statement::While { stmts, .. }
                | Statement::DoWhile { stmts, .. }
                | Statement::ForEach { stmts, .. }
                | Statement::ForRange { stmts, .. }
                | Statement::Try { stmts }
                | Statement::Catch { stmts } => stack.push((stmts, current + 1)),
                Statement::TryCatch { try_stmts, catches } => {
                    stack.push((try_stmts, current + 1));
                    for catch in catches {
                        stack.push((&catch.stmts, current + 1));
                    }
                }
                Statement::StateMachine { blocks, .. } => {
                    for block in blocks {
                        stack.push((&block.stmts, current + 1));
                    }
                }
                Statement::Provenanced { statement, .. } => {
                    stack.push((std::slice::from_ref(statement), current + 1))
                }
                _ => {}
            }
        }
    }
    depth
}

fn stress_classes_parallel(code: &Bytecode, workers: usize) -> Result<Vec<StressItem>, String> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    let classes: Vec<_> = code
        .types
        .iter()
        .filter_map(|ty| match ty {
            Type::Obj(object) | Type::Struct(object) => Some(object),
            _ => None,
        })
        .collect();
    let next = AtomicUsize::new(0);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for worker in 0..workers {
            let name = format!("hlbc-stress-classes-{worker}");
            let next = &next;
            let classes = &classes;
            let handle = std::thread::Builder::new()
                .name(name)
                .stack_size(64 * 1024 * 1024)
                .spawn_scoped(scope, move || {
                    let mut items = Vec::new();
                    loop {
                        let position = next.fetch_add(1, Ordering::Relaxed);
                        let Some(object) = classes.get(position) else {
                            break;
                        };
                        let item =
                            match catch_unwind(AssertUnwindSafe(|| stress_class(code, object))) {
                                Ok(item) => item,
                                Err(payload) => {
                                    let fixture = safe_class_name(code, object);
                                    let mut item = StressItem::default();
                                    record_panic(
                                        &mut item.panics,
                                        "decompiler",
                                        &fixture,
                                        panic_message(payload),
                                    );
                                    item
                                }
                            };
                        items.push((position, item));
                    }
                    items
                })
                .map_err(|error| format!("could not spawn class stress worker: {error}"))?;
            handles.push(handle);
        }
        let mut indexed = Vec::with_capacity(classes.len());
        for handle in handles {
            indexed.extend(handle.join().map_err(|payload| {
                format!("class stress worker panicked: {}", panic_message(payload))
            })?);
        }
        indexed.sort_by_key(|(position, _)| *position);
        Ok(indexed.into_iter().map(|(_, item)| item).collect())
    })
}

fn stress_class(code: &Bytecode, object: &TypeObj) -> StressItem {
    let fixture = safe_class_name(code, object);
    let mut item = StressItem::default();
    match catch_unwind(AssertUnwindSafe(|| {
        decompile_class_with_options(
            code,
            object,
            DecompileOptions {
                optimization_profile: OptimizationProfile::Fidelity,
                ..DecompileOptions::default()
            },
        )
    })) {
        Ok(Ok(decompiled)) => {
            item.checked = true;
            item.fallback_usage = fallback_usage(
                decompiled
                    .value
                    .methods
                    .iter()
                    .map(|method| method.statements.as_slice()),
                &decompiled.diagnostics,
            );
            if let Err(payload) = catch_unwind(AssertUnwindSafe(|| {
                decompiled
                    .value
                    .display(code, &FormatOptions::new(4))
                    .to_string()
            })) {
                record_panic(
                    &mut item.panics,
                    "formatter",
                    &fixture,
                    panic_message(payload),
                );
            }
        }
        Ok(Err(error)) => {
            item.decompilation_failure = Some(FailureRecord {
                fixture,
                message: error.to_string(),
            })
        }
        Err(payload) => record_panic(
            &mut item.panics,
            "decompiler",
            &fixture,
            panic_message(payload),
        ),
    }
    item
}

fn safe_class_name(code: &Bytecode, object: &TypeObj) -> String {
    code.strings
        .get(object.name.0)
        .map(ToString::to_string)
        .unwrap_or_else(|| format!("class-string:{}", object.name.0))
}

fn merge_stress_item(report: &mut StressReport, item: StressItem, class: bool) {
    if item.checked {
        if class {
            report.classes_checked += 1;
        } else {
            report.functions_checked += 1;
        }
    }
    if let Some(failure) = item.cfg_failure {
        report.cfg_failures += 1;
        report.cfg_failure_details.push(failure);
    }
    if let Some(failure) = item.decompilation_failure {
        report.decompilation_failures += 1;
        report.decompilation_failure_details.push(failure);
    }
    if item.panics.cfg > 0 {
        report.cfg_failures += item.panics.cfg;
    }
    let decompilation_panics = item.panics.decompiler + item.panics.formatter + item.panics.parser;
    report.decompilation_failures += decompilation_panics;
    report.panics.parser += item.panics.parser;
    report.panics.cfg += item.panics.cfg;
    report.panics.decompiler += item.panics.decompiler;
    report.panics.formatter += item.panics.formatter;
    report.panics.total += item.panics.total;
    report.panics.details.extend(item.panics.details);
    report.fallback_usage.add(&item.fallback_usage);
}

fn run_stress(path: Option<&Path>) -> StressReport {
    let Some(path) = path else {
        return StressReport::default();
    };
    let started = Instant::now();
    let mut report = StressReport {
        supplied: true,
        ..StressReport::default()
    };
    match catch_unwind(AssertUnwindSafe(|| Bytecode::from_file(path))) {
        Ok(Ok(code)) => {
            report.parsed = true;
            report.functions = code.functions.len();
            report.classes = code
                .types
                .iter()
                .filter(|ty| matches!(ty, Type::Obj(_) | Type::Struct(_)))
                .count();
            let workers = stress_worker_count(report.functions.max(report.classes));
            report.workers = workers;
            eprintln!(
                "hlboot stress: {} functions and {} classes on {} workers",
                report.functions, report.classes, workers
            );
            match stress_functions_parallel(&code, workers) {
                Ok(items) => {
                    for item in items {
                        merge_stress_item(&mut report, item, false);
                    }
                }
                Err(error) => report.error = Some(error),
            }
            eprintln!(
                "hlboot stress: functions complete ({}/{})",
                report.functions_checked, report.functions
            );
            match stress_classes_parallel(&code, workers) {
                Ok(items) => {
                    for item in items {
                        merge_stress_item(&mut report, item, true);
                    }
                }
                Err(error) => {
                    report.error = Some(match report.error.take() {
                        Some(previous) => format!("{previous}; {error}"),
                        None => error,
                    })
                }
            }
            eprintln!(
                "hlboot stress: classes complete ({}/{})",
                report.classes_checked, report.classes
            );
            report
                .cfg_failure_details
                .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
            report
                .decompilation_failure_details
                .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
            report
                .panics
                .details
                .sort_by(|a, b| (&a.fixture, &a.message).cmp(&(&b.fixture, &b.message)));
        }
        Ok(Err(error)) => report.error = Some(error.to_string()),
        Err(payload) => {
            record_panic(
                &mut report.panics,
                "parser",
                &normalized_path(path),
                panic_message(payload),
            );
            report.error = Some("parser panicked; see panic details".to_owned());
        }
    }
    report.elapsed_millis = started.elapsed().as_millis().min(u128::from(u64::MAX)) as u64;
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

fn compare_execution_results(
    original: &ExecutionResult,
    recompiled: &ExecutionResult,
) -> (bool, Vec<String>) {
    if original == recompiled {
        return (true, Vec::new());
    }
    let original_stdout = normalize_function_addresses(&original.stdout);
    let recompiled_stdout = normalize_function_addresses(&recompiled.stdout);
    let original_stderr = normalize_function_addresses(&original.stderr);
    let recompiled_stderr = normalize_function_addresses(&recompiled.stderr);
    let normalized = original_stdout == recompiled_stdout
        && original_stderr == recompiled_stderr
        && original.exception == recompiled.exception
        && original.exit_status == recompiled.exit_status
        && original.timed_out == recompiled.timed_out;
    let normalizations = if normalized {
        vec!["HashLink function pointer addresses".to_owned()]
    } else {
        Vec::new()
    };
    (normalized, normalizations)
}

fn normalize_function_addresses(value: &str) -> String {
    let bytes = value.as_bytes();
    let marker = b"function#";
    let mut output = String::with_capacity(value.len());
    let mut cursor = 0;
    while cursor < bytes.len() {
        let Some(relative) = bytes[cursor..]
            .windows(marker.len())
            .position(|window| window == marker)
        else {
            output.push_str(&value[cursor..]);
            break;
        };
        let start = cursor + relative;
        output.push_str(&value[cursor..start + marker.len()]);
        output.push_str("<address>");
        let mut end = start + marker.len();
        while end < bytes.len() && bytes[end].is_ascii_hexdigit() {
            end += 1;
        }
        cursor = end;
    }
    output
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
    fn runtime_comparison_only_normalizes_documented_function_addresses() {
        assert_eq!(
            normalize_function_addresses("function#ABC123 and function#9f"),
            "function#<address> and function#<address>"
        );
        assert_eq!(normalize_function_addresses("other#ABC123"), "other#ABC123");
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
        for fixture in [
            "LoopWhile",
            "LoopInfinite",
            "LoopForEach",
            "LoopRange",
            "TryCatch",
        ] {
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

    #[test]
    fn milestone_five_haxe_recovery_golden() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let actual = render_fixture_golden(&manifest.join("../../data/HaxeRecovery.hl"))
            .unwrap()
            .haxe;
        let expected = fs::read_to_string(manifest.join("tests/goldens/HaxeRecovery.recovery.hx"))
            .unwrap()
            .replace("\r\n", "\n");
        assert_eq!(expected, actual, "Haxe recovery golden changed");
    }

    #[test]
    fn milestone_six_collection_and_string_recovery_golden() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let actual = render_fixture_golden(&manifest.join("../../data/MapLiteral.hl"))
            .unwrap()
            .haxe;
        let expected = fs::read_to_string(manifest.join("tests/goldens/MapLiteral.recovery.hx"))
            .unwrap()
            .replace("\r\n", "\n");
        assert_eq!(expected, actual, "safe recovery golden changed");
    }

    #[test]
    fn conflicting_ssa_debug_names_are_declared_once() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let interpolation = render_fixture_golden(&manifest.join("../../data/StringInterp.hl"))
            .unwrap()
            .haxe;
        assert_eq!(
            interpolation
                .lines()
                .filter(|line| line.trim_start().starts_with("var b ="))
                .count(),
            1
        );
        assert!(interpolation.contains("var b__hl_"));

        let enums = render_fixture_golden(&manifest.join("../../data/Enums.hl"))
            .unwrap()
            .haxe;
        assert_eq!(
            enums
                .lines()
                .filter(|line| line.trim_start().starts_with("var b:"))
                .count(),
            1
        );
    }

    #[test]
    fn readability_profile_structures_switches_and_removes_trace_metadata() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        for fixture in ["Enums", "Switch"] {
            let code =
                Bytecode::from_file(manifest.join(format!("../../data/{fixture}.hl"))).unwrap();
            let rendered = render_fixture_with_options(
                &code,
                fixture,
                DecompileOptions {
                    optimization_profile: OptimizationProfile::Readability,
                    include_unreachable: false,
                    ..DecompileOptions::default()
                },
            )
            .unwrap();
            assert!(rendered.golden.haxe.contains("switch ("));
            assert!(!rendered.golden.haxe.contains("__hl_state"));
        }

        let code = Bytecode::from_file(manifest.join("../../data/Arrays.hl")).unwrap();
        let arrays = render_fixture_with_options(
            &code,
            "Arrays",
            DecompileOptions {
                optimization_profile: OptimizationProfile::Readability,
                include_unreachable: false,
                ..DecompileOptions::default()
            },
        )
        .unwrap()
        .golden
        .haxe;
        assert!(arrays.contains("trace(a)"));
        assert!(!arrays.contains("Reflect.setField"), "{arrays}");
        assert!(!arrays.contains("HashLink null check: haxe__Log.trace"));
    }

    #[test]
    #[ignore = "requires a crashlink checkout; set HLBC_CRASHLINK"]
    fn shared_readability_is_at_least_crashlink_for_five_categories() {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let crashlink = std::env::var_os("HLBC_CRASHLINK")
            .map(PathBuf::from)
            .unwrap_or_else(|| manifest.join("../../../../../crashlink"));
        assert!(
            crashlink.is_dir(),
            "crashlink checkout is missing at {}",
            crashlink.display()
        );
        let output = tempfile::tempdir().unwrap();
        let fixtures = manifest.join("../../data");
        for fixture in ["Arrays", "Enums", "LoopWhile", "Switch", "StringInterp"] {
            let code = Bytecode::from_file(fixtures.join(format!("{fixture}.hl"))).unwrap();
            let rendered = render_fixture_with_options(
                &code,
                fixture,
                DecompileOptions {
                    optimization_profile: OptimizationProfile::Readability,
                    include_unreachable: false,
                    ..DecompileOptions::default()
                },
            )
            .unwrap();
            let directory = output.path().join(fixture).join("readable");
            fs::create_dir_all(&directory).unwrap();
            fs::write(
                directory.join(format!("{fixture}.hx")),
                rendered.golden.haxe,
            )
            .unwrap();
        }
        let options = HarnessOptions {
            fixtures_dir: fixtures,
            output_dir: output.path().to_path_buf(),
            crashlink: Some(crashlink),
            ..HarnessOptions::default()
        };
        let comparison = run_readability_comparison(&options);
        assert_eq!(comparison.comparisons.len(), 5, "{:?}", comparison.error);
        assert!(
            comparison.passed,
            "{}",
            serde_json::to_string_pretty(&comparison).unwrap()
        );
    }

    #[test]
    #[ignore = "run with cargo test --release and --ignored"]
    fn ordinary_function_release_p95_stays_below_fifty_milliseconds() {
        assert!(
            !cfg!(debug_assertions),
            "this benchmark must run with cargo test --release"
        );
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let mut samples = Vec::new();
        for fixture in ["Empty", "Branch", "Arrays", "LoopWhile", "Switch"] {
            let code =
                Bytecode::from_file(manifest.join(format!("../../data/{fixture}.hl"))).unwrap();
            let function = code.function_by_name("main").unwrap();
            for _ in 0..10 {
                let started = Instant::now();
                decompile_code_with_options(
                    &code,
                    function,
                    DecompileOptions {
                        optimization_profile: OptimizationProfile::Balanced,
                        ..DecompileOptions::default()
                    },
                )
                .unwrap();
                samples.push(BenchmarkSample {
                    fixture: fixture.to_owned(),
                    function: "main".to_owned(),
                    function_index: function.findex.0,
                    duration_micros: duration_micros(started.elapsed()),
                });
            }
        }
        let mut benchmark = BenchmarkSummary {
            release_mode: true,
            iterations: 10,
            percentile: 95,
            budget_micros: 50_000,
            ..BenchmarkSummary::default()
        };
        finish_benchmark(&mut benchmark, samples, Duration::from_millis(50));
        assert!(
            benchmark.passed,
            "release p95 was {}us",
            benchmark.percentile_micros
        );
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

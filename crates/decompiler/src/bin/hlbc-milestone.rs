use std::collections::BTreeSet;
use std::path::PathBuf;
use std::time::Duration;

use hlbc_decompiler::harness::{run_milestone, HarnessOptions};

fn main() {
    match run() {
        Ok(()) => {}
        Err(message) => {
            eprintln!("{message}");
            std::process::exit(1);
        }
    }
}

fn run() -> Result<(), String> {
    let mut options = HarnessOptions::default();
    let mut golden_fixtures = BTreeSet::new();
    let mut final_acceptance = false;
    let mut args = std::env::args_os().skip(1);
    while let Some(argument) = args.next() {
        match argument.to_string_lossy().as_ref() {
            "--fixtures" => {
                options.fixtures_dir = PathBuf::from(required(&mut args, "--fixtures")?)
            }
            "--output-dir" => {
                options.output_dir = PathBuf::from(required(&mut args, "--output-dir")?)
            }
            "--goldens-dir" => {
                options.goldens_dir = PathBuf::from(required(&mut args, "--goldens-dir")?)
            }
            "--report" => options.report_path = PathBuf::from(required(&mut args, "--report")?),
            "--hlboot" => options.hlboot = Some(PathBuf::from(required(&mut args, "--hlboot")?)),
            "--crashlink" => {
                options.crashlink = Some(PathBuf::from(required(&mut args, "--crashlink")?))
            }
            "--timeout-seconds" => {
                let value = required(&mut args, "--timeout-seconds")?;
                let seconds = value
                    .to_string_lossy()
                    .parse::<u64>()
                    .map_err(|_| "--timeout-seconds must be an integer".to_owned())?;
                options.timeout = Duration::from_secs(seconds);
            }
            "--minimum-fixtures" => {
                options.minimum_fixtures = parse_usize(
                    required(&mut args, "--minimum-fixtures")?,
                    "--minimum-fixtures",
                )?;
            }
            "--minimum-similarity" => {
                let value = required(&mut args, "--minimum-similarity")?;
                options.minimum_opcode_similarity = value
                    .to_string_lossy()
                    .parse::<f64>()
                    .map_err(|_| "--minimum-similarity must be a number".to_owned())?;
            }
            "--benchmark-iterations" => {
                options.benchmark_iterations = parse_usize(
                    required(&mut args, "--benchmark-iterations")?,
                    "--benchmark-iterations",
                )?;
            }
            "--interactive-budget-ms" => {
                let millis = parse_usize(
                    required(&mut args, "--interactive-budget-ms")?,
                    "--interactive-budget-ms",
                )?;
                options.interactive_budget = Duration::from_millis(millis as u64);
            }
            "--golden-fixture" => {
                golden_fixtures.insert(
                    required(&mut args, "--golden-fixture")?
                        .to_string_lossy()
                        .into_owned(),
                );
            }
            "--update-goldens" => options.update_goldens = true,
            "--no-execute" => options.execute = false,
            "--final-acceptance" => final_acceptance = true,
            "--help" | "-h" => {
                print_help();
                return Ok(());
            }
            other => return Err(format!("unknown argument `{other}`; use --help")),
        }
    }
    if !golden_fixtures.is_empty() {
        options.golden_fixtures = golden_fixtures;
    }

    let report = run_milestone(&options).map_err(|error| error.to_string())?;
    println!(
        "fixtures={} parsed={} decompiled={} recompiled={} opcode_similarity={:.6} report={}",
        report.fixture_counts.discovered,
        report.fixture_counts.parsed,
        report.fixture_counts.decompiled,
        report.fixture_counts.recompiled,
        report.opcode_fidelity.normalized_similarity,
        options.report_path.display()
    );
    if !report.parse_failures.is_empty()
        || !report.decompilation_failures.is_empty()
        || !report.recompilation_failures.is_empty()
        || !report.execution_mismatches.is_empty()
        || (report.stress_test.supplied
            && (!report.stress_test.parsed
                || report.stress_test.cfg_failures > 0
                || report.stress_test.decompilation_failures > 0
                || report.stress_test.panics.total > 0))
        || (final_acceptance && !report.acceptance.passed)
    {
        return Err("acceptance gate failed; inspect the machine-readable JSON report".to_owned());
    }
    Ok(())
}

fn required(
    args: &mut impl Iterator<Item = std::ffi::OsString>,
    flag: &str,
) -> Result<std::ffi::OsString, String> {
    args.next()
        .ok_or_else(|| format!("{flag} requires a value"))
}

fn parse_usize(value: std::ffi::OsString, flag: &str) -> Result<usize, String> {
    value
        .to_string_lossy()
        .parse::<usize>()
        .map_err(|_| format!("{flag} must be a non-negative integer"))
}

fn print_help() {
    println!(
        "hlbc-milestone [OPTIONS]\n\
         \n\
         --fixtures PATH          Directory containing compiled .hl fixtures\n\
         --output-dir PATH        Generated Haxe and recompiled bytecode directory\n\
         --goldens-dir PATH       Golden file directory\n\
         --report PATH            Deterministic JSON report path\n\
         --golden-fixture NAME    Fixture checked against goldens (repeatable)\n\
         --update-goldens         Explicitly replace AST and Haxe goldens\n\
         --hlboot PATH            Optional hlboot.dat stress test (or HLBC_HLBOOT)\n\
         --crashlink PATH         Crashlink checkout for readability comparisons\n\
         --timeout-seconds N      Recompile/execution timeout\n\
         --minimum-fixtures N      Required discovered/recompiled fixture count (default 20)\n\
         --minimum-similarity N    Required average opcode similarity (default 0.95)\n\
         --benchmark-iterations N  Per-fixture interactive benchmark samples (default 5)\n\
         --interactive-budget-ms N p95 release-mode latency budget (default 50)\n\
         --no-execute             Skip HashLink runtime comparisons\n\
         --final-acceptance       Require every final acceptance criterion"
    );
}

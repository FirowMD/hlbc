use std::path::PathBuf;

use hlbc::Bytecode;
use hlbc_decompiler::api::v1::{decompile_project, ProjectOptions};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = std::env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../../data/Empty.hl"));
    let output = std::env::args_os()
        .nth(2)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("target/recovered-project"));
    let code = Bytecode::from_file(input)?;
    let result = decompile_project(&code, output, ProjectOptions::default())?;
    println!(
        "wrote {} files for {} declarations with {} worker(s)",
        result.generated_files.len(),
        result.graph.units.len(),
        result.workers_used
    );
    Ok(())
}

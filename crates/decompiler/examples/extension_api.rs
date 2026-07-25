use std::path::PathBuf;

use hlbc::Bytecode;
use hlbc_decompiler::api::v1::{
    run_verified_pass, ExtensionPass, ExtensionPassError, ExtensionPassOutput, OptimizedIr,
    PassContext, TypedIr,
};

struct CountOperations;

impl ExtensionPass for CountOperations {
    fn name(&self) -> &'static str {
        "count-operations"
    }

    fn run(
        &self,
        _context: &PassContext<'_>,
        input: &OptimizedIr,
    ) -> Result<ExtensionPassOutput, ExtensionPassError> {
        let count = input
            .ir
            .blocks
            .iter()
            .map(|block| block.operations.len())
            .sum::<usize>();
        println!(
            "function {} has {count} IR operations",
            input.ir.function_index
        );
        Ok(ExtensionPassOutput::unchanged(input.clone()))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../../data/Empty.hl"));
    let code = Bytecode::from_file(path)?;
    let function = code.main().or_else(|_| code.entrypoint())?;
    let ir = TypedIr::build(&code, function)?.value;
    run_verified_pass(&CountOperations, &OptimizedIr::new(ir))?;
    Ok(())
}

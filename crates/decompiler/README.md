# hlbc-decompiler [![Crates.io](https://img.shields.io/crates/v/hlbc-decompiler?label=hlbc-decompiler)](https://crates.io/crates/hlbc-decompiler)

[**H**ash**l**ink](https://hashlink.haxe.org/) **b**yte**c**ode disassembler and analyzer.

*This crate is a library, see [hlbc-cli](https://crates.io/crates/hlbc-cli) for an actual program to use.*

---

[See the wiki](https://github.com/Gui-Yom/hlbc/wiki/Decompilation) for examples of decompilation output.

## Foundation test gate

Compile all fixtures and run the deterministic milestone report:

```shell
just fixtures
cargo run -p hlbc-decompiler --bin hlbc-milestone -- --no-execute
```

Update the committed AST and formatted-Haxe goldens explicitly with:

```shell
cargo run -p hlbc-decompiler --bin hlbc-milestone -- --update-goldens --no-execute
```

Supply a large stress input with `--hlboot /path/to/hlboot.dat` or the `HLBC_HLBOOT`
environment variable. Public decompilation functions return
`Result<Decompiled<T>, DecompileError>`; use `DecompileOptions` to select strict or
best-effort behavior.

## Typed IR

`ir::TypedIr::build(&bytecode, &function)` constructs the verified typed SSA IR
used by decompilation. The IR preserves original opcodes and exact opcode-range
provenance, exposes definition/use chains and recovered locals, and classifies
effects and possible exceptions before AST optimizations.

## Safe optimization profiles

`DecompileOptions::optimization_profile` selects one deterministic pipeline:

| Profile | Ordered passes |
| --- | --- |
| `fidelity` | IR verification |
| `balanced` | IR verification, SSA constant/copy propagation, effect-aware single-use temporary inlining |
| `readability` | all balanced passes, verified dead-store and dead-code elimination |

`balanced` is the default. The bytecode-exact `TypedIr` is immutable; passes
produce an `OptimizedIr` overlay containing SSA aliases, inline candidates, and
elimination decisions. Lowering consumes that overlay while CFG construction
and provenance continue to use the exact source IR.

Every pass runs transactionally. Its input and candidate output are verified,
and an invalid candidate is discarded with a provenance-bearing diagnostic.
Inlining is limited to pure, non-throwing, single-use definitions and cannot
cross calls, effects, allocations, exception points, or ordered operations.
Dead-store elimination uses explicit global effects and stops at reads, calls,
aliasing, raw memory, control flow, or exception barriers.

```rust
use hlbc_decompiler::{DecompileOptions, OptimizationProfile};

let options = DecompileOptions {
    optimization_profile: OptimizationProfile::Readability,
    trace_optimizations: true,
    ..DecompileOptions::default()
};
```

With `trace_optimizations`, each `Decompiled<T>` retains an
`optimization_traces` entry per function. Every pass record contains its stable
pass name, applied/rollback state, before/after validity, changed operation
provenance, diagnostics, and deterministic JSON before/after snapshots. Tracing
is disabled by default.

## Haxe recovery passes

Constructor and anonymous-object finalization retains argument/field evaluation
order. Array builders fold only complete ordered writes. Map recovery folds a
map constructor plus consecutive `set` operations only when temporary uses are
exclusive, there is no intervening alias, and key evaluation remains before
value evaluation. String/trace recovery recognizes both direct and static-field
runtime helpers and preserves conversion order.

## Project decompilation and analysis

`project::decompile_project` discovers declarations from the source-level
`main` function (or explicit roots), follows type and call dependencies, and
writes a deterministic Haxe package tree:

```shell
cargo run -p hlbc-decompiler --example decompile_project -- \
  ../../data/Empty.hl target/recovered-project
haxe target/recovered-project/build.hxml
```

The output contains `src/`, `build.hxml`, `bytesto4t-project.json`, and stable
JSON analysis/diagnostic reports. Only files listed by the previous project
manifest are removed on regeneration.

`interprocedural::analyze_program_with_cache` recovers finite type, constant,
closure, and likely-call-target sets. Incomplete sets explicitly represent
dynamic dispatch, escaped values, or conservative widening. `AnalysisCache`
keys each summary by function bytecode, analysis configuration, and transitive
dependency fingerprints; a changed callee invalidates its callers.

Function workers publish isolated artifacts through
`parallel::decompile_functions_parallel`. The coordinator sorts function
results, diagnostics, annotations, filenames, and reports, so worker count and
scheduling do not affect output.

## Provenance and extension API

Every `Decompiled<T>` exposes provenance-indexed `recovery_annotations` with a
bounded `Confidence` and explicit `Approximation` values. Declarative compiler
patterns are registered with `patterns::PatternRegistry`; matches include
captures, validation results, confidence, and exact opcode provenance.

`views::decompile_synchronized_views` returns faithful and readable Haxe
documents backed by one typed IR. Stable opcode links identify the IR
operations and source nodes in both documents. Recompilation reports use
`divergence::attribute_opcode_divergences` to identify the smallest AST and IR
region responsible for each opcode replacement, insertion, or deletion.

Extensions should import the versioned `hlbc_decompiler::api::v1` facade. It
stabilizes pass, IR, provenance, diagnostic, pattern, project, and view types.
Custom IR passes implement `ExtensionPass` and run through
`run_verified_pass`, which rejects invalid input or output transactionally:

```shell
cargo run -p hlbc-decompiler --example extension_api -- ../../data/Empty.hl
```

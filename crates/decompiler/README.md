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
effects and possible exceptions before AST optimizations. Existing Haxe output
currently uses the IR compatibility stream while lowering migrates by opcode
family.

# hlbc-enhanced-private
Enhanced HLBC

## Final acceptance

Build fixtures before running tests:

```shell
sh scripts/compile-fixtures.sh
cargo test --workspace --all-targets
```

Run the complete release acceptance gate with the optional large stress input
and a crashlink checkout:

```shell
HLBC_STRESS_THREADS=8 cargo run --release -p hlbc-decompiler \
  --bin hlbc-milestone -- \
  --final-acceptance \
  --hlboot /path/to/hlboot.dat \
  --crashlink /path/to/crashlink \
  --report target/final-acceptance.json
```

The schema-version 2 JSON report contains fixture parsing/decompilation and
recompilation, original-versus-generated execution, per-function opcode
divergences, opcode coverage, parser/CFG/decompiler/formatter panics, fallback
usage, crashlink readability comparisons, release p95 latency, and all final
acceptance criteria. Fidelity output is written under
`target/milestone/<fixture>/generated`; readability output is written under
`target/milestone/<fixture>/readable`.

External comparison and benchmark tests can also be run independently:

```shell
HLBC_CRASHLINK=/path/to/crashlink cargo test --release -p hlbc-decompiler \
  shared_readability_is_at_least_crashlink_for_five_categories -- --ignored
cargo test --release -p hlbc-decompiler \
  ordinary_function_release_p95_stays_below_fifty_milliseconds -- --ignored
```

Focused recovery goldens are only replaced by the explicit update command:

```shell
cargo run -p hlbc-decompiler --bin hlbc-milestone -- \
  --update-goldens \
  --golden-fixture HaxeRecovery \
  --golden-fixture MapLiteral \
  --no-execute
```

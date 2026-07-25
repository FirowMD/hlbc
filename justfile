set windows-shell := ["cmd", "/c"]

default:
    just --list

data file:
    just -d data --justfile data/justfile build {{file}}

fixtures:
    powershell -NoProfile -ExecutionPolicy Bypass -File scripts/compile-fixtures.ps1

milestone *args:
    cargo run -p hlbc-decompiler --bin hlbc-milestone -- {{args}}

update-goldens:
    cargo run -p hlbc-decompiler --bin hlbc-milestone -- --update-goldens --no-execute

release-benchmark:
    cargo test --release -p hlbc-decompiler ordinary_function_release_p95_stays_below_fifty_milliseconds -- --ignored

final-acceptance hlboot crashlink:
    cargo run --release -p hlbc-decompiler --bin hlbc-milestone -- --final-acceptance --hlboot {{hlboot}} --crashlink {{crashlink}} --report target/final-acceptance.json

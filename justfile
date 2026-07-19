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

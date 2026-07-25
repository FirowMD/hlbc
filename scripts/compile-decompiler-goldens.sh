#!/usr/bin/env sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
goldens="$root/crates/decompiler/tests/goldens"
output="$root/target/compile-goldens"
mkdir -p "$output"

count=0
for source in \
    "$goldens"/Empty.hx \
    "$goldens"/*.compile.hx \
    "$goldens"/*.control.hx \
    "$goldens"/*.recovery.hx; do
    test -f "$source" || continue
    file=$(basename "$source")
    name=${file%%.*}
    cp "$source" "$output/$name.hx"
    haxe -cp "$output" -main "$name" -hl "$output/$name.hl"
    count=$((count + 1))
done

test "$count" -gt 0 || {
    echo "No decompiler Haxe goldens were found" >&2
    exit 1
}
echo "Compiled $count decompiler Haxe goldens"

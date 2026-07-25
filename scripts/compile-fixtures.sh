#!/usr/bin/env sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
count=0
for source in "$root"/data/*.hx; do
    test -f "$source" || { echo "No data/*.hx fixtures were found" >&2; exit 1; }
    name=$(basename "$source" .hx)
    (cd "$root" && haxe -cp data -main "$name" -hl "data/$name.hl")
    count=$((count + 1))
done

compiled=$(find "$root/data" -maxdepth 1 -type f -name '*.hl' | wc -l | tr -d ' ')
test "$compiled" -eq "$count" || {
    echo "Compiled $compiled .hl files for $count .hx fixtures" >&2
    exit 1
}
echo "Compiled $compiled HashLink fixtures"

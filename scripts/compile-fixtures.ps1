$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $PSScriptRoot
$fixtures = Get-ChildItem -LiteralPath (Join-Path $root "data") -Filter *.hx | Sort-Object Name
if ($fixtures.Count -eq 0) {
    throw "No data/*.hx fixtures were found"
}

Push-Location $root
try {
    foreach ($fixture in $fixtures) {
        $name = $fixture.BaseName
        & haxe -cp data -main $name -hl "data/$name.hl"
        if ($LASTEXITCODE -ne 0) {
            throw "Haxe failed to compile $($fixture.Name)"
        }
    }
} finally {
    Pop-Location
}

$compiled = @(Get-ChildItem -LiteralPath (Join-Path $root "data") -Filter *.hl)
if ($compiled.Count -ne $fixtures.Count) {
    throw "Compiled $($compiled.Count) .hl files for $($fixtures.Count) .hx fixtures"
}
Write-Output "Compiled $($compiled.Count) HashLink fixtures"

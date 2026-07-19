$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $PSScriptRoot
$fixtures = Get-ChildItem -LiteralPath (Join-Path $root "data") -Filter *.hx | Sort-Object Name
if ($fixtures.Count -eq 0) {
    throw "No data/*.hx fixtures were found"
}

foreach ($fixture in $fixtures) {
    $name = $fixture.BaseName
    & haxe -cp (Join-Path $root "data") -main $name -hl (Join-Path (Join-Path $root "data") "$name.hl")
    if ($LASTEXITCODE -ne 0) {
        throw "Haxe failed to compile $($fixture.Name)"
    }
}

$compiled = @(Get-ChildItem -LiteralPath (Join-Path $root "data") -Filter *.hl)
if ($compiled.Count -ne $fixtures.Count) {
    throw "Compiled $($compiled.Count) .hl files for $($fixtures.Count) .hx fixtures"
}
Write-Output "Compiled $($compiled.Count) HashLink fixtures"

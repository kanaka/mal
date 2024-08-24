$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1

# READ
function READ([String] $str) {
    return read_str($str)
}

# EVAL
function EVAL($ast, $env) {
    # Write-Host "EVAL: $(pr_str $ast)"

    if ($ast -eq $null) { return $ast }
    switch ($ast.GetType().Name) {
        "Symbol"  { return $env[$ast.value] }
        "List" { }  # continue after the switch
        "Vector"  { return new-vector @($ast.values | ForEach-Object { EVAL $_ $env }) }
        "HashMap" {
            $hm = new-hashmap @()
            foreach ($k in $ast.values.Keys) {
                $hm.values[$k] = EVAL $ast.values[$k] $env
            }
            return $hm
        }
        default   { return $ast }
    }

    if (empty? $ast) { return $ast }

    $f = ( EVAL $ast.first() $env )
    $fargs = @($ast.rest().values | ForEach-Object { EVAL $_ $env })
    return &$f @fargs
}

# PRINT
function PRINT($exp) {
    return pr_str $exp $true
}

# REPL
# Case sensitive hashtable
$repl_env = New-Object System.Collections.HashTable
$repl_env["+"] = { param($a, $b); $a + $b }
$repl_env["-"] = { param($a, $b); $a - $b }
$repl_env["*"] = { param($a, $b); $a * $b }
$repl_env["/"] = { param($a, $b); $a / $b }

function REP([String] $str) {
    return PRINT (EVAL (READ $str) $repl_env)
}

while ($true) {
    Write-Host "user> " -NoNewline
    $line = [Console]::ReadLine()
    if ($line -eq $null) {
        break
    }
    try {
        Write-Host (REP($line))
    } catch {
        Write-Host "Exception: $($_.Exception.Message)"
    }
}

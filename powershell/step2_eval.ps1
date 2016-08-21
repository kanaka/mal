$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1

# READ
function READ([String] $str) {
    return read_str($str)
}

# EVAL
function eval_ast($ast, $env) {
    switch ($ast.GetType().Name) {
        "Symbol"  { return $env[$ast.value] }
        "List"    { return new-list ($ast.values | ForEach { EVAL $_ $env }) }
        "Vector"  { return new-vector ($ast.values | ForEach { EVAL $_ $env }) }
        "HashMap" {
            $hm = new-hashmap @()
            foreach ($k in $ast.values.Keys) {
                $hm.values[$k] = EVAL $ast.values[$k] $env
            }
            return $hm
        }
        default   { return $ast }
    }
}

function EVAL($ast, $env) {
    #Write-Host "EVAL $(pr_str $ast)"
    if (-not (list? $ast)) {
        return (eval_ast $ast $env)
    }
    if (empty? $ast) { return $ast }

    $el = (eval_ast $ast $env)
    $f, $fargs = $el.first(), $el.rest().values
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

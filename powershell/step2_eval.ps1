$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1

function READ([String] $str) {
    return read_str($str)
}

function eval_ast($ast, $env) {
    switch ($ast.GetType().Name) {
        "Symbol" { return $env[$ast.value] }
        "List"   { return new-list($ast.values | ForEach { EVAL $_ $env } ) }
        default  { return $ast }
    }
}

function EVAL($ast, $env) {
    if (-not (list? $ast)) {
        return (eval_ast $ast $env)
    }
    if (empty? $ast) { return $ast }

    $el = (eval_ast $ast $env).values
    $f, $params = $el[0], $el[1..$el.Length]
    return &$f @params
}

function PRINT($exp) {
    return pr_str $exp $true
}

$repl_env = @{
    "+" = { param($a, $b); $a + $b };
    "-" = { param($a, $b); $a - $b };
    "*" = { param($a, $b); $a * $b };
    "/" = { param($a, $b); $a / $b }}

function REPL([String] $str) {
    return PRINT (EVAL (READ $str) $repl_env)
}

while ($true) {
    Write-Host "user> " -NoNewline
    $line = [Console]::ReadLine()
    if ($line -eq $null) {
        break
    }
    try {
        Write-Host (REPL($line))
    } catch {
        Write-Host "Exception: $($_.Exception.Message)"
    }
}

$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1
Import-Module $PSScriptRoot/env.psm1

function READ([String] $str) {
    return read_str($str)
}

function eval_ast($ast, $env) {
    switch ($ast.GetType().Name) {
        "Symbol" { return $env.get($ast) }
        "List"   { return new-list($ast.values | ForEach { EVAL $_ $env } ) }
        default  { return $ast }
    }
}

function EVAL($ast, $env) {
    #Write-Host "EVAL $(pr_str $ast $true)"
    if (-not (list? $ast)) {
        return (eval_ast $ast $env)
    }
    if (empty? $ast) { return $ast }

    $a0, $a1, $a2 = $ast.nth(0), $ast.nth(1), $ast.nth(2)
    switch ($a0.value) {
        "def!" {
            return $env.set($a1, (EVAL $a2 $env)) 
        }
        "let*" {
            $let_env = new-env $env
            for ($i=0; $i -lt $a1.values.Length; $i+=2) {
                $_ = $let_env.set($a1.nth($i), (EVAL $a1.nth(($i+1)) $let_env))
            }
            return EVAL $a2 $let_env
        }
        default {
            $el = (eval_ast $ast $env)
            $f, $params = $el.first(), $el.rest()
            return &$f @params
        }
    }
}

function PRINT($exp) {
    return pr_str $exp $true
}

$repl_env = new-env
$_ = $repl_env.set((new-symbol "+"), { param($a, $b); $a + $b })
$_ = $repl_env.set((new-symbol "-"), { param($a, $b); $a - $b })
$_ = $repl_env.set((new-symbol "*"), { param($a, $b); $a * $b })
$_ = $repl_env.set((new-symbol "/"), { param($a, $b); $a / $b })

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

$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1
Import-Module $PSScriptRoot/env.psm1
Import-Module $PSScriptRoot/core.psm1

# READ
function READ([String] $str) {
    return read_str($str)
}

# EVAL
function eval_ast($ast, $env) {
    if ($ast -eq $null) { return $ast }
    switch ($ast.GetType().Name) {
        "Symbol"  { return $env.get($ast) }
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

    $a0, $a1, $a2 = $ast.nth(0), $ast.nth(1), $ast.nth(2)
    switch -casesensitive ($a0.value) {
        "def!" {
            return $env.set($a1, (EVAL $a2 $env)) 
        }
        "let*" {
            $let_env = new-env $env
            for ($i=0; $i -lt $a1.values.Count; $i+=2) {
                $_ = $let_env.set($a1.nth($i), (EVAL $a1.nth(($i+1)) $let_env))
            }
            return EVAL $a2 $let_env
        }
        "do" {
            return (eval_ast $ast.rest() $env).last()
        }
        "if" {
            $cond = (EVAL $a1 $env)
            if ($cond -eq $null -or
                ($cond -is [Boolean] -and $cond -eq $false)) {
                return (EVAL $ast.nth(3) $env)
            } else {
                return (EVAL $a2 $env)
            }
        }
        "fn*" {
            # Save EVAL into a variable that will get closed over
            $feval = Get-Command EVAL
            return {
                return (&$feval $a2 (new-env $env $a1.values $args))
            }.GetNewClosure()
        }
        default {
            $el = (eval_ast $ast $env)
            $f, $fargs = $el.first(), $el.rest().values
            return &$f @fargs
        }
    }
}

# PRINT
function PRINT($exp) {
    return pr_str $exp $true
}

# REPL
$repl_env = new-env

function REP([String] $str) {
    return PRINT (EVAL (READ $str) $repl_env)
}

# core.EXT: defined using PowerShell
foreach ($kv in $core_ns.GetEnumerator()) {
    $_ = $repl_env.set((new-symbol $kv.Key), $kv.Value)
}

# core.mal: defined using the language itself
$_ = REP('(def! not (fn* (a) (if a false true)))')

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

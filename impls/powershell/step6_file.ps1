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
  while ($true) {
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
            $env = $let_env
            $ast = $a2  # TCO
        }
        "do" {
            if ($ast.values.Count -gt 2) {
                $middle = new-list $ast.values[1..($ast.values.Count-2)]
                $_ = eval_ast $middle $env
            }
            $ast = $ast.last()  # TCO
        }
        "if" {
            $cond = (EVAL $a1 $env)
            if ($cond -eq $null -or
                ($cond -is [Boolean] -and $cond -eq $false)) {
                $ast = $ast.nth(3)  # TCO
            } else {
                $ast = $a2  # TCO
            }
        }
        "fn*" {
            # Save EVAL into a variable that will get closed over
            $feval = Get-Command EVAL
            $fn = {
                return (&$feval $a2 (new-env $env $a1.values $args))
            }.GetNewClosure()
            return new-malfunc $a2 $a1.values $env $fn
        }
        default {
            $el = (eval_ast $ast $env)
            $f, $fargs = $el.first(), $el.rest().values
            if (malfunc? $f) {
                $env = (new-env $f.env $f.params $fargs)
                $ast = $f.ast  # TCO
            } else {
                return &$f @fargs
            }
        }
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
$_ = $repl_env.set((new-symbol "eval"), { param($a); (EVAL $a $repl_env) })
$_ = $repl_env.set((new-symbol "*ARGV*"), (new-list $args[1..$args.Count]))

# core.mal: defined using the language itself
$_ = REP('(def! not (fn* (a) (if a false true)))')
$_ = REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))')


if ($args.Count -gt 0) {
    $_ = REP('(load-file "' + $args[0] + '")')
    exit 0
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

$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/types.psm1
Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1
Import-Module $PSScriptRoot/env.psm1

# READ
function READ([String] $str) {
    return read_str($str)
}

# EVAL
function EVAL($ast, $env) {

    $dbgeval_env = ($env.find("DEBUG-EVAL"))
    if ($dbgeval_env -ne $null) {
        $dbgeval = $dbgeval_env.get("DEBUG-EVAL")
        if ($dbgeval -ne $null -and
            -not ($dbgeval -is [Boolean] -and $dbgeval -eq $false)) {
            Write-Host "EVAL: $(pr_str $ast)"
        }
    }

    if ($ast -eq $null) { return $ast }
    switch ($ast.GetType().Name) {
        "Symbol"  { return $env.get($ast.value) }
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

    $a0, $a1, $a2 = $ast.nth(0), $ast.nth(1), $ast.nth(2)
    switch -casesensitive ($a0.value) {
        "def!" {
            return $env.set($a1.value, (EVAL $a2 $env))
        }
        "let*" {
            $let_env = new-env $env
            for ($i=0; $i -lt $a1.values.Count; $i+=2) {
                $_ = $let_env.set($a1.nth($i).value, (EVAL $a1.nth(($i+1)) $let_env))
            }
            return EVAL $a2 $let_env
        }
        default {
            $f = ( EVAL $ast.first() $env )
            $fargs = @($ast.rest().values | ForEach-Object { EVAL $_ $env })
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
$_ = $repl_env.set("+", { param($a, $b); $a + $b })
$_ = $repl_env.set("-", { param($a, $b); $a - $b })
$_ = $repl_env.set("*", { param($a, $b); $a * $b })
$_ = $repl_env.set("/", { param($a, $b); $a / $b })

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

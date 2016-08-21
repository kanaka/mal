$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1

# READ
function READ([String] $str) {
    return read_str($str)
}

# EVAL
function EVAL($ast, $env) {
    return $ast
}

# PRINT
function PRINT($exp) {
    return pr_str $exp $true
}

# REPL
function REP([String] $str) {
    return PRINT (EVAL (READ $str) @{})
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

$ErrorActionPreference = "Stop"

Import-Module $PSScriptRoot/reader.psm1
Import-Module $PSScriptRoot/printer.psm1

function READ([String] $str) {
    return read_str($str)
}

function EVAL($ast, $env) {
    return $ast
}

function PRINT($exp) {
    return pr_str $exp $true
}

function REPL([String] $str) {
    return PRINT (EVAL (READ $str) @{})
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

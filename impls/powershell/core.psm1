function time_ms {
    $ms = [double]::Parse((Get-Date (get-date).ToUniversalTime() -UFormat %s))
    [int64] ($ms * 1000)
}

function get($hm, $key) {
    if ($hm -eq $null) {
        $null
    } else {
        $hm.values.Item($key)
    }
}

function concat {
    $res = @()
    foreach($a in $args) {
        $res = $res + $a.values
    }
    new-list $res
}

function vec($seq) {
    if(vector? $seq) {
        return $seq
    } else {
        return new-vector($seq.values)
    }
}

function nth($lst, $idx) {
    if ($idx -ge $lst.values.Count) {
        throw "nth: index out of range"
    }
    $lst.nth($idx)
}


function do_map($f, $l) {
    if (malfunc?($f)) {
        $f = $f.fn
    }
    new-list ($l.values | foreach { &$f $_ })
}

function do_apply($f) {
    if (malfunc?($f)) {
        $f = $f.fn
    }
    if ($args.Count -gt 1) {
        $fargs = $args[0..($args.Count-2)] + $args[-1].values
    } else {
        $fargs = $args[$args.Count-1].values
    }
    &$f @fargs
}

function conj($lst) {
    if (list? $lst) {
        [Array]::Reverse($args)
        return new-list ($args + $lst.values)
    } else {
        return new-vector ($lst.values + $args)
    }
}

function seq($obj) {
    if ($obj -eq $null) {
        return $null
    } elseif (list? $obj) {
        if ($obj.values.Count -gt 0) {
            return $obj
        } else {
            return $null
        }
    } elseif (vector? $obj) {
        if ($obj.values.Count -gt 0) {
            return new-list $obj.values
        } else {
            return $null
        }
    } elseif (string? $obj) {
        if ($obj.Length -gt 0) {
            return new-list ($obj -split '')[1..$obj.Length]
        } else {
            return $null
        }
        return new-list $obj
    } else {
        throw "seq: called on non-sequence"
    }
}

function swap_BANG($a, $f) {
    if (malfunc?($f)) {
        $f = $f.fn
    }
    $fargs = @($a.value) + $args
    if ($fargs.Count -eq 0) {
        $a.value = &$f
    } else {
        $a.value = &$f @fargs
    }
    $a.value
}


$core_ns = @{
    "="           = { param($a, $b); equal? $a $b };
    "throw"       = Get-Command mal_throw;

    "nil?"        = { param($a); $a -eq $null };
    "true?"       = { param($a); $a -eq $true };
    "false?"      = { param($a); $a -eq $false };
    "number?"     = { param($a); $a -is [int32] };
    "string?"     = { param($a); string? $a };
    "symbol"      = Get-Command new-symbol;
    "symbol?"     = { param($a); symbol? $a };
    "keyword"     = Get-Command new-keyword;
    "keyword?"    = { param($a); keyword? $a };
    "fn?"         = { param($a); (fn? $a) -or ((malfunc? $a) -and
                                               (-not $a.macro)) };
    "macro?"      = { param($a); (malfunc? $a) -and $a.macro };

    "pr-str"      = { pr_seq $args $true  " " };
    "str"         = { pr_seq $args $false "" };
    "prn"         = { Write-Host (pr_seq $args $true  " "); $null };
    "println"     = { Write-Host (pr_seq $args $false " "); $null };
    "read-string" = { read_str $args[0] };
    "readline"    = { Write-Host $args[0] -NoNewline; [Console]::Readline() };
    "slurp"       = { Get-Content -Path $args[0] -Raw };

    "<"       = { param($a, $b); $a -lt $b };
    "<="      = { param($a, $b); $a -le $b };
    ">"       = { param($a, $b); $a -gt $b };
    ">="      = { param($a, $b); $a -ge $b };
    "+"       = { param($a, $b); $a + $b };
    "-"       = { param($a, $b); $a - $b };
    "*"       = { param($a, $b); $a * $b };
    "/"       = { param($a, $b); $a / $b };
    "time-ms" = Get-Command time_ms;

    "list"    = { new-list $args };
    "list?"   = Get-Command list?;
    "vector"  = { new-vector $args };
    "vector?" = Get-Command vector?;
    "hash-map" = { new-hashmap $args };
    "map?"    = Get-Command hashmap?;
    "assoc"   = { param($a); assoc_BANG $a.copy() $args };
    "dissoc"  = { param($a); dissoc_BANG $a.copy() $args };
    "get"     = { param($a,$b); get $a $b };
    "contains?" = { param($a,$b); $a.values.Contains($b) };
    "keys"    = Get-Command keys;
    "vals"    = Get-Command vals;

    "sequential?" = Get-Command sequential?;
    "cons"    = { param($a, $b); new-list (@($a) + $b.values) };
    "concat"  = Get-Command concat;
    "vec"     = Get-Command vec;
    "nth"     = Get-Command nth;
    "first"   = { param($a); if ($a -eq $null) { $null } else { $a.first() } };
    "rest"    = { param($a); if ($a -eq $null) { new-list @() } else { $a.rest() } };
    "empty?"  = { param($a); $a -eq $null -or $a.values.Count -eq 0 };
    "count"   = { param($a); $a.values.Count };
    "apply"   = Get-Command do_apply;
    "map"     = Get-Command do_map;

    "conj"    = Get-Command conj;
    "seq"     = Get-Command seq;

    "meta"    = { param($a); $a.meta };
    "with-meta" = { param($a, $b); $c = $a.copy(); $c.meta = $b; $c };
    "atom"    = { param($a); new-atom($a) };
    "atom?"   = { param($a); atom?($a) };
    "deref"   = { param($a); $a.value };
    "reset!"  = { param($a, $b); $a.value = $b; $b };
    "swap!"   = Get-Command swap_BANG;
}

Export-ModuleMember -Variable core_ns


function pr_str {
    param($obj, $print_readably = $true)
    if ($obj -eq $null) {
        return "nil"
    }

    switch ($obj.GetType().Name) {
        "String" {
            if ($obj[0] -eq "$([char]0x29e)") {
                return ":$($obj.substring(1))"
            } elseif ($print_readably) {
                $s = $obj -replace "\\", "\\"
                $s = $s -replace "`"", "\`""
                $s = $s -replace "`n", "\n"
                return "`"$s`""
            } else {
                return "$obj"
            }
        }
        "Vector" {
            $res = $obj.values | ForEach { (pr_str $_ $print_readably) }
            return "[" + ($res -join " ") + "]"
        }
        "List" {
            $res = $obj.values | ForEach { (pr_str $_ $print_readably) }
            return "(" + ($res -join " ") + ")"
        }
        "HashMap" {
            $res = @()
            foreach ($k in $obj.values.Keys) {
                $res += pr_str $k $print_readably
                $res += pr_str $obj.values[$k] $print_readably
            }
            return "{" + ($res -join " ") + "}"
        }
        "Symbol" {
            return $obj.value
        }
        "Boolean" {
            return $obj.ToString().ToLower()
        }
        "Atom" {
            return "(atom $(pr_str $obj.value $print_readably))"
        }
        "PSCustomObject" {
            return "(fn* $(pr_str (new-list $obj.params) $print_readably) $(pr_str $obj.ast $print_readably))"
        }
        default {
            return $obj.ToString()
        }
    }
}

function pr_seq {
    param($seq, $print_readably, $sep)
    $lst = foreach($a in $seq) { pr_str $a $print_readably } 
    $lst -join $sep
}


function pr_str($obj, $print_readably) {
    if ($obj -eq $null) {
        return "nil"
    }
    #Write-Host ("type:" + $obj.GetType().Name)
    switch ($obj.GetType().Name) {
        "String" {
            return "`"$obj`""
        }
        "List" {
            $res = $obj.values | ForEach { (pr_str $_ $print_readably) }
            return "(" + ($res -join " ") + ")"
        }
        "Symbol" {
            return $obj.value
        }
        default {
            return $obj.ToString()
        }
    }
}

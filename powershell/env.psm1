Import-Module $PSScriptRoot/types.psm1

Class Env {
    [HashTable] $data
    [Env]       $outer

    Env([Env] $out, $binds, $exprs) {
        # Case-sensitive hash table
        $this.data  = New-Object System.Collections.HashTable
        $this.outer = $out

        if ($binds -ne $null) {
            for ($i = 0; $i -lt $binds.Length; $i++) {
                if ($binds[$i].value -eq "&") {
                    if ($exprs.Length -gt $i) {
                        $rest = $exprs[$i..($exprs.Length-1)]
                    } else {
                        $rest = @()
                    }
                    $this.data[$binds[($i+1)].value] = new-list($rest)
                    break
                } else {
                    $this.data[$binds[$i].value] = $exprs[$i]
                }
            }
        }
    }

    [Object] set($key, $value) {
        $this.data[$key.value] = $value
        return $value
    }

    [Env] find($key) {
        if ($this.data.Contains($key.value)) {
            return $this
        } elseif ($this.outer -ne $null) {
            return $this.outer.find($key)
        } else {
            return $null
        }
    }

    [Object] get($key) {
        $e = $this.find($key)
        if ($e -ne $null) {
            return $e.data[$key.value]
        } else {
            throw "'$($key.value)' not found"
        }
    }
}

function new-env([Env] $out, $binds, $exprs) {
    [Env]::new($out, $binds, $exprs)
}


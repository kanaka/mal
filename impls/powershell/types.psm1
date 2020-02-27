#
# Exceptions
#
Class MalException : Exception {
    [Object] $object

    MalException($obj) {
        $this.object = $obj
    }
}

function mal_throw($obj) {
    throw [MalException] $obj
}

#
# Symbols
#

Class Symbol {
    [String] $value

    Symbol([String] $val) {
        $this.value = $val
    }

    copy() { $this }
}

function new-symbol([String] $val) {
    [Symbol]::new($val)
}

function symbol?($obj) {
    $obj -is [Symbol]
}

#
# Strings
#

function string?($obj) {
    ($obj -is [String]) -and ($obj[0] -ne "$([char]0x29e)")
}

#
# Keywords
#

function new-keyword($obj) {
    if (keyword? $obj) {
        $obj
    } else {
        "$([char]0x29e)$obj"
    }
}

function keyword?($obj) {
    ($obj -is [String]) -and ($obj[0] -eq "$([char]0x29e)")
}


#
# Lists
#

Class List {
    #[System.Collections.ArrayList] $values
    [Object[]] $values
    [Object]   $meta

    List() {
        $this.values = @()
        #$this.values = New-Object System.Collections.ArrayList
    }

    List([Object[]] $vals) {
    #List([System.Collections.ArrayList] $vals) {
        $this.values = $vals
    }

    [List] copy() {
        return [List]::new($this.values)
    }

    [void] push([Object] $val) {
        $this.values.Add($val)
    }

    [Object] first() {
        return $this.values[0]
    }

    [List] rest() {
        if ($this.values.Count -le 1) {
            return [List]::new(@())
        } else {
            return [List]::new($this.values[1..($this.values.Count)])
        }
    }

    [Object] last() {
        if ($this.values.Count -eq 0) {
            return $null
        } else {
            return $this.values[$this.values.Count-1]
        }
    }

    [Object] nth([int64] $idx) {
        return $this.values[$idx]
    }
}

function new-list([Object[]] $vals) {
#function new-list([System.Collections.ArrayList] $vals) {
    if ($vals.Count -eq 0) {
        return [List]::new()
    } else {
        return [List]::new($vals)
    }
}

function list?($obj) {
    $obj -is [List] -and -not ($obj -is [Vector])
}


#
# Vectors
#

Class Vector : List {
    Vector() {
        $this.values = @()
        #$this.values = New-Object System.Collections.ArrayList
    }

    Vector([Object[]] $vals) {
    #Vector([System.Collections.ArrayList] $vals) {
        $this.values = $vals
    }

    [Vector] copy() {
        return [Vector]::new($this.values)
    }

}

function new-vector([Object[]] $vals) {
    if ($vals.Count -eq 0) {
        return [Vector]::new()
    } else {
        return [Vector]::new($vals)
    }
}

function vector?($obj) {
    $obj -is [Vector]
}


#
# HashMaps
#

Class HashMap {
    [Hashtable] $values
    [Object]   $meta

    HashMap() {
        # Case-sensitive hashtable
        $this.values = New-Object System.Collections.HashTable
    }

    HashMap([Hashtable] $vals) {
        $this.values = $vals
    }

    [HashMap] copy() {
        return [HashMap]::new($this.values.clone())
    }

}

function assoc_BANG($hm, $kvs) {
    $ht = $hm.values
    for ($i = 0; $i -lt $kvs.Count; $i+=2) {
        $ht[$kvs[$i]] = $kvs[($i+1)]
    }
    return $hm
}

function dissoc_BANG($hm, $ks) {
    $ht = $hm.values
    foreach ($k in $ks) {
        $ht.Remove($k)
    }
    return $hm
}


function new-hashmap([Object[]] $vals) {
    $hm = [HashMap]::new()
    assoc_BANG $hm $vals
}

function hashmap?($obj) {
    $obj -is [HashMap]
}

function keys($hm) {
    return new-list ($hm.values.GetEnumerator() | ForEach { $_.Key })
}

function vals($hm) {
    return new-list ($hm.values.GetEnumerator() | ForEach { $_.Value })
}


#
# Atoms

Class Atom {
    [Object] $value

    Atom([Object] $val) {
        $this.value = $val
    }
}

function new-atom([Object] $val) {
    [Atom]::new($val)
}

function atom?($obj) {
    $obj -is [Atom]
}


# Functions

Class MalFunc {
    [Object]      $ast
    [Object[]]    $params
    [Object]      $env
    [scriptBlock] $fn
    [Boolean]     $macro
    [Object]      $meta

    MalFunc($ast, $params, $env, $fn, $macro, $meta){
        $this.ast    = $ast
        $this.params = $params
        $this.env    = $env
        $this.fn     = $fn
        $this.macro  = $macro
        $this.meta   = $meta
    }

    [MalFunc] copy() {
        return [MalFunc]::new($this.ast, $this.params, $this.env, $this.fn,
                              $this.macro, $this.meta)
    }

}

function new-malfunc($ast, $params, $env, $fn, $macro, $meta) {
    [MalFunc]::new($ast, $params, $env, $fn, $macro, $meta)
}

function malfunc?($obj) {
    $obj -is [MalFunc]
}

function fn?($obj) {
    $obj -is [System.Management.Automation.ScriptBlock]
}
#
# General functions
#
function equal?($a, $b) {
    if ($a -eq $null -and $b -eq $null) {
        return $true
    } elseif ($a -eq $null -or $b -eq $null) {
        return $false
    }
    $ta, $tb = $a.GetType().Name, $b.GetType().Name
    if (-not (($ta -eq $tb) -or ((sequential?($a)) -and (sequential?($b))))) {
        return $false
    }
    switch ($ta) {
        { $_ -eq "List" -or $_ -eq "Vector" } {
            if ($a.values.Count -ne $b.values.Count) {
                return $false
            }
            for ($i = 0; $i -lt $a.value.Count; $i++) {
                if (-not (equal? $a.values[$i] $b.values[$i])) {
                    return $false
                }
            }
            return $true
        }
        "HashMap" {
            $hta, $htb = $a.values, $b.values
            $alen = ($hta.GetEnumerator | Measure-Object).Count
            $blen = ($htb.GetEnumerator | Measure-Object).Count
            if ($alen -ne $blen) {
                return $false
            }
            foreach ($kv in $hta.GetEnumerator()) {
                if (-not (equal? $kv.Value $htb[$kv.Key])) {
                    return $false
                }
            }
            return $true
        }
        "Symbol" {
            return $a.value -ceq $b.value
        }
        default {
            return $a -ceq $b
        }
    }
}


#
# Sequence functions
#
function sequential?($obj) {
    $obj -is [List]
}

function empty?($obj) {
    $obj.values.Count -eq 0
}



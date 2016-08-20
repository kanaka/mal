Import-Module $PSScriptRoot/types.psm1

Class Env {
    [HashTable] $data
    [Env]       $outer

    Env() {
        # Case-sensitive hash table
        $this.data  = New-Object System.Collections.HashTable
        $this.outer = $null
    }

    Env([Env] $out) {
        # Case-sensitive hash table
        $this.data  = New-Object System.Collections.HashTable
        $this.outer = $out
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

function new-env {
    [Env]::new()
}

function new-env([Env] $out) {
    [Env]::new($out)
}


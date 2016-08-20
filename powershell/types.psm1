Class Symbol {
    [String] $value

    Symbol([String] $val) {
        $this.value = $val
    }
}

function new-symbol([string] $val) {
    [Symbol]::new($val)
}

Class List {
    [Object[]] $values

    List([Object[]] $vals) {
        $this.values = $vals
    }

    [void] push([Object] $val) {
        $this.values += $val
    }

    [Object] first() {
        return $this.values[0]
    }

    [Object[]] rest() {
        return $this.values[1..($this.values.Length-1)]
    }

    [Object] nth([int] $idx) {
        return $this.values[$idx]
    }
}

function new-list([Object[]] $vals) {
    [List]::new($vals)
}

function list?($obj) {
    $obj -is [List]
}

function empty?($obj) {
    $obj.values.Count -eq 0
}

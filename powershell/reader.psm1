Import-Module $PSScriptRoot/types.psm1

Class Reader {
    [String[]] $tokens
    [int]      $pos

    Reader([String[]] $toks) {
        $this.tokens = $toks
        $this.pos = 0
    }

    [String] peek() {
        return $this.tokens[$this.pos]
    }

    [String] next() {
        return $this.tokens[$this.pos++]
    }
}


function tokenize {
    $r = [regex]"[\s,]*(~@|[\[\]{}()'``~^@]|`"(?:\\.|[^\\`"])*`"|;.*|[^\s\[\]{}('`"``,;)]*)"
    $r.Matches($args) | 
        Where-Object { $_.Groups.Item(1).Value.Length -gt 0 } |
        Foreach-Object { $_.Groups.Item(1).Value }
}

function read_atom([Reader] $rdr) {
    $token = $rdr.next()
    if ($token -match "^-?[0-9]+$") {
        return [convert]::ToInt32($token, 10)
    } elseif ($token -match "^`".*`"") {
        return $token.Substring(1,$token.Length-2)
    } else {
        return new-symbol($token)
    }
}

function read_list([Reader] $rdr) {
    $ast = new-list(@())
    $token = $rdr.next()
    if ($token -ne '(') {
        throw "expected '('"
    }
    while (($token = $rdr.peek()) -ne ')') {
        if ($token -eq "") {
            throw "expected ')', got EOF"
        }
        $form = read_form $rdr
        $ast.push($form)
    }
    $token = $rdr.next()
    return $ast
}

function read_form([Reader] $rdr) {
    $token = $rdr.peek()
    switch ($token) {
        ")"     { throw "unexpected ')'" }
        "("     { return read_list($rdr) }
        default { return read_atom($rdr) }
    }
}

function read_str {
    $toks = tokenize($args[0])
    if ($toks.Length -eq 0) { return }
    read_form([Reader]::new($toks))
}

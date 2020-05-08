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
    $r = [regex]"[\s,]*(~@|[\[\]{}()'``~^@]|`"(?:\\.|[^\\`"])*`"?|;.*|[^\s\[\]{}('`"``,;)]*)"
    $r.Matches($args) | 
        Where-Object { $_.Groups.Item(1).Value.Length -gt 0 -and
                       $_.Groups.Item(1).Value[0] -ne ";" } |
        Foreach-Object { $_.Groups.Item(1).Value }
}

function read_atom([Reader] $rdr) {
    $token = $rdr.next()
    if ($token -match "^-?[0-9]+$") {
        return [convert]::ToInt32($token, 10)
    } elseif ($token -match "^`"(?:\\.|[^\\`"])*`"$") {
        $s = $token.Substring(1,$token.Length-2)
        $s = $s -replace "\\\\", "$([char]0x29e)"
        $s = $s -replace "\\`"", "`""
        $s = $s -replace "\\n", "`n"
        $s = $s -replace "$([char]0x29e)", "\"
        return $s
    } elseif ($token -match "^`".*") {
        throw "expected '`"', got EOF"
    } elseif ($token -match ":.*") {
        return "$([char]0x29e)$($token.substring(1))"
    } elseif ($token -eq "true") {
        return $true
    } elseif ($token -eq "false") {
        return $false
    } elseif ($token -eq "nil") {
        return $null
    } else {
        return new-symbol($token)
    }
}

function read_seq([Reader] $rdr, $start, $end) {
    $seq = @()
    $token = $rdr.next()
    if ($token -ne $start) {
        throw "expected '$start'"
    }
    while (($token = $rdr.peek()) -ne $end) {
        if ($token -eq "") {
            throw "expected '$end', got EOF"
        }
        $form = read_form $rdr
        $seq += $form
    }
    $token = $rdr.next()
    return ,$seq
}

function read_list([Reader] $rdr) {
    return new-list (read_seq $rdr "(" ")")
}

function read_vector([Reader] $rdr) {
    return new-vector (read_seq $rdr "[" "]")
}

function read_hash_map([Reader] $rdr) {
    return new-hashmap (read_seq $rdr "{" "}")
}

function read_form([Reader] $rdr) {
    $token = $rdr.peek()
    switch ($token) {
        # reader macros/transforms
        "'"     { $_ = $rdr.next();
                  return new-list @((new-symbol "quote"),
                                    (read_form $rdr)) }
        "``"    { $_ = $rdr.next();
                  return new-list @((new-symbol "quasiquote"),
                                    (read_form $rdr)) }
        "~"     { $_ = $rdr.next();
                  return (new-list @((new-symbol "unquote"),
                                     (read_form $rdr))) }
        "~@"    { $_ = $rdr.next();
                  return (new-list @((new-symbol "splice-unquote"),
                                     (read_form $rdr))) }
        "^"     { $_ = $rdr.next();
                  $meta = read_form $rdr
                  return (new-list @((new-symbol "with-meta"),
                                     (read_form $rdr),
                                     $meta)) }
        "@"     { $_ = $rdr.next();
                  return (new-list @((new-symbol "deref"),
                                     (read_form $rdr))) }

        # list
        ")"     { throw "unexpected ')'" }
        "("     { return read_list $rdr }

        # vector
        "]"     { throw "unexpected ']'" }
        "["     { return read_vector $rdr }

        # hashmap
        "}"     { throw "unexpected '}'" }
        "{"     { return read_hash_map $rdr }

        default { return read_atom $rdr  }
    }
}

function read_str {
    $toks = tokenize($args[0])
    if ($toks.Length -eq 0) { return $null }
    read_form([Reader]::new($toks))
}

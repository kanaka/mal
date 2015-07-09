oo::class create Reader {
    variable pos tokens

    constructor {tokens_list} {
        set tokens $tokens_list
        set pos 0
    }

    method peek {} {
        lindex $tokens $pos 
    }

    method next {} {
        set token [my peek]
        incr pos
        return $token
    }
}

proc tokenize str {
    set re {[\s,]*(~@|[\[\]\{\}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]\{\}('\"`~^@,;)]*)}
    set tokens {}
    foreach {_ capture} [regexp -line -all -inline $re $str] {
        if {[string length $capture] > 0 && [string range $capture 0 0] != ";"} {
            lappend tokens $capture
        }
    }
    return $tokens
}

proc read_tokens_list {reader start_char end_char} {
    set token [$reader next]
    if {$token != $start_char} {
        error "expected '$start_char'"
    }

    set elements {}
    set token [$reader peek]
    while {$token != $end_char} {
        if {$token == ""} {
            error "expected '$end_char'"
        }
        lappend elements [read_form $reader]
        set token [$reader peek]
    }
    $reader next
    return $elements
}

proc read_list {reader} {
    set elements [read_tokens_list $reader "(" ")"]
    list_new $elements
}

proc read_vector {reader} {
    set elements [read_tokens_list $reader "\[" "\]"]
    vector_new $elements
}

proc read_hashmap {reader} {
    set res [dict create]
    foreach {keytoken valtoken} [read_tokens_list $reader "{" "}"] {
        dict set res [obj_val $keytoken] $valtoken
    }
    hashmap_new $res
}

proc parse_string {str} {
    set res [string range $str 1 end-1]
    string map {"\\n" "\n" "\\\"" "\"" "\\\\" "\\"} $res
}

proc parse_keyword {str} {
    # Remove initial ":"
    string range $str 1 end
}

proc read_atom {reader} {
    set token [$reader next]
    switch -regexp $token {
        ^-?[0-9]+$ { return [obj_new "integer" $token] }
        ^nil$      { return $::mal_nil }
        ^true$     { return $::mal_true }
        ^false$    { return $::mal_false }
        ^:         { return [keyword_new [parse_keyword $token]] }
        ^\".*\"$   { return [string_new [parse_string $token]] }
        default    { return [symbol_new $token] }
    }
}

proc symbol_shortcut {symbol_name reader} {
    $reader next
    list_new [list [symbol_new $symbol_name] [read_form $reader]]
}

proc read_form {reader} {
    switch [$reader peek] {
        "'"     { return [symbol_shortcut "quote" $reader] }
        "`"     { return [symbol_shortcut "quasiquote" $reader] }
        "~"     { return [symbol_shortcut "unquote" $reader] }
        "~@"    { return [symbol_shortcut "splice-unquote" $reader] }
        "^"     {
            $reader next
            set meta [read_form $reader]
            return [list_new [list [symbol_new "with-meta"] [read_form $reader] $meta]]
        }
        "@"     { return [symbol_shortcut "deref" $reader] }
        "("     { return [read_list $reader] }
        ")"     { error "unexpected ')'" }
        "\["    { return [read_vector $reader] }
        "\]"    { error "unexpected '\]'" }
        "\{"    { return [read_hashmap $reader] }
        "\}"    { error "unexpected '\}'" }
        default { return [read_atom $reader] }
    }
}

proc read_str str {
    set tokens [tokenize $str]
    set reader [Reader new $tokens]
    set res [read_form $reader]
    $reader destroy
    return $res
}

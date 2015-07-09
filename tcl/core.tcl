proc mal_equal {a} {
    bool_new [equal_q [lindex $a 0] [lindex $a 1]]
}

set ::mal_exception_obj 0
proc mal_throw {a} {
    set ::mal_exception_obj [lindex $a 0]
    error "__MalException__"
}

proc mal_nil_q {a} {
    bool_new [nil_q [lindex $a 0]]
}

proc mal_true_q {a} {
    bool_new [true_q [lindex $a 0]]
}

proc mal_false_q {a} {
    bool_new [false_q [lindex $a 0]]
}

proc mal_symbol {a} {
    symbol_new [obj_val [lindex $a 0]]
}

proc mal_symbol_q {a} {
    bool_new [symbol_q [lindex $a 0]]
}

proc mal_keyword {a} {
    keyword_new [obj_val [lindex $a 0]]
}

proc mal_keyword_q {a} {
    bool_new [keyword_q [lindex $a 0]]
}

proc render_array {arr readable delim} {
    set res {}
    foreach e $arr {
        lappend res [pr_str $e $readable]
    }
    join $res $delim
}

proc mal_pr_str {a} {
    string_new [render_array $a 1 " "]
}

proc mal_str {a} {
    string_new [render_array $a 0 ""]
}

proc mal_prn {a} {
    puts [render_array $a 1 " "]
    return $::mal_nil
}

proc mal_println {a} {
    puts [render_array $a 0 " "]
    return $::mal_nil
}

proc mal_read_string {a} {
    read_str [obj_val [lindex $a 0]]
}

proc mal_readline {a} {
    set prompt [obj_val [lindex $a 0]]
    set res [_readline $prompt]
    if {[lindex $res 0] == "EOF"} {
        return $::mal_nil
    }
    string_new [lindex $res 1]
}

proc mal_slurp {a} {
    set filename [obj_val [lindex $a 0]]
    set file [open $filename]
    set content [read $file]
    close $file
    string_new $content
}

proc mal_lt {a} {
    bool_new [expr {[obj_val [lindex $a 0]] < [obj_val [lindex $a 1]]}]
}

proc mal_lte {a} {
    bool_new [expr {[obj_val [lindex $a 0]] <= [obj_val [lindex $a 1]]}]
}

proc mal_gt {a} {
    bool_new [expr {[obj_val [lindex $a 0]] > [obj_val [lindex $a 1]]}]
}

proc mal_gte {a} {
    bool_new [expr {[obj_val [lindex $a 0]] >= [obj_val [lindex $a 1]]}]
}

proc mal_add {a} {
    integer_new [expr {[obj_val [lindex $a 0]] + [obj_val [lindex $a 1]]}]
}

proc mal_sub {a} {
    integer_new [expr {[obj_val [lindex $a 0]] - [obj_val [lindex $a 1]]}]
}

proc mal_mul {a} {
    integer_new [expr {[obj_val [lindex $a 0]] * [obj_val [lindex $a 1]]}]
}

proc mal_div {a} {
    integer_new [expr {[obj_val [lindex $a 0]] / [obj_val [lindex $a 1]]}]
}

proc mal_time_ms {a} {
    integer_new [clock milliseconds]
}

proc mal_list {a} {
    list_new $a
}

proc mal_list_q {a} {
    bool_new [list_q [lindex $a 0]]
}

proc mal_vector {a} {
    vector_new $a
}

proc mal_vector_q {a} {
    bool_new [vector_q [lindex $a 0]]
}

proc mal_hash_map {a} {
    set d [dict create]
    foreach {k v} $a {
        dict set d [obj_val $k] $v
    }
    hashmap_new $d
}

proc mal_map_q {a} {
    bool_new [hashmap_q [lindex $a 0]]
}

proc mal_assoc {a} {
    set d [dict create]
    dict for {k v} [obj_val [lindex $a 0]] {
        dict set d $k $v
    }
    foreach {k v} [lrange $a 1 end] {
        dict set d [obj_val $k] $v
    }
    hashmap_new $d
}

proc mal_dissoc {a} {
    set d [dict create]
    dict for {k v} [obj_val [lindex $a 0]] {
        dict set d $k $v
    }
    foreach k [lrange $a 1 end] {
        dict unset d [obj_val $k]
    }
    hashmap_new $d
}

proc mal_get {a} {
    lassign $a hashmap_obj key_obj
    if {[dict exists [obj_val $hashmap_obj] [obj_val $key_obj]]} {
        dict get [obj_val $hashmap_obj] [obj_val $key_obj]
    } else {
        return $::mal_nil
    }
}

proc mal_contains_q {a} {
    lassign $a hashmap_obj key_obj
    bool_new [dict exists [obj_val $hashmap_obj] [obj_val $key_obj]]
}

proc mal_keys {a} {
    set res {}
    foreach k [dict keys [obj_val [lindex $a 0]]] {
        lappend res [string_new $k]
    }
    list_new $res
}

proc mal_vals {a} {
    list_new [dict values [obj_val [lindex $a 0]]]
}

proc mal_sequential_q {a} {
    bool_new [sequential_q [lindex $a 0]]
}

proc mal_cons {a} {
    lassign $a head lst
    list_new [concat [list $head] [obj_val $lst]]
}

proc mal_concat {a} {
    set res {}
    foreach lst $a {
        if {[nil_q $lst]} {
            continue
        }
        set res [concat $res [obj_val $lst]]
    }
    list_new $res
}

proc mal_nth {a} {
    lassign $a lst_obj index_obj
    set index [obj_val $index_obj]
    set lst [obj_val $lst_obj]
    if {$index >= [llength $lst]} {
        error "nth: index out of range"
    }
    lindex $lst $index
}

proc mal_first {a} {
    lassign $a lst
    if {[nil_q $lst] || [llength [obj_val $lst]] == 0} {
        return $::mal_nil
    }
    lindex [obj_val $lst] 0
}

proc mal_rest {a} {
    lassign $a lst
    list_new [lrange [obj_val $lst] 1 end]
}

proc mal_empty_q {a} {
    bool_new [expr {[llength [obj_val [lindex $a 0]]] == 0}]
}

proc mal_count {a} {
    integer_new [llength [obj_val [lindex $a 0]]]
}

proc mal_apply {a} {
    set f [lindex $a 0]
    if {[llength $a] > 1} {
        set mid_args [lrange $a 1 end-1]
        set last_list [lindex $a end]
        set apply_args [concat $mid_args [obj_val $last_list]]
    } else {
        set apply_args {}
    }

    switch [obj_type $f] {
        function {
            set funcdict [obj_val $f]
            set body [dict get $funcdict body]
            set env [dict get $funcdict env]
            set binds [dict get $funcdict binds]
            set funcenv [Env new $env $binds $apply_args]
            return [EVAL $body $funcenv]
        }
        nativefunction {
            set body [concat [list [obj_val $f]] {$a}]
            set lambda [list {a} $body]
            return [apply $lambda $apply_args]
        }
        default {
            error "Not a function"
        }
    }
}

proc mal_map {a} {
    lassign $a f seq
    set res {}
    foreach item [obj_val $seq] {
        set mappeditem [mal_apply [list $f [list_new [list $item]]]]
        lappend res $mappeditem
    }
    list_new $res
}

proc mal_conj {a} {
    lassign $a a0
    if {[list_q $a0]} {
        set lst $a0
        foreach item [lrange $a 1 end] {
            set lst [mal_cons [list $item $lst]]
        }
        return $lst
    } elseif {[vector_q $a0]} {
        set res [obj_val $a0]
        foreach item [lrange $a 1 end] {
            lappend res $item
        }
        vector_new $res
    } else {
        error "conj requires list or vector"
    }
}

proc mal_meta {a} {
    obj_meta [lindex $a 0]
}

proc mal_with_meta {a} {
    lassign $a a0 a1
    obj_new [obj_type $a0] [obj_val $a0] $a1
}

proc mal_atom {a} {
    atom_new [lindex $a 0]
}

proc mal_atom_q {a} {
    bool_new [atom_q [lindex $a 0]]
}

proc mal_deref {a} {
    obj_val [lindex $a 0]
}

proc mal_reset_bang {a} {
    lassign $a a0 a1
    obj_set_val $a0 $a1
}

proc mal_swap_bang {a} {
    lassign $a a0 f
    set apply_args [concat [list [obj_val $a0]] [lrange $a 2 end]]
    set newval [mal_apply [list $f [list_new $apply_args]]]
    mal_reset_bang [list $a0 $newval]
}

set core_ns [dict create \
    "="            [nativefunction_new mal_equal] \
    "throw"        [nativefunction_new mal_throw] \
    \
    "nil?"         [nativefunction_new mal_nil_q] \
    "true?"        [nativefunction_new mal_true_q] \
    "false?"       [nativefunction_new mal_false_q] \
    "symbol"       [nativefunction_new mal_symbol] \
    "symbol?"      [nativefunction_new mal_symbol_q] \
    "keyword"      [nativefunction_new mal_keyword] \
    "keyword?"     [nativefunction_new mal_keyword_q] \
    \
    "pr-str"       [nativefunction_new mal_pr_str] \
    "str"          [nativefunction_new mal_str] \
    "prn"          [nativefunction_new mal_prn] \
    "println"      [nativefunction_new mal_println] \
    "read-string"  [nativefunction_new mal_read_string] \
    "readline"     [nativefunction_new mal_readline] \
    "slurp"        [nativefunction_new mal_slurp] \
    \
    "<"            [nativefunction_new mal_lt] \
    "<="           [nativefunction_new mal_lte] \
    ">"            [nativefunction_new mal_gt] \
    ">="           [nativefunction_new mal_gte] \
    "+"            [nativefunction_new mal_add] \
    "-"            [nativefunction_new mal_sub] \
    "*"            [nativefunction_new mal_mul] \
    "/"            [nativefunction_new mal_div] \
    "time-ms"      [nativefunction_new mal_time_ms] \
    \
    "list"         [nativefunction_new mal_list] \
    "list?"        [nativefunction_new mal_list_q] \
    "vector"       [nativefunction_new mal_vector] \
    "vector?"      [nativefunction_new mal_vector_q] \
    "hash-map"     [nativefunction_new mal_hash_map] \
    "map?"         [nativefunction_new mal_map_q] \
    "assoc"        [nativefunction_new mal_assoc] \
    "dissoc"       [nativefunction_new mal_dissoc] \
    "get"          [nativefunction_new mal_get] \
    "contains?"    [nativefunction_new mal_contains_q] \
    "keys"         [nativefunction_new mal_keys] \
    "vals"         [nativefunction_new mal_vals] \
    \
    "sequential?"  [nativefunction_new mal_sequential_q] \
    "cons"         [nativefunction_new mal_cons] \
    "concat"       [nativefunction_new mal_concat] \
    "nth"          [nativefunction_new mal_nth] \
    "first"        [nativefunction_new mal_first] \
    "rest"         [nativefunction_new mal_rest] \
    "empty?"       [nativefunction_new mal_empty_q] \
    "count"        [nativefunction_new mal_count] \
    "apply"        [nativefunction_new mal_apply] \
    "map"          [nativefunction_new mal_map] \
    \
    "conj"         [nativefunction_new mal_conj] \
    \
    "meta"         [nativefunction_new mal_meta] \
    "with-meta"    [nativefunction_new mal_with_meta] \
    "atom"         [nativefunction_new mal_atom] \
    "atom?"        [nativefunction_new mal_atom_q] \
    "deref"        [nativefunction_new mal_deref] \
    "reset!"       [nativefunction_new mal_reset_bang] \
    "swap!"        [nativefunction_new mal_swap_bang] \
]

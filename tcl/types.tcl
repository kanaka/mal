oo::class create MalObj {
    variable type val meta

    constructor {obj_type obj_val {obj_meta 0}} {
        set type $obj_type
        set val $obj_val
        set meta $obj_meta
    }

    method get_type {} {
        return $type
    }

    method get_val {} {
        return $val
    }

    method get_meta {} {
        return $meta
    }

    method set_val {new_val} {
        set val $new_val
        return $new_val
    }
}

proc obj_new {obj_type obj_val {obj_meta 0}} {
    MalObj new $obj_type $obj_val $obj_meta
}

proc obj_type {obj} {
    $obj get_type
}

proc obj_val {obj} {
    $obj get_val
}

proc obj_meta {obj} {
    $obj get_meta
}

proc obj_set_val {obj new_val} {
    $obj set_val $new_val
}

set ::mal_nil [obj_new "nil" {}]
set ::mal_true [obj_new "true" {}]
set ::mal_false [obj_new "false" {}]

proc nil_q {obj} {
    expr {[obj_type $obj] == "nil"}
}

proc false_q {obj} {
    expr {[obj_type $obj] == "false"}
}

proc true_q {obj} {
    expr {[obj_type $obj] == "true"}
}

proc bool_new {val} {
    if {$val == 0} {
        return $::mal_false
    } else {
        return $::mal_true
    }
}

proc integer_new {num} {
    obj_new "integer" $num
}

proc symbol_new {name} {
    obj_new "symbol" $name
}

proc symbol_q {obj} {
    expr {[obj_type $obj] == "symbol"}
}

proc string_new {val} {
    obj_new "string" $val
}

proc keyword_new {val} {
    string_new "\u029E$val"
}

proc keyword_q {obj} {
    expr {[obj_type $obj] == "string" && [string index [obj_val $obj] 0] == "\u029E"}
}

proc list_new {lst} {
    obj_new "list" $lst $::mal_nil
}

proc list_q {obj} {
    expr {[obj_type $obj] == "list"}
}

proc vector_new {lst} {
    obj_new "vector" $lst $::mal_nil
}

proc vector_q {obj} {
    expr {[obj_type $obj] == "vector"}
}

proc hashmap_new {lst} {
    obj_new "hashmap" $lst $::mal_nil
}

proc hashmap_q {obj} {
    expr {[obj_type $obj] == "hashmap"}
}

proc sequential_q {obj} {
    expr {[list_q $obj] || [vector_q $obj]}
}

proc sequential_equal_q {seq_a seq_b} {
    foreach obj_a [obj_val $seq_a] obj_b [obj_val $seq_b] {
        if {$obj_a == "" || $obj_b == "" || ![equal_q $obj_a $obj_b]} {
            return 0
        }
    }
    return 1
}

proc hashmap_equal_q {hashmap_a hashmap_b} {
    set dict_a [obj_val $hashmap_a]
    set dict_b [obj_val $hashmap_b]
    set keys_a [lsort [dict keys $dict_a]]
    set keys_b [lsort [dict keys $dict_b]]
    if {$keys_a != $keys_b} {
        return 0
    }
    foreach key $keys_a {
        set obj_a [dict get $dict_a $key]
        set obj_b [dict get $dict_b $key]
        if {![equal_q $obj_a $obj_b]} {
            return 0
        }
    }
    return 1
}

proc equal_q {a b} {
    if {[sequential_q $a] && [sequential_q $b]} {
        sequential_equal_q $a $b
    } elseif {[hashmap_q $a] && [hashmap_q $b]} {
        hashmap_equal_q $a $b
    } else {
        expr {[obj_type $a] == [obj_type $b] && [obj_val $a] == [obj_val $b]}
    }
}

proc nativefunction_new {name} {
    obj_new "nativefunction" $name $::mal_nil
}

proc function_new {body env binds} {
    set funcdict [dict create body $body env $env binds $binds is_macro 0]
    obj_new "function" $funcdict $::mal_nil
}

proc function_q {obj} {
    expr {[obj_type $obj] == "function"}
}

proc macro_q {obj} {
    expr {[obj_type $obj] == "function" && [dict get [obj_val $obj] is_macro]}
}

proc atom_new {val} {
    obj_new "atom" $val $::mal_nil
}

proc atom_q {obj} {
    expr {[obj_type $obj] == "atom"}
}

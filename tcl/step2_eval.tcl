source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl

proc READ str {
    read_str $str
}

proc eval_ast {ast env} {
    switch [obj_type $ast] {
        "symbol" {
            set varname [obj_val $ast]
            if {[dict exists $env $varname]} {
                return [dict get $env $varname]
            } else {
                error "'$varname' not found"
            }
        }
        "list" {
            set res {}
            foreach element [obj_val $ast] {
                lappend res [EVAL $element $env]
            }
            return [list_new $res]
        }
        "vector" {
            set res {}
            foreach element [obj_val $ast] {
                lappend res [EVAL $element $env]
            }
            return [vector_new $res]
        }
        "hashmap" {
            set res [dict create]
            dict for {k v} [obj_val $ast] {
                dict set res $k [EVAL $v $env]
            }
            return [hashmap_new $res]
        }
        default { return $ast }
    }
}

proc EVAL {ast env} {
    if {![list_q $ast]} {
        return [eval_ast $ast $env]
    }
    set lst_obj [eval_ast $ast $env]
    set lst [obj_val $lst_obj]
    set f [lindex $lst 0]
    set call_args [lrange $lst 1 end]
    apply $f $call_args
}

proc PRINT exp {
    pr_str $exp 1
}

proc REP {str env} {
    PRINT [EVAL [READ $str] $env]
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

set repl_env [dict create \
    "+" {{a} {mal_add $a}} \
    "-" {{a} {mal_sub $a}} \
    "*" {{a} {mal_mul $a}} \
    "/" {{a} {mal_div $a}} \
]

fconfigure stdout -translation binary

# repl loop
while {true} {
    set res [_readline "user> "]
    if {[lindex $res 0] == "EOF"} {
        break
    }
    set line [lindex $res 1]
    if {$line == ""} {
        continue
    }
    if { [catch { puts [REP $line $repl_env] } exception] } {
        puts "Error: $exception"
    }
}
puts ""

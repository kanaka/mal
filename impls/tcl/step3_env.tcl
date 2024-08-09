source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl
source env.tcl

proc READ str {
    read_str $str
}

proc EVAL {ast env} {
    set dbgenv [$env find "DEBUG-EVAL"]
    if {$dbgenv != 0} {
        set dbgeval [$env get "DEBUG-EVAL"]
        if {![false_q $dbgeval] && ![nil_q $dbgeval]} {
            set img [PRINT $ast]
            puts "EVAL: ${img}"
        }
    }

    switch [obj_type $ast] {
        "symbol" {
            set varname [obj_val $ast]
            return [$env get $varname]
        }
        "list" {
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

    set a0 [lindex [obj_val $ast] 0]
    if {$a0 == ""} {
        return $ast
    }
    set a1 [lindex [obj_val $ast] 1]
    set a2 [lindex [obj_val $ast] 2]
    switch [obj_val $a0] {
        "def!" {
            set varname [obj_val $a1]
            set value [EVAL $a2 $env]
            return [$env set $varname $value]
        }
        "let*" {
            set letenv [Env new $env]
            set bindings_list [obj_val $a1]
            foreach {varnameobj varvalobj} $bindings_list {
                $letenv set [obj_val $varnameobj] [EVAL $varvalobj $letenv]
            }
            return [EVAL $a2 $letenv]
        }
        default {
            set lst {}
            foreach element [obj_val $ast] {
                lappend lst [EVAL $element $env]
            }
            set f [lindex $lst 0]
            set call_args [lrange $lst 1 end]
            return [apply $f $call_args]
        }
    }
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

set repl_env [Env new]
$repl_env set "+" {{a} {mal_add $a}}
$repl_env set "-" {{a} {mal_sub $a}}
$repl_env set "*" {{a} {mal_mul $a}}
$repl_env set "/" {{a} {mal_div $a}}

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

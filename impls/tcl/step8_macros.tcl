source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl
source env.tcl
source core.tcl

proc READ str {
    read_str $str
}

proc starts_with {lst sym} {
    if {[llength $lst] != 2} {
        return 0
    }
    lassign [lindex $lst 0] a0
    return [symbol_q $a0] && [expr {[obj_val $a0] == $sym}]
}
proc qq_loop {elt acc} {
    if {[list_q $elt] && [starts_with [obj_val $elt] "splice-unquote"]} {
        return [list_new [list [symbol_new "concat"] [lindex [obj_val $elt] 1] $acc]]
    } else {
        return [list_new [list [symbol_new "cons"] [quasiquote $elt] $acc]]
    }
}
proc qq_foldr {xs} {
    set acc [list_new []]
    for {set i [expr {[llength $xs] - 1}]} {0 <= $i} {incr i -1} {
        set acc [qq_loop [lindex $xs $i] $acc]
    }
    return $acc
}

proc quasiquote {ast} {
    switch [obj_type $ast] {
        "symbol" {
            return [list_new [list [symbol_new "quote"] $ast]]
        }
        "hashmap" {
            return [list_new [list [symbol_new "quote"] $ast]]
        }
        "vector" {
            return [list_new [list [symbol_new "vec"] [qq_foldr [obj_val $ast]]]]
        }
        "list" {
            if {[starts_with [obj_val $ast] "unquote"]} {
                return [lindex [obj_val $ast] 1]
            } else {
                return [qq_foldr [obj_val $ast]]
            }
        }
        default {
            return $ast
        }
    }
}

proc EVAL {ast env} {
    while {true} {

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

        lassign [obj_val $ast] a0 a1 a2 a3
        if {$a0 == ""} {
            return $ast
        }
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
                set ast $a2
                set env $letenv
                # TCO: Continue loop
            }
            "quote" {
                return $a1
            }
            "quasiquote" {
                set ast [quasiquote $a1]
            }
            "defmacro!" {
                set varname [obj_val $a1]
                set value [EVAL $a2 $env]
                return [$env set $varname [macro_new $value]]
            }
            "do" {
                foreach element [lrange [obj_val $ast] 1 end-1] {
                    EVAL $element $env
                }
                set ast [lindex [obj_val $ast] end]
                # TCO: Continue loop
            }
            "if" {
                set condval [EVAL $a1 $env]
                if {[false_q $condval] || [nil_q $condval]} {
                    if {$a3 == ""} {
                        return $::mal_nil
                    }
                    set ast $a3
                } else {
                    set ast $a2
                }
                # TCO: Continue loop
            }
            "fn*" {
                set binds {}
                foreach v [obj_val $a1] {
                    lappend binds [obj_val $v]
                }
                return [function_new $a2 $env $binds]
            }
            default {
                set f [EVAL $a0 $env]
                set unevaluated_args [lrange [obj_val $ast] 1 end]
                if {[macro_q $f]} {
                    set fn [obj_val $f]
                    set f_ast   [dict get $fn body]
                    set f_env   [dict get $fn env]
                    set f_binds [dict get $fn binds]
                    set apply_env [Env new $f_env $f_binds $unevaluated_args]
                    set ast [EVAL $f_ast $apply_env]
                    continue
                }
                set call_args {}
                foreach element $unevaluated_args {
                    lappend call_args [EVAL $element $env]
                }
                switch [obj_type $f] {
                    function {
                        set fn [obj_val $f]
                        set ast [dict get $fn body]
                        set env [Env new [dict get $fn env] [dict get $fn binds] $call_args]
                        # TCO: Continue loop
                    }
                    nativefunction {
                        set body [concat [list [obj_val $f]] {$a}]
                        set lambda [list {a} $body]
                        return [apply $lambda $call_args]
                    }
                    default {
                        error "Not a function"
                    }
                }
            }
        }
    }
}

proc PRINT exp {
    pr_str $exp 1
}

proc REP {str env} {
    PRINT [EVAL [READ $str] $env]
}

proc RE {str env} {
    EVAL [READ $str] $env
}

proc mal_eval {a} {
    global repl_env
    EVAL [lindex $a 0] $repl_env
}

set repl_env [Env new]
dict for {k v} $core_ns {
    $repl_env set $k $v
}

$repl_env set "eval" [nativefunction_new mal_eval]

set argv_list {}
foreach arg [lrange $argv 1 end] {
    lappend argv_list [string_new $arg]
}
$repl_env set "*ARGV*" [list_new $argv_list]

# core.mal: defined using the language itself
RE "(def! not (fn* (a) (if a false true)))" $repl_env
RE "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))" $repl_env
RE "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" $repl_env

fconfigure stdout -translation binary

set DEBUG_MODE 0
if { [array names env DEBUG] != "" && $env(DEBUG) != "0" } {
    set DEBUG_MODE 1
}

if {$argc > 0} {
    REP "(load-file \"[lindex $argv 0]\")" $repl_env
    exit
}

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
        if { $DEBUG_MODE } {
            puts $::errorInfo
        }
    }
}
puts ""

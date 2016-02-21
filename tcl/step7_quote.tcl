source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl
source env.tcl
source core.tcl

proc READ str {
    read_str $str
}

proc is_pair {ast} {
    expr {[sequential_q $ast] && [llength [obj_val $ast]] > 0}
}

proc quasiquote {ast} {
    if {![is_pair $ast]} {
        return [list_new [list [symbol_new "quote"] $ast]]
    }
    lassign [obj_val $ast] a0 a1
    if {[symbol_q $a0] && [obj_val $a0] == "unquote"} {
        return $a1
    }
    lassign [obj_val $a0] a00 a01
    set rest [list_new [lrange [obj_val $ast] 1 end]]
    if {[is_pair $a0] && [symbol_q $a00] && [obj_val $a00] == "splice-unquote"} {
        return [list_new [list [symbol_new "concat"] $a01 [quasiquote $rest]]]
    } else {
        return [list_new [list [symbol_new "cons"] [quasiquote $a0] [quasiquote $rest]]]
    }
}

proc eval_ast {ast env} {
    switch [obj_type $ast] {
        "symbol" {
            set varname [obj_val $ast]
            return [$env get $varname]
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
    while {true} {
        if {![list_q $ast]} {
            return [eval_ast $ast $env]
        }
        lassign [obj_val $ast] a0 a1 a2 a3
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
            "do" {
                set el [list_new [lrange [obj_val $ast] 1 end-1]]
                eval_ast $el $env
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
                set lst_obj [eval_ast $ast $env]
                set lst [obj_val $lst_obj]
                set f [lindex $lst 0]
                set call_args [lrange $lst 1 end]
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
RE "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" $repl_env

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

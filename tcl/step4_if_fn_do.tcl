source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl
source env.tcl
source core.tcl

proc READ str {
    read_str $str
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
            return [EVAL $a2 $letenv]
        }
        "do" {
            set el [list_new [lrange [obj_val $ast] 1 end-1]]
            eval_ast $el $env
            return [EVAL [lindex [obj_val $ast] end] $env]
        }
        "if" {
            set condval [EVAL $a1 $env]
            if {[false_q $condval] || [nil_q $condval]} {
                if {$a3 == ""} {
                    return $::mal_nil
                }
                return [EVAL $a3 $env]
            }
            return [EVAL $a2 $env]
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
                    set funcdict [obj_val $f]
                    set body [dict get $funcdict body]
                    set env [dict get $funcdict env]
                    set binds [dict get $funcdict binds]
                    set funcenv [Env new $env $binds $call_args]
                    return [EVAL $body $funcenv]
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

proc PRINT exp {
    pr_str $exp 1
}

proc REP {str env} {
    PRINT [EVAL [READ $str] $env]
}

proc RE {str env} {
    EVAL [READ $str] $env
}

set repl_env [Env new]
dict for {k v} $core_ns {
    $repl_env set $k $v
}

# core.mal: defined using the language itself
RE "(def! not (fn* (a) (if a false true)))" $repl_env

fconfigure stdout -translation binary

set DEBUG_MODE 0
if { [array names env DEBUG] != "" && $env(DEBUG) != "0" } {
    set DEBUG_MODE 1
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

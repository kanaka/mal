source mal_readline.tcl

proc READ str {
    return $str
}

proc EVAL {ast env} {
    return $ast
}

proc PRINT exp {
    return $exp
}

proc REP str {
    PRINT [EVAL [READ $str] {}]
}

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
    puts [REP $line]
}
puts ""

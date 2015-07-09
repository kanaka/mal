source mal_readline.tcl
source types.tcl
source reader.tcl
source printer.tcl

proc READ str {
    read_str $str
}

proc EVAL {ast env} {
    return $ast
}

proc PRINT exp {
    pr_str $exp 1
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
    if { [catch { puts [REP $line] } exception] } {
        puts "Error: $exception"
    }
}
puts ""

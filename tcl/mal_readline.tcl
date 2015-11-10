if {[catch {package require tclreadline}]} {
    proc _readline prompt {
        puts -nonewline $prompt
        flush stdout
        if {[gets stdin line] < 0} {
            return {"EOF" ""}
        }
        list "OK" $line
    }
} else {
    set ::historyfile "$env(HOME)/.mal-history"
    ::tclreadline::readline initialize $::historyfile

    proc _readline prompt {
        set reached_eof 0
        ::tclreadline::readline eofchar { set reached_eof 1 }
        set line [::tclreadline::readline read $prompt]
        if {$reached_eof} {
            return {"EOF" ""}
        }
        ::tclreadline::readline write $::historyfile
        list "OK" $line
    }
}

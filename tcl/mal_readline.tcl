if {[lindex $argv 0] == "--raw"} {
    set ::readline_mode "raw"
    set argv [lrange $argv 1 end]
    incr argc -1
} else {
    if {[catch {package require tclreadline}]} {
        set ::readline_mode "raw"
    } else {
        set ::readline_mode "library"
    }
}

set ::historyfile "$env(HOME)/.mal-history"
set ::readline_library_initalized 0
proc readline_library_init {} {
    if {$::readline_library_initalized} {
        return
    }

    ::tclreadline::readline initialize $::historyfile
    ::tclreadline::readline builtincompleter 0
    ::tclreadline::readline customcompleter ""
    set ::readline_library_initalized 1
}

proc _readline_library prompt {
    readline_library_init

    set reached_eof 0
    ::tclreadline::readline eofchar { set reached_eof 1 }
    set line [::tclreadline::readline read $prompt]
    if {$reached_eof} {
        return {"EOF" ""}
    }
    ::tclreadline::readline write $::historyfile
    list "OK" $line
}

proc _readline_raw prompt {
    puts -nonewline $prompt
    flush stdout
    if {[gets stdin line] < 0} {
        return {"EOF" ""}
    }
    list "OK" $line
}

proc _readline prompt {
    if {$::readline_mode == "library"} {
        _readline_library $prompt
    } else {
        _readline_raw $prompt
    }
}

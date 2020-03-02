oo::class create Env {
    variable outer data

    constructor {{outerenv 0} {binds ""} {exprs ""}} {
        set outer $outerenv
        set data [dict create]
        if {$binds != ""} {
            for {set i 0} {$i < [llength $binds]} {incr i} {
                set b [lindex $binds $i]
                if {$b == "&"} {
                    set varrest [lindex $binds [expr {$i + 1}]]
                    set restexprs [list_new [lrange $exprs $i end]]
                    my set $varrest $restexprs
                    break
                } else {
                    my set $b [lindex $exprs $i]
                }
            }
        }
    }

    method set {symbol objval} {
        dict set data $symbol $objval
        return $objval
    }

    method find {symbol} {
        if {[dict exist $data $symbol]} {
            return [self]
        } elseif {$outer != 0} {
            return [$outer find $symbol]
        } else {
            return 0
        }
    }

    method get {symbol} {
        set foundenv [my find $symbol]
        if {$foundenv == 0} {
            error "'$symbol' not found"
        } else {
            return [$foundenv get_symbol $symbol]
        }
    }

    method get_symbol {symbol} {
        dict get $data $symbol
    }
}

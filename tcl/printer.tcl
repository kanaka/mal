proc format_list {elements start_char end_char readable} {
    set res {}
    foreach element $elements {
        lappend res [pr_str $element $readable]
    }
    set joined [join $res " "]
    return "${start_char}${joined}${end_char}"
}

proc format_hashmap {dictionary readable} {
    set lst {}
    dict for {keystr valobj} $dictionary {
        lappend lst [string_new $keystr]
        lappend lst $valobj
    }
    format_list $lst "\{" "\}" $readable
}

proc format_string {str readable} {
    if {[string index $str 0] == "\u029E"} {
        return ":[string range $str 1 end]"
    } elseif {$readable} {
        set escaped [string map {"\n" "\\n" "\"" "\\\"" "\\" "\\\\"} $str]
        return "\"$escaped\""
    } else {
        return $str
    }
}

proc format_function {funcdict} {
    set type "function"
    if {[dict get $funcdict is_macro]} {
        set type "macro"
    }
    return "<$type:args=[join [dict get $funcdict binds] ","]>"
}

proc pr_str {ast readable} {
    set nodetype [obj_type $ast]
    set nodevalue [obj_val $ast]
    switch $nodetype {
        nil            { return "nil" }
        true           { return "true" }
        false          { return "false" }
        integer        { return $nodevalue }
        symbol         { return $nodevalue }
        string         { return [format_string $nodevalue $readable] }
        list           { return [format_list $nodevalue "(" ")" $readable] }
        vector         { return [format_list $nodevalue "\[" "\]" $readable] }
        hashmap        { return [format_hashmap [dict get $nodevalue] $readable] }
        atom           { return "(atom [pr_str $nodevalue $readable])" }
        function       { return [format_function $nodevalue] }
        nativefunction { return "<nativefunction:$nodevalue>" }
        default        { error "cannot print type $nodetype" }
    }
}

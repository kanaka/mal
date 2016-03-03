MalCore := Map with(
    "=", block(a, a at(0) == a at(1)),

    "pr-str",  block(a, a map(s, s malPrint(true)) join(" ")),
    "str",     block(a, a map(s, s malPrint(false)) join("")),
    "prn",     block(a, a map(s, s malPrint(true)) join(" ") println ; nil),
    "println", block(a, a map(s, s malPrint(false)) join(" ") println ; nil),

    "<",  block(a, a at(0) <  a at(1)),
    "<=", block(a, a at(0) <= a at(1)),
    ">",  block(a, a at(0) >  a at(1)),
    ">=", block(a, a at(0) >= a at(1)),
    "+",  block(a, a at(0) +  a at(1)),
    "-",  block(a, a at(0) -  a at(1)),
    "*",  block(a, a at(0) *  a at(1)),
    "/",  block(a, a at(0) /  a at(1)),

    "list",  block(a, a),
    "list?", block(a, a at(0) type == "MalList"),

    "empty?", block(a, a at(0) ifNil(true) isEmpty),
    "count",  block(a, a at(0) ifNil(return(0)) size)
)

USING: assocs kernel lib.types tools.test ;
IN: lib.env

{ "1" } [
    T{ malsymbol { name "foo" } }
    T{ malenv
        { outer T{ malenv f f H{ { "foo" "2" } } } }
        { data H{ { "foo" "1" } } }
    } env-get
] unit-test

{ "2" } [
    T{ malsymbol { name "foo" } }
    T{ malenv
        { outer T{ malenv f f H{ { "foo" "2" } } } }
        { data H{ { "bar" "1" } } }
    } env-get
] unit-test

{ "3" } [
    T{ malsymbol { name "foo" } }
    T{ malenv { outer f } { data H{ } } }
    [ [ "3" ] 2dip env-set ] [ env-get ] 2bi
] unit-test

[
    T{ malsymbol { name "baz" } }
    T{ malenv
        { outer T{ malenv f f H{ { "foo" "2" } } } }
        { data H{ { "bar" "1" } } }
    } env-get
] must-fail

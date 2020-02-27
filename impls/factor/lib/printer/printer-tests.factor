USING: lists lib.types tools.test ;
IN: lib.printer

{ "(atom \"foo\")" } [ T{ malatom { val "foo" } } pr-str ] unit-test
{ "#<fn>" } [ T{ malfn } pr-str ] unit-test
{ ":foo" } [ T{ malkeyword { name "foo" } } pr-str ] unit-test
{ "foo" } [ T{ malsymbol { name "foo" } } pr-str ] unit-test
{ "14" } [ 14 pr-str ] unit-test
{ "\"\\\\foo\\\"\"" } [ "\\foo\"" pr-str ] unit-test
{ "(1 2 3 4)" } [ { 1 2 3 4 } pr-str ] unit-test
{ "[1 2 3 4]" } [ V{ 1 2 3 4 } pr-str ] unit-test
{ "{1 2}" } [ H{ { 1 2 } } pr-str ] unit-test
{ "true" } [ t pr-str ] unit-test
{ "false" } [ f pr-str ] unit-test
{ "nil" } [ +nil+ pr-str ] unit-test

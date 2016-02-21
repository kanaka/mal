USING: lists mal.types tools.test ;
IN: mal.reader

{ "foo" } [ "\"foo\"" read-atom ] unit-test
{ T{ malkeyword { name "foo" } } } [ ":foo" read-atom ] unit-test
{ f } [ "false" read-atom ] unit-test
{ t } [ "true" read-atom ] unit-test
{ +nil+ } [ "nil" read-atom ] unit-test
{ T{ malsymbol { name "foo" } } } [ "foo" read-atom ] unit-test
{ 14 } [ "14" read-atom ] unit-test
{ 1.5 } [ "1.5" read-atom ] unit-test
{ 2/3 } [ "2/3" read-atom ] unit-test

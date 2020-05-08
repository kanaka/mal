USING: assocs effects kernel sequences stack-checker tools.test ;
IN: lib.core

{ t } [
    ns values [
        infer ( x -- * ) ( x -- x ) [ effect= ] bi-curry@ bi or
    ] all?
] unit-test

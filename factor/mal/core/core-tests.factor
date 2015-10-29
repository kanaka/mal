USING: assocs effects kernel sequences stack-checker tools.test ;
IN: mal.core

{ t } [
    ns values [
        infer ( x -- * ) ( x -- x ) [ effect= ] bi-curry@ bi or
    ] all?
] unit-test

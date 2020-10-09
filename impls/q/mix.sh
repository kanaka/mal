#!/bin/sh

fname="$1"
shift

rm -f "$fname"_mixed.q

for mixin in "$@"; do
    cat "$mixin".q >> "$fname"_mixed.q
done

cat "$fname".q >> "$fname"_mixed.q

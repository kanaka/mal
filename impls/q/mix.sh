#!/bin/sh

fname="$1"
shift

for mixin in "$@"; do
    cat "$mixin".q >> "$fname"_mixed.q
done

cat "$fname".q >> "$fname"_mixed.q

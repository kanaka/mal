#!/bin/bash

chuck_options=""

if [[ $1 == --silent ]]; then
    shift
    chuck_options="--silent"
fi

script_file=$1

export CHUCK_ARGS="$@"

chuck $chuck_options $(awk "match(\$0,\"^ *// *@import (.+)\",m) {printf \"%s \",m[1]} END {print \"$script_file\"}" $script_file)

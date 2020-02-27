#!/bin/bash

# ghdl doesn't allow passing command-line arguments to the VHDL program.  To
# circumvent that, we write the command-line arguments as lines in
# vhdl_argv.tmp, and read the content of that file at the beginning of the VHDL
# program.

cleanup() {
    trap - TERM QUIT INT EXIT
    rm -f vhdl_argv.tmp
}
trap "cleanup" TERM QUIT INT EXIT

bin="$1"
shift

for arg in "$@" ; do
    echo "$arg"
done > vhdl_argv.tmp

$bin

#!/bin/bash

#
# Usage: run_argv_test.sh <command line arguments to run mal>
#
# Example: run_argv_test.sh python step6_file.py
#

assert_equal() {
  if [ "$1" = "$2" ] ; then
    echo "OK: '$1'"
  else
    echo "FAIL: Expected '$1' but got '$2'"
    echo
    exit 1
  fi
}

if [ -z "$1" ] ; then
  echo "Usage: $0 <command line arguments to run mal>"
  exit 1
fi

root="$(dirname $0)"

out="$( $@ $root/tests/print_argv.mal aaa bbb ccc | tr -d '\r' )"
assert_equal '("aaa" "bbb" "ccc")' "$out"

# Note: The 'make' implementation cannot handle arguments with spaces in them,
# so for now we skip this test.
#
# out="$( $@ $root/tests/print_argv.mal aaa 'bbb ccc' ddd )"
# assert_equal '("aaa" "bbb ccc" "ddd")' "$out"

out="$( $@ $root/tests/print_argv.mal | tr -d '\r' )"
assert_equal '()' "$out"

echo 'Passed all *ARGV* tests'
echo

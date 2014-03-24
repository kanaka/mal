#!/bin/bash

INTERACTIVE=

source tests/common.sh
source reader.sh

echo "Testing read of constants/strings"
assert_eq $LINENO 2 "READ_STR '2'; number_pr_str \$r"
assert_eq $LINENO 12345 "READ_STR '12345'; number_pr_str \$r"
assert_eq $LINENO 12345 "READ_STR '12345 \"abc\"'; number_pr_str \$r"
assert_eq $LINENO 'abc' "READ_STR '\"abc\"'; number_pr_str \$r"
assert_eq $LINENO 'a string (with parens)' "READ_STR '\"a string (with parens)\"'; number_pr_str \$r"

echo "Testing read of symbols"
assert $LINENO "READ_STR 'abc'; _symbol? \$r"
assert_eq $LINENO 'abc' "READ_STR 'abc'; symbol_pr_str \$r"
assert_eq $LINENO '.' "READ_STR '.'; symbol_pr_str \$r"

raw_val () {
    r="${ANON["${1}"]}"
}

echo "Testing READ_STR of strings"
assert_eq $LINENO 'a string' "READ_STR '\"a string\"'; raw_val \$r"
assert_eq $LINENO 'a string (with parens)' "READ_STR '\"a string (with parens)\"'; raw_val \$r"
assert_eq $LINENO 'a string' "READ_STR '\"a string\"()'; raw_val \$r"
assert_eq $LINENO 'a string' "READ_STR '\"a string\"123'; raw_val \$r"
assert_eq $LINENO 'a string' "READ_STR '\"a string\"abc'; raw_val \$r"
assert_eq $LINENO '' "READ_STR '\"\"'; raw_val \$r"
assert_eq $LINENO 'abc ' "READ_STR '\"abc \"'; raw_val \$r"
assert_eq $LINENO ' abc' "READ_STR '\" abc\"'; raw_val \$r"
assert_eq $LINENO '$abc' "READ_STR '\"\$abc\"'; raw_val \$r"
assert_eq $LINENO 'abc$()' "READ_STR '\"abc\$()\"'; raw_val \$r"
# TODO: fix parsing of escaped characters
#assert_eq $LINENO '"xyz"' "READ_STR '\"\\\"xyz\\\"\"'; raw_val \$r"

echo "Testing READ_STR of lists"
assert_eq $LINENO 2 "READ_STR '(2 3)'; _count \$r"
assert_eq $LINENO 2 "READ_STR '(2 3)'; first \$r; number_pr_str \$r"
assert_eq $LINENO 3 "READ_STR '(2 3)'; rest \$r; first \$r; number_pr_str \$r"

READ_STR "(+ 1 2 \"str1\" \"string (with parens) and 'single quotes'\")"
L="${r}"
assert_eq $LINENO 5 "_count \$r"
assert_eq $LINENO 'str1' "_nth ${L} 3; raw_val \$r"
assert_eq $LINENO "string (with parens) and 'single quotes'" "_nth ${L} 4; raw_val \$r"
assert_eq $LINENO '(2 3)' "READ_STR '(2 3)'; list_pr_str \$r"
assert_eq $LINENO '(2 3 "string (with parens)")' "READ_STR '(2 3 \"string (with parens)\")'; list_pr_str \$r yes"


echo "Testing READ_STR of vectors"
assert_eq $LINENO 2 "READ_STR '[2 3]'; _count \$r"
assert_eq $LINENO 2 "READ_STR '[2 3]'; first \$r; number_pr_str \$r"
assert_eq $LINENO 3 "READ_STR '[2 3]'; rest \$r; first \$r; number_pr_str \$r"

READ_STR "[+ 1 2 \"str1\" \"string (with parens) and 'single quotes'\"]"
L="${r}"
assert_eq $LINENO 5 "_count \$r"
assert_eq $LINENO 'str1' "_nth ${L} 3; raw_val \$r"
assert_eq $LINENO "string (with parens) and 'single quotes'" "_nth ${L} 4; raw_val \$r"
assert_eq $LINENO '[2 3]' "READ_STR '[2 3]'; vector_pr_str \$r yes"
assert_eq $LINENO '[2 3 "string (with parens)"]' "READ_STR '[2 3 \"string (with parens)\"]'; vector_pr_str \$r yes"


echo "Testing READ_STR of quote/quasiquote"
assert_eq $LINENO 'quote' "READ_STR \"'1\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 1 "READ_STR \"'1\"; _nth \$r 1; raw_val \$r"
assert_eq $LINENO 'quote' "READ_STR \"'(1 2 3)\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 3 "READ_STR \"'(1 2 3)\"; _nth \$r 1; _nth \$r 2; raw_val \$r"

assert_eq $LINENO 'quasiquote' "READ_STR \"\\\`1\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 1 "READ_STR \"\\\`1\"; _nth \$r 1; raw_val \$r"
assert_eq $LINENO 'quasiquote' "READ_STR \"\\\`(1 2 3)\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 3 "READ_STR \"\\\`(1 2 3)\"; _nth \$r 1; _nth \$r 2; raw_val \$r"

assert_eq $LINENO 'unquote' "READ_STR \"~1\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 1 "READ_STR \"~1\"; _nth \$r 1; raw_val \$r"
assert_eq $LINENO 'unquote' "READ_STR \"~(1 2 3)\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 3 "READ_STR \"~(1 2 3)\"; _nth \$r 1; _nth \$r 2; raw_val \$r"

assert_eq $LINENO 'splice-unquote' "READ_STR \"~@1\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 1 "READ_STR \"~@1\"; _nth \$r 1; raw_val \$r"
assert_eq $LINENO 'splice-unquote' "READ_STR \"~@(1 2 3)\"; _nth \$r 0; raw_val \$r"
assert_eq $LINENO 3 "READ_STR \"~@(1 2 3)\"; _nth \$r 1; _nth \$r 2; raw_val \$r"


echo "All tests completed"

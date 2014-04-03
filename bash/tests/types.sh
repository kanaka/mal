#!/bin/bash

source tests/common.sh
source types.sh

echo "Testing type function"
assert_eq $LINENO bash "_obj_type xyz"
assert_eq $LINENO nil "_obj_type ${__nil}"
assert_eq $LINENO true "_obj_type ${__true}"
assert_eq $LINENO false "_obj_type ${__false}"


echo "Testing number? function"
assert_eq $LINENO number "number 1; _obj_type \$r"
assert_eq $LINENO number "number 10; _obj_type \$r"
assert_eq $LINENO number "number 12345; _obj_type \$r"


echo "Testing symbols"
assert_eq $LINENO symbol "symbol abc; _obj_type \$r"
symbol "a sym value"; SYM1="${r}"
assert_eq $LINENO "a sym value" "symbol_pr_str ${SYM1} yes"
assert_eq $LINENO ${__true} "symbol? ${SYM1}"


echo "Testing strings"
assert_eq $LINENO string "string abc; _obj_type \$r"
string "a string value"; STR1="${r}"
assert_eq $LINENO "\"a string value\"" "string_pr_str ${STR1} yes"
assert_eq $LINENO ${__true} "string? ${STR1}"
# TODO: fix to count characters instead of words
#assert_eq $LINENO 14 "_count ${STR1}"

string "a string (with parens)"; STR2="${r}"
assert_eq $LINENO "\"a string (with parens)\"" "string_pr_str ${STR2} yes"
assert_eq $LINENO ${__true} "string? ${STR2}"

# TODO: test str and subs


echo "Testing function objects"
assert_eq $LINENO "function" "_function \"echo hello\"; _obj_type \$r"
_function "r=\"arg1:'\$1' arg2:'\$2'\""; FN1="${r}"
assert_eq $LINENO ${__true} "function? ${FN1}"
assert_eq $LINENO "arg1:'A' arg2:'B'" "${ANON["${FN1}"]} A B"



echo "Testing lists"
list; LE="${r}"
assert_eq $LINENO list "_obj_type ${LE}"

echo "Testing lists (cons)"
list; cons P ${r}; L1="${r}"
cons Q ${L1}; L2="${r}"
assert_eq $LINENO ${__true} "list? ${L1}"
assert_eq $LINENO ${__true} "list? ${L2}"
assert_eq $LINENO P "first ${L1}"
assert_eq $LINENO 2 "_count ${L2}"
assert_eq $LINENO Q "first ${L2}"
assert_eq $LINENO P "_nth ${L2} 1"
rest ${L2}; L2R="${r}"

echo "Testing lists (concat)"
concat ${L1} ${L2}; L1_2="${r}"
assert_eq $LINENO 3 "_count ${L1_2}"
assert_eq $LINENO P "first ${L1_2}"
assert_eq $LINENO Q "_nth ${L1_2} 1"
assert_eq $LINENO P "_nth ${L1_2} 2"
rest ${L1_2}; L1_2R="${r}"

echo "Testing lists (conj)"
list; conj ${r} A B; L3="${r}"
list; conj ${r} X ${L3}; L4="${r}"
assert_eq $LINENO ${__true} "list? ${L3}"
assert_eq $LINENO ${__true} "list? ${L4}"
assert_eq $LINENO A "first ${L3}"
assert_eq $LINENO X "first ${L4}"
_nth ${L4} 1; L4_1="${r}"
assert_eq $LINENO ${__true} "list? ${L4_1}"
assert_eq $LINENO A "first ${L4_1}"


echo "Testing hash maps"
hash_map; X="${r}"
hash_map; Y="${r}"
assert_eq $LINENO ${__true} "hash_map? ${X}"
assert_eq $LINENO ${__true} "hash_map? ${Y}"

string "a"
mykey="${r}"
assert_eq $LINENO "" "_get ${X} a"
assert_eq $LINENO ${__false} "contains? ${X} ${mykey}"
assoc! ${X} a 'value of X a'
assert_eq $LINENO "value of X a" "_get ${X} a"
assert_eq $LINENO ${__true} "contains? ${X} ${mykey}"

# TODO: more testing of Y, assoc!, dissoc!


# TODO: vectors


echo "Testing _map/map function"
list; conj "${r}" 1 2 3; L5="${r}"
inc () { r=$(( ${1} + 1)); }
assert_eq $LINENO "2 3 4" "_map inc ${L5}; r=\${ANON[\$r]}"
_function "r=\$(( \$1 + 1 ));"; inc_func="${r}"
assert_eq $LINENO "2 3 4" "map ${inc_func} ${L5}; r=\${ANON[\$r]}"


echo "Testing equal? function" 
assert_eq $LINENO ${__true}  "equal? 2 2"
assert_eq $LINENO ${__false} "equal? 2 3"
assert_eq $LINENO ${__false} "equal? 2 3"
assert_eq $LINENO ${__true}  "equal? abc abc"
assert_eq $LINENO ${__false} "equal? abc abz"
assert_eq $LINENO ${__false} "equal? zbc abc"
assert_eq $LINENO ${__true}  "string abc; A=\$r; string abc; B=\$r; equal? \$A \$B"
assert_eq $LINENO ${__false} "string abc; A=\$r; string abz; B=\$r; equal? \$A \$B"
assert_eq $LINENO ${__false} "string zbc; A=\$r; string abc; B=\$r; equal? \$A \$B"
assert_eq $LINENO ${__true}  "symbol abc; A=\$r; symbol abc; B=\$r; equal? \$A \$B"
assert_eq $LINENO ${__false} "symbol abc; A=\$r; symbol abz; B=\$r; equal? \$A \$B"
assert_eq $LINENO ${__false} "symbol zbc; A=\$r; symbol abc; B=\$r; equal? \$A \$B"
list; conj "${r}" 1 2 3; L6="${r}"
list; conj "${r}" 1 2 3; L7="${r}"
list; conj "${r}" 1 2 Z; L8="${r}"
list; conj "${r}" Z 2 3; L9="${r}"
list; conj "${r}" 1 2;   L10="${r}"
assert_eq $LINENO ${__true}  "equal? ${L6} ${L7}"
assert_eq $LINENO ${__false} "equal? ${L6} ${L8}"
assert_eq $LINENO ${__false} "equal? ${L6} ${L9}"
assert_eq $LINENO ${__false} "equal? ${L6} ${L10}"
assert_eq $LINENO ${__false} "equal? ${L10} ${L6}"

# TODO: empty? function tests

echo "Testing ENV environment (1 level)"
ENV; env1="${r}"
assert_eq $LINENO "" "ENV_GET \"${env1}\" a"
ENV_SET "${env1}" a "val_a"
ENV_SET "${env1}" b "val_b"
ENV_SET "${env1}" = "val_eq"
assert_eq $LINENO "val_a" "ENV_GET \"${env1}\" a"
assert_eq $LINENO "val_b" "ENV_GET \"${env1}\" b"
assert_eq $LINENO "val_eq" "ENV_GET \"${env1}\" ="
assert_eq $LINENO "${env1}" "ENV_FIND \"${env1}\" ="

echo "Testing ENV environment (2 levels)"
ENV "${env1}"; env2="${r}"
ENV_SET "${env2}" b "val_b2"
ENV_SET "${env2}" c "val_c"
assert_eq $LINENO "${env1}" "ENV_FIND \"${env2}\" a"
assert_eq $LINENO "${env2}" "ENV_FIND \"${env2}\" b"
assert_eq $LINENO "${env2}" "ENV_FIND \"${env2}\" c"
assert_eq $LINENO "val_a" "ENV_GET \"${env2}\" a"
assert_eq $LINENO "val_b2" "ENV_GET \"${env2}\" b"
assert_eq $LINENO "val_c" "ENV_GET \"${env2}\" c"


echo "All tests completed"

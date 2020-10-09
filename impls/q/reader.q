/ -------------------------------------------- Lexer --------------------------------------------
dopeek: {first x};
doconsume: {tail x};
donext: {(dopeek x; doconsume x) };

/ Inputs:
/ - string
/ Outputs:
/ - list of tokens
tokenize: {[str]; s: collapse[accumulate[notempty; str; read_token] @ 0]; s where not s~\:() };

/ top(acc)0 == `char and x0 == `char -> push(`char; pop()1,x1); else push(x)
collapse: {last ({inp: x @ 0; dat: tail inp; val: first inp; stk: x @ 1; $[not[(notempty val) and (notempty (last stk))]; (dat; stk, enlist val); $[(`char = ((last stk) @ 0)) and (`char = (val @ 0)); (dat; (init stk), enlist (`char; ((last stk) @ 1), (val @ 1))); (dat; stk, enlist val)]] }/ [{notempty (x @ 0)}; (tail x; enlist (first x))])};


read_token: {[str]; l: first str; actionordefault[l; tokenmap][l; tail str]}; 
read_string: {[str]; l: first str; actionordefault[l; strtokenmap][l; tail str]};
read_comment: {(first x; tail x)};

doesnt_end_string: {$[=[count x; 0]; 0b; $[=[first x; "\\"]; 1b; $[=[first x; "\""]; 0b; 1b]]]};
doesnt_end_comment:{$[=[count x; 0]; 0b; $[=[first x; "\n"]; 0b; 1b]]};
escape: {x};

readtilde: {$[=["@"; first y]; ((`special; "~@"); tail y); ((`special; x); y)]};
readone: {((`special; x); y)};
readpunc: {((`punc; x); y)};
readignore: {((); y)};
readstring: {s: accumulate[doesnt_end_string; y; read_string]; tok: first s; rest: last s; $[=["\""; first rest]; ((`string; raze tok); tail rest); ((`incompletestring; raze tok); rest)]};
readstrbackslash: {(actionordefault[first y; backslashmap][first y]; tail y)};
readcomment: {s: accumulate[doesnt_end_comment; y; read_comment]; rest: last s; ((); rest)};
readchar: {((`char; x); y)};
takeone: {(x; y)};

tokenmap: ([tok: ("a "; "a,"; "a\n"; "a~"; "a["; "a]"; "a{"; "a}"; "a("; "a)"; "a'"; "a`"; "a^"; "a@"; "a\""; "a;"; "d.")]
            fn: (readignore; readignore; readignore; readtilde; readpunc; readpunc; readpunc; readpunc; readpunc; readpunc; readone; readone; readone; readone; readstring; readcomment; readchar));

strtokenmap: ([tok: ("a\\"; "d.")]
               fn: (readstrbackslash; takeone));

backslashmap: ([tok: ("a\\"; "a\""; "an"; "d.")]
                fn: ({x}; {x}; {"\n"}; {"\\", x}))

token_eq: {(y = first x) and strequals[z; last x]};
token_ty_eq: {(y = first x)};

/ -------------------------------------------- Reader --------------------------------------------
ends_list: {$[notempty x; token_eq[first x; `punc; ")"]; 1b]}
ends_vector: {$[notempty x; token_eq[first x; `punc; "]"]; 1b]}
ends_hmap: {$[notempty x; token_eq[first x; `punc; "}"]; 1b]}
read_form: {[tokens]; $[notempty tokens; $[token_eq[dopeek tokens; `punc; "("]; read_list doconsume tokens;
                                           token_eq[dopeek tokens; `punc; "["]; read_vec doconsume tokens;
                                           token_eq[dopeek tokens; `punc; "{"]; read_hmap doconsume tokens;
                                           read_atom_or_expand tokens];
                                         ()]};
read_list: {[tokens]; s: accumulate[{not ends_list x}; tokens; read_form]; $[ends_list[last s] and notempty[last s]; ((`list; first s); tail last s); ((`error; "unbalanced list"); last s)]};
read_vec: {[tokens]; s: accumulate[{not ends_vector x}; tokens; read_form]; $[ends_vector[last s] and notempty[last s]; ((`vector; first s); tail last s); ((`error; "unbalanced vector"); last s)]};
read_hmap: {[tokens]; s: accumulate[{not ends_hmap x}; tokens; read_form]; $[ends_hmap[last s] and notempty[last s]; ((make_hmap first s); tail last s); ((`error; "unbalanced hashmap"); last s)]};
take_one_and_splice: {rest: read_form tail y; ((`list; ((`symbol; x); rest@0)); rest@1)};
read_atom_or_expand: {[tokens]; s: dopeek tokens; $[token_eq[s; `special; "~@"]; take_one_and_splice["splice-unquote"; tokens];
                                                    token_eq[s; `special; "@"]; take_one_and_splice["deref"; tokens];
                                                    token_eq[s; `special; "^"]; {fst:read_form tail x; snd:read_form last fst; ((`list; ((`symbol; "with-meta"); first snd; first fst)); snd@1)}tokens;
                                                    token_eq[s; `special; "'"]; take_one_and_splice["quote"; tokens];
                                                    token_eq[s; `special; "`"]; take_one_and_splice["quasiquote"; tokens];
                                                    token_eq[s; `special; "~"]; take_one_and_splice["unquote"; tokens];
                                                    read_atom tokens]};
read_atom: {[tokens]; s: dopeek tokens; ($[token_ty_eq[s; `char]; read_symbol_or_number_or_kw_or_native (s @ 1);
                                           token_ty_eq[s; `string]; s;
                                           token_ty_eq[s; `incompletestring]; (`error; "unbalanced string");
                                           (`error; "wrong kind ", string s)];
                                         doconsume tokens)};
read_symbol_or_number_or_kw_or_native: {$[":" = first x; (`keyword; tail x); read_symbol_or_number_or_native x]};
read_symbol_or_number_or_native: {$[strequals[x; "true"]; (`true; ());
                                    strequals[x; "false"]; (`false; ());
                                    strequals[x; "nil"]; (`nil; ());
                                    read_symbol_or_number x]}
DIGITS: "0123456789";
skip_numerics: {a:$[x ~ "-"; x;
               first x = "-"; (tail x) except DIGITS;
               x except DIGITS]; a};
read_symbol_or_number: {v:$[first x = "-"; x; "0",x]; $[notempty skip_numerics v; (`symbol; x); (`number; parse v)]};

read_str: {[str]; first read_form tokenize str};

maketable: {$[notempty x; (flip (enlist `k)!enlist (x`k))!x; x]};
make_hmap: {[items]; $[=[0; (count items) mod 2]; (`hashmap; maketable first accumulate[notempty; items; {(`k`v!(enlist x @ 0; enlist x @ 1); skip[2; x])}]); (`error; "odd number of items in hashmap")]}

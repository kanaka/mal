isfn:{t:type x; (100h = t) or (104h = t)};
pr_str: {[x;readably]; $[isfn[x]; "<#fn>"; (printmap (x @ 0))[x; readably]]};

prhmap: {[x;y]; "{", $[notempty x; (({x," ",y}/) {(pr_str[(first x`k); x`y]), " ", (pr_str[(first x`v); x`y])} each select y,k,v from x); ""], "}"};
prlist: {[x;y;z]; (first z), (({x," ",y}/) {pr_str[x`v; x`y]} each select y,v from []flip (enlist `v)!(enlist x)), (last z)};
escapemap: ([tok: ("a\\"; "a\""; "a\n"; "d.")]
            fn: ({"\\\\"}; {"\\\""}; {"\\n"}; {x}));
escape: {[x]; first accumulate[notempty; x; {(actionordefault[first x; escapemap][first x]; tail x)}]};

printmap: `list`vector`hashmap`string`symbol`number`keyword`nil`true`false`error`atom!(
    {[x;y]; prlist[x @ 1; y; "()"]};
    {[x;y]; prlist[x @ 1; y; "[]"]};
    {[x;y]; prhmap[x @ 1; y]};
    {[x;y]; raze $[y; "\""; ""], $[y; escape[x @ 1]; x @ 1], $[y; "\""; ""]};
    {[x;y]; x @ 1};
    {[x;y]; string x @ 1};
    {[x;y]; raze ":", (x @ 1)};
    {[x;y]; "nil"};
    {[x;y]; "true"};
    {[x;y]; "false"};
    {[x;y]; "Error: ", (x @ 1)};
    {[x;y]; "(atom ", (pr_str[get_atom last x; y]), ")"});

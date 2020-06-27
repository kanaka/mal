pr_str: {[x;readably]; (printmap (x @ 0))[x; readably]};

prhmap: {[x;y]; "{", $[notempty x; (({x," ",y}/) {(pr_str[(first x`k); x`y]), " ", (pr_str[(first x`v); x`y])} each select y,k,v from x); ""], "}"};
prlist: {[x;y;z]; (first z), (({x," ",y}/) {pr_str[x`v; x`y]} each select y,v from []flip (enlist `v)!(enlist x)), (last z)};

printmap: `list`vector`hashmap`string`symbol`number`keyword`nil`true`false`error!(
    {[x;y]; prlist[x @ 1; y; "()"]};
    {[x;y]; prlist[x @ 1; y; "[]"]};
    {[x;y]; prhmap[x @ 1; y]};
    {[x;y]; raze "\"", $[y; x @ 1; x @ 1], "\""}; / TODO: impl 'readably'
    {[x;y]; x @ 1};
    {[x;y]; string x @ 1};
    {[x;y]; raze ":", (x @ 1)};
    {[x;y]; "nil"};
    {[x;y]; "true"};
    {[x;y]; "false"};
    {[x;y]; "Error: ", (x @ 1)});

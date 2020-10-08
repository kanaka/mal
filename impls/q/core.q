add_core_ns: {[env];
  accumulate[notempty; core_ns; {[xs; env]; env_set[env; xs @ 0; xs @ 1]; ((); skip[2; xs])}[;env]];
  env};

isseq_container: {[ty]; (ty = `list) or (ty = `vector)};
uninhabited: {[ty]; (`true`false`nil?ty) <> 3};

vequals: {[x; y];
  xty: first x;
  yty: first y;
  $[(isseq_container xty) and (isseq_container yty); 
      $[(count last x) <> (count last y); 0b; (&/) ((enlist 1b),((last x) vequals' (last y)))];
    (xty <> yty); 0b;
    / TODO: Implement hashmap equality
    xty = `hashmap; hashmapequals[last x; last y];
    xty = `string; strequals[last x; last y];
    xty = `keyword; strequals[last x; last y];
    uninhabited xty; 1b;
    (last x) = (last y)]};

core_ns: (
  "prn"; {[xs];
    v:" " sv (first accumulate[notempty; xs; {(pr_str[first x; 1b]; tail x)}]);
    1 v; 1"\n";
    (`nil; ())};
  "str"; {[xs];
    (`string; "" sv (first accumulate[notempty; xs; {(pr_str[first x; 0b]; tail x)}]))};
  "pr-str"; {[xs];
    (`string; " " sv (first accumulate[notempty; xs; {(pr_str[first x; 1b]; tail x)}]))};
  "println"; {[xs];
    v:" " sv (first accumulate[notempty; xs; {(pr_str[first x; 0b]; tail x)}]);
    1 v; 1"\n";
    (`nil; ())};
  "list"; {[xs]; (`list; xs)};
  "list?"; {[xs]; bool (`list = first first xs)};
  "empty?"; {[xs]; bool (0 = count last first xs)};
  "count"; {[xs]; number count last first xs};
  "="; {[xs]; bool vequals[xs @ 0; xs @ 1]};
  "<"; {[xs]; bool ((last first xs) < (last (xs @ 1)))};
  ">"; {[xs]; bool ((last first xs) > (last (xs @ 1)))};
  ">="; {[xs]; bool ((last first xs) >= (last (xs @ 1)))};
  "<="; {[xs]; bool ((last first xs) <= (last (xs @ 1)))});

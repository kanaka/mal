rl:{1 x; read0 0};

/ We have to get a bit crafty with this one
/ as we cannot really do infinite loops, so
/ we make a iterator that never quits and keeps
/ calling a callback
forever: {{.[x; enlist (); show]; x}/ [{1b}; x]};

notempty: {>[count x; 0]};
tail: {(1; -[count x; 1]) sublist x};
init: {(0; -[count x; 1]) sublist x};
skip: {(x; -[count y; x]) sublist y};
take: {(0; x) sublist y};

/ Accumulate cond init fn: let acc = [] in while (cond(init)) { let x = fn(init); acc.append(x[0]); init = x[1]; }; return acc;
apply_and_record: {acc: x @ 0; init: x @ 1; fn: x @ 2; cond: x @ 3; res: fn[init]; (acc, enlist first res; last res; fn; cond)};
accumulate: {[cond;init;fn]; res: apply_and_record/ [{(x @ 3)[x @ 1]}; ((); init; fn; cond)]; (res @ 0; res @ 1)};

strequals: {$[=[count x; count y]; all (x = y); 0b]};

/ 101h 'missing?'
actionordefault: {res:y["a",x][`fn]; $[=[type res; 101h]; y["d."][`fn]; res]};

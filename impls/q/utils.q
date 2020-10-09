rl:{1 x; read0 0};

indebug:{(.Q.def[`debug`_!(0b;0b)].Q.opt .z.x)`debug}

/ We have to get a bit crafty with this one
/ as we cannot really do infinite loops, so
/ we make a iterator that never quits and keeps
/ calling a callback
/ forever: 
forever: $[indebug`; {{x`; x}/ [{1b}; x]}; {{.[x; enlist (); show]; x}/ [{1b}; x]}];

notempty: {>[count x; 0]};
tail: {(1; -[count x; 1]) sublist x};
init: {(0; -[count x; 1]) sublist x};
skip: {(x; -[count y; x]) sublist y};
take: {(0; x) sublist y};

/ Accumulate cond init fn: let acc = [] in while (cond(init)) { let x = fn(init); acc.append(x[0]); init = x[1]; }; return acc;
apply_and_record: {acc: x @ 0; init: x @ 1; fn: x @ 2; cond: x @ 3; res: fn[init]; (acc, enlist first res; last res; fn; cond)};
accumulate: {[cond;init;fn]; res: apply_and_record/ [{(x @ 3)[x @ 1]}; ((); init; fn; cond)]; (res @ 0; res @ 1)};

apply_and_replace: {init: x @ 0; fn: x @ 1; cond: x @ 2; res: fn[init]; (res; fn; cond)}
while_: {[cond;init;fn]; res: apply_and_replace/ [{(x @ 2)[x @ 0]}; (init; fn; cond)]; res};

strequals: {$[=[count x; count y]; all (x = y); 0b]};

/ 101h 'missing?'
actionordefault: {res:y["a",x][`fn]; $[=[type res; 101h]; y["d."][`fn]; res]};

throw: {'(x)};

bool: {[x];($[x; `true; `false]; ())};
number: {[x];(`number; x)};
list: {[x]; (`list; x)};
str: {[x]; (`string; x)};
symbol: {[x]; (`symbol; x)};
vec: {[x]; (`vector; x)};

issymbol:{((first x) ~ `symbol) and strequals[last x; y]};

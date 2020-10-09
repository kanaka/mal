/ This is extremely ugly, but it's the easiest way I can think of
/ to get environments without wiring env's everywhere
/ This will force us out of using `peach', but whatever -
/ performance can suffer so long as I don't.
global_env_storage: ();

allocate_env: {`global_env_storage set global_env_storage, enlist x; count global_env_storage};
get_env: {global_env_storage (x - 1)};
set_env: {global_env_storage[x - 1]:y};

make_env: {allocate_env (`data`outer!(([name:(enlist "d.")] fn:(enlist {[x;y];})); x))};
make_env_from: {allocate_env (`data`outer!(x; y))};
make_env_with_binds: {[outer; binds; exprs];
  env:make_env outer;
  accumulate[{notempty first x}; (binds; exprs); {[xs;env];
    binds:first xs;
    exprs:last xs;
    setall: {[e;n;x]; env_set[e; n; (`list; x)]; ((); ((); ()))};
    setone: {[e;ns;xs]; env_set[e; last first ns; first xs]; ((); (tail ns; tail xs))};
    $[strequals[last first binds; "&"]; setall[env; last first tail binds; exprs]; setone[env; binds; exprs]]
    }[;env]];
  env};

env_set: {[env; n; v]; e:get_env env; set_env[env; `data`outer!(e[`data], ([name: enlist raze "a",n] fn: enlist v); e`outer)]; env};
env_find: {[env; n];
  readfrom: {[env; n]; f:env[`data][raze "a",n][`fn]; $[=[101h; type f]; env_find[env`outer; n]; env]};
  $[env ~ (); env; readfrom[get_env env; n]]};
env_get: {[env; n];
  fenv: env_find[env; n];
  $[fenv ~ (); (`error; throw "'", n, "' not found"); (fenv`data)[raze "a",n][`fn]]};
env_get_nothrow: {[env; n];
  fenv: env_find[env; n];
  $[fenv ~ (); (`error; "'", n, "' not found"); (fenv`data)[raze "a",n][`fn]]};

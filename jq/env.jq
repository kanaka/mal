include "utils";

def childEnv(binds; exprs):
    {
        parent: .,
        fallback: null,
        dirty_atoms: .dirty_atoms,
        environment: [binds, exprs] | transpose | (
            . as $dot | reduce .[] as $item (
                { value: [], seen: false, name: null, idx: 0 };
                if $item[1] != null then
                    if .seen then
                        {
                            value: (.value[1:-1] + (.value|last[1].value += [$item[1]])),
                            seen: true,
                            name: .name
                        }
                    else
                        if $item[0] == "&" then
                            $dot[.idx+1][0] as $name | {
                                value: (.value + [[$name, {kind:"list", value: [$item[1]]}]]),
                                seen: true,
                                name: $name
                            }
                        else
                            {
                                value: (.value + [$item]),
                                seen: false,
                                name: null
                            }
                        end
                    end | (.idx |= .idx + 1)
                else
                    if $item[0] == "&" then
                            $dot[.idx+1][0] as $name | {
                                value: (.value + [[$name, {kind:"list", value: []}]]),
                                seen: true,
                                name: $name
                            }
                    else . end
                end
            )
        ) | .value | map({(.[0]): .[1]}) | add 
    };

def pureChildEnv:
    {
        parent: .,
        environment: {},
        fallback: null,
        dirty_atoms: .dirty_atoms
    };

def rootEnv:
    {
        parent: null,
        fallback: null,
        environment: {},
        dirty_atoms: []
    };

def inform_function(name):
    (.names += [name]) | (.names |= unique);

def inform_function_multi(names):
    . as $dot | reduce names[] as $name(
        $dot;
        inform_function($name)
    );

def env_multiset(keys; value):
    (if value.kind == "function" then
        value | inform_function_multi(keys)
    else
        value
    end) as $value | {
        parent: .parent,
        environment: (
            .environment + (reduce keys[] as $key(.environment; .[$key] |= value))
        ),
        fallback: .fallback,
        dirty_atoms: .dirty_atoms
    };

def env_multiset(env; keys; value):
    env | env_multiset(keys; value);

def env_set($key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function of its names
        $value | inform_function($key)
    else 
        $value
    end) as $value | {
        parent: .parent,
        environment: (.environment + (.environment | .[$key] |= $value)), # merge together, as .environment[key] |= value does not work
        fallback: .fallback,
        dirty_atoms: .dirty_atoms
    };

def env_dump_keys(atoms):
    def _dump0:
        [ .environment // {} | to_entries[] | select(.value.kind != "atom") | .key ];
    def _dump1:
        .environment // {} | keys;
    if . == null then [] else
        if .parent == null then
            (if atoms then _dump1 else _dump0 end + (.fallback | env_dump_keys(atoms))) | unique
        else
            (.parent | env_dump_keys(atoms) + (if atoms then _dump1 else _dump0 end) + (.fallback | env_dump_keys(atoms))) | unique
        end
    end;

def env_dump_keys:
    env_dump_keys(false);

def env_set(env; $key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        $value | (.names += [$key]) | (.names |= unique)
    else 
        $value
    end) as $value | {
        parent: env.parent,
        environment: ((env.environment // jqmal_error("Environment empty in \(env | keys)")) + (env.environment | .[$key] |= $value)), # merge together, as env.environment[key] |= value does not work
        fallback: env.fallback,
        dirty_atoms: env.dirty_atoms
    };

def env_find(env):
    if env.environment[.] == null then
        if env.parent then
            env_find(env.parent) // if env.fallback then env_find(env.fallback) else null end
        else
            null
        end
    else
        env
    end;

def env_setfallback(env; fallback):
    {
        parent: env.parent,
        fallback: fallback,
        environment: env.environment,
        dirty_atoms: env.dirty_atoms
    };

def env_get(env):
    . as $key | env_find(env).environment[$key] // jqmal_error("'\($key)' not found");

def env_get(env; key):
    key | env_get(env);

def env_req(env; key):
    key as $key | key | env_find(env).environment[$key] // null;
    
def addEnv(env):
    {
        expr: .,
        env: env
    };

def addToEnv(env; name; expr):
    {
        expr: expr,
        env: env_set(env; name; expr)
    };


def wrapEnv:
    {
        replEnv: .,
        currentEnv: .,
        isReplEnv: true
    };

def wrapEnv(replEnv):
    {
        replEnv: replEnv,
        currentEnv: .,
        isReplEnv: (replEnv == .) # should we allow separate copies?
    };

def unwrapReplEnv:
    .replEnv;

def unwrapCurrentEnv:
    .currentEnv;

def env_set6(env; key; value):
    if env.isReplEnv then
        env_set(env.currentEnv; key; value) | wrapEnv
    else
        env_set(env.currentEnv; key; value) | wrapEnv(env.replEnv)
    end;

def env_set_(env; key; value):
    if env.currentEnv != null then
        env_set6(env; key; value)
    else
        env_set(env; key; value)
    end;

def addToEnv6(envexp; name):
    envexp.expr as $value
    | envexp.env as $rawEnv
    | (if $rawEnv.isReplEnv then
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv
    else
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv($rawEnv.replEnv)
    end) as $newEnv
    | {
        expr: $value,
        env: $newEnv
    };

def addToEnv(envexp; name):
    if envexp.env.replEnv != null then
        addToEnv6(envexp; name)
    else {
        expr: envexp.expr,
        env: env_set_(envexp.env; name; envexp.expr)
    } end;

def _env_remove_references(refs):
    if . != null then
        {
            environment: (.environment | to_entries | map(select(.key as $key | refs | contains([$key]) | not)) | from_entries),
            parent: (.parent | _env_remove_references(refs)),
            fallback: (.fallback | _env_remove_references(refs)),
            dirty_atoms: (.dirty_atoms | map(select(. as $dot | refs | contains([$dot]) | not)))
        }
    else . end;

def env_remove_references(refs):
    . as $env 
    | if has("replEnv") then
        .currentEnv |= _env_remove_references(refs)
      else
        _env_remove_references(refs)
      end;

# for step2
def lookup(env):
    env.environment[.] //
        if env.parent then
            lookup(env.parent)
        else
            jqmal_error("'\(.)' not found")
        end;
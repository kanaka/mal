include "utils";

def childEnv(binds; exprs):
    {
        parent: .,
        fallback: null,
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
        fallback: null
    };

def rootEnv:
    {
        parent: null,
        fallback: null,
        environment: {}
    };

def inform_function(name):
    (.names += [name]) | (.names |= unique);

def inform_function_multi(names):
    . as $dot | reduce names[] as $name(
        $dot;
        inform_function($name)
    );

def env_multiset(keys; value):
    (if value.kind == "function" then # multiset not allowed on atoms
        value | inform_function_multi(keys)
    else
        value
    end) as $value | {
        parent: .parent,
        environment: (
            .environment + (reduce keys[] as $key(.environment; .[$key] |= value))
        ),
        fallback: .fallback
    };

def env_multiset(env; keys; value):
    env | env_multiset(keys; value);

def env_set($key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        ($value |
        if $value.kind == "atom" then
            # check if the one we have is newer
            env_req(env; key) as $ours |
            if $ours.last_modified > $value.last_modified then
                $ours
            else
                # update modification timestamp
                $value | .last_modified |= now
            end
        else
            .
        end) | inform_function($key)
    else 
        $value
    end) as $value | {
        parent: .parent,
        environment: (.environment + (.environment | .[$key] |= $value)), # merge together, as .environment[key] |= value does not work
        fallback: .fallback
    };

def env_dump_keys:
    def _dump1:
        .environment // {} | keys;
    if . == null then [] else
        if .parent == null then
            (
                _dump1 +
                (.fallback | env_dump_keys)
            )
        else
            (
                _dump1 +
                (.parent | env_dump_keys) +
                (.fallback | env_dump_keys)
            )
        end | unique
    end;

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

def env_get(env):
    . as $key | $key | env_find(env).environment[$key] as $value |
    if $value == null then
        jqmal_error("'\($key)' not found")
    else
        if $value.kind == "atom" then
            $value.identity as $id |
            $key | env_find(env.parent).environment[$key] as $possibly_newer |
            if $possibly_newer.identity == $id and $possibly_newer.last_modified > $value.last_modified then
                $possibly_newer
            else
                $value
            end
        else
            $value
        end
    end;

def env_get(env; key):
    key | env_get(env);

def env_req(env; key):
    key as $key | key | env_find(env).environment[$key] as $value |
    if $value == null then
        null
    else
        if $value.kind == "atom" then
            $value.identity as $id |
            $key | env_find(env.parent).environment[$key] as $possibly_newer |
            if $possibly_newer.identity == $id and $possibly_newer.last_modified > $value.last_modified then
                $possibly_newer
            else
                $value
            end
        else
            $value
        end
    end;

def env_set(env; $key; $value):
    (if $value.kind == "function" then
        # inform the function/atom of its names
        $value | (.names += [$key]) | (.names |= unique)
    else 
        $value
    end) as $value | {
        parent: env.parent,
        environment: ((env.environment // jqmal_error("Environment empty in \(env | keys)")) + (env.environment | .[$key] |= $value)), # merge together, as env.environment[key] |= value does not work
        fallback: env.fallback
    };

def env_setfallback(env; fallback):
    {
        parent: env.parent,
        fallback: fallback,
        environment: env.environment
    };
    
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


def wrapEnv(atoms):
    {
        replEnv: .,
        currentEnv: .,
        atoms: atoms,
        isReplEnv: true
    };

def wrapEnv(replEnv; atoms):
    {
        replEnv: replEnv,
        currentEnv: .,
        atoms: atoms, # id -> value
        isReplEnv: (replEnv == .) # should we allow separate copies?
    };

def unwrapReplEnv:
    .replEnv;

def unwrapCurrentEnv:
    .currentEnv;

def env_set6(env; key; value):
    if env.isReplEnv then
        env_set(env.currentEnv; key; value) | wrapEnv(env.atoms)
    else
        env_set(env.currentEnv; key; value) | wrapEnv(env.replEnv; env.atoms)
    end;

def env_set_(env; key; value):
    if env.currentEnv != null then
        env_set6(env; key; value)
    else
        env_set(env; key; value)
    end;

def addToEnv(envexp; name):
    envexp.expr as $value
    | envexp.env as $rawEnv
    | (if $rawEnv.isReplEnv then
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv($rawEnv.atoms)
    else
        env_set_($rawEnv.currentEnv; name; $value) | wrapEnv($rawEnv.replEnv; $rawEnv.atoms)
    end) as $newEnv
    | {
        expr: $value,
        env: $newEnv
    };

def _env_remove_references(refs):
    if . != null then
        if .environment == null then
            _debug("This one broke the rules, officer: \(.)")
        else
            {
                environment: (.environment | to_entries | map(select(.key as $key | refs | contains([$key]) | not)) | from_entries),
                parent: (.parent | _env_remove_references(refs)),
                fallback: (.fallback | _env_remove_references(refs))
            }
        end
    else . end;

def env_remove_references(refs):
    . as $env 
    | if (refs|length == 0) then
        # optimisation: most functions are purely lexical
        $env
    else 
        if has("replEnv") then
            .currentEnv |= _env_remove_references(refs)
        else
            _env_remove_references(refs)
        end
    end;
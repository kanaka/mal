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

def env_multiset(fn):
    .environment += (reduce fn.names[] as $key(.environment; .[$key] |= fn));

def env_set($key; $value):
    (if $value.kind == "function" or $value.kind == "atom" then
        # inform the function/atom of its names
        ($value |
        if $value.kind == "atom" then
            # check if the one we have is newer
            ($key | env_get(env)) as $ours |
            if $ours.last_modified > $value.last_modified then
                $ours
            else
                # update modification timestamp
                $value | .last_modified |= now
            end
        else
            .
        end) |
        .names += [$key] |
        .names |= unique

    else 
        $value
    end) as $value |
    # merge together, as .environment[$key] |= value does not work
    .environment += (.environment | .[$key] |= $value);

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

# Helper for env_get.
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
    # key -> value or null
    . as $key | env_find(env).environment[$key] |
    if . != null and .kind == "atom" then
        ($key | env_find(env.parent).environment[$key]) as $possibly_newer |
        if $possibly_newer.identity == .identity
            and $possibly_newer.last_modified > .last_modified
        then
            $possibly_newer
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

def env_set_(env; key; value):
    if env.currentEnv != null then
        # Moving the common env_set before the if breaks something. ?
        if env.isReplEnv then
            env_set(env.currentEnv; key; value) | wrapEnv(env.atoms)
        else
            env_set(env.currentEnv; key; value) | wrapEnv(env.replEnv; env.atoms)
        end
    else
        env_set(env; key; value)
    end;

def addToEnv(name):
    # { expr, env } -> { same expr, new env }
    .expr as $value |
    .env |= (
        . as $rawEnv |
        if .isReplEnv then
            env_set_(.currentEnv; name; $value) | wrapEnv($rawEnv.atoms)
        else
            env_set_(.currentEnv; name; $value) | wrapEnv($rawEnv.replEnv; $rawEnv.atoms)
        end);

def _env_remove_references(refs):
    if . != null then
        if .environment == null then
            debug("This one broke the rules, officer: \(.)")
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

include "utils";

def childEnv(binds; exprs):
    {
        parent: .,
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
        environment: {}
    };

def rootEnv:
    {
        parent: null,
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
    (if value.kind == "function" then
        value | inform_function_multi(keys)
    else
        value
    end) as $value | {
        parent: .parent,
        environment: (
            .environment + (reduce keys[] as $key(.environment; .[$key] |= value))
        )
    };

def env_multiset(env; keys; value):
    env | env_multiset(keys; value);

def env_set($key; $value):
    (if $value.kind == "function" then
        # inform the function of its names
        $value | inform_function($key)
    else 
        $value
    end) as $value | {
        parent: .parent,
        environment: (.environment + (.environment | .[$key] |= $value)) # merge together, as .environment[key] |= value does not work
    };

def env_set(env; $key; $value):
    (if $value.kind == "function" then
        # inform the function of its names
        $value | (.names += [$key])
    else 
        $value
    end) as $value | {
        parent: env.parent,
        environment: (env.environment + (env.environment | .[$key] |= $value)) # merge together, as env.environment[key] |= value does not work
    };

def env_find(env):
    if env.environment[.] == null then
        if env.parent then
            env_find(env.parent)
        else
            null
        end
    else
        env
    end;

def env_get(env):
    . as $key | env_find(env).environment[$key] // jqmal_error("Symbol \($key) not found");

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

def addToEnv(envexp; name):
    {
        expr: envexp.expr,
        env: env_set(envexp.env; name; envexp.expr)
    };

# for step2
def lookup(env):
    env.environment[.] //
        if env.parent then
            lookup(env.parent)
        else
            jqmal_error("Symbol \(.) not found")
        end;
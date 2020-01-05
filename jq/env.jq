include "utils";

def childEnv(binds; value):
    {
        parent: .,
        environment: [binds, value] | transpose | map({(.[0]): .[1]}) | from_entries
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

def env_set(key; value):
    {
        parent: .parent,
        environment: (.environment + (.environment | .[key] |= value)) # merge together, as .environment[key] |= value does not work
    };

def env_set(env; key; value):
    {
        parent: env.parent,
        environment: (env.environment + (env.environment | .[key] |= value)) # merge together, as env.environment[key] |= value does not work
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

#include "MAL.h"
#include "Environment.h"
#include "StaticList.h"
#include "Types.h"

#define CHECK_ARGS_IS(expected) \
    checkArgsIs(name.c_str(), expected, \
                  std::distance(argsBegin, argsEnd))

#define CHECK_ARGS_BETWEEN(min, max) \
    checkArgsBetween(name.c_str(), min, max, \
                       std::distance(argsBegin, argsEnd))

#define CHECK_ARGS_AT_LEAST(expected) \
    checkArgsAtLeast(name.c_str(), expected, \
                        std::distance(argsBegin, argsEnd))

static StaticList<malBuiltIn*> handlers;

#define ARG(type, name) type* name = VALUE_CAST(type, *argsBegin++)

#define FUNCNAME(uniq) builtIn ## uniq
#define HRECNAME(uniq) handler ## uniq
#define BUILTIN_DEF(uniq, symbol) \
    static malBuiltIn::ApplyFunc FUNCNAME(uniq); \
    static StaticList<malBuiltIn*>::Node HRECNAME(uniq) \
        (handlers, new malBuiltIn(symbol, FUNCNAME(uniq))); \
    malValuePtr FUNCNAME(uniq)(const String& name, \
        malValueIter argsBegin, malValueIter argsEnd, malEnvPtr env)

#define BUILTIN(symbol)  BUILTIN_DEF(__LINE__, symbol)

#define BUILTIN_INTOP(op, checkDivByZero) \
    BUILTIN(#op) { \
        CHECK_ARGS_IS(2); \
        ARG(malInteger, lhs); \
        ARG(malInteger, rhs); \
        if (checkDivByZero) { \
            ASSERT(rhs->value() != 0, "Division by zero"); \
        } \
        return mal::integer(lhs->value() op rhs->value()); \
    }

BUILTIN_INTOP(+,            false);
BUILTIN_INTOP(/,            true);
BUILTIN_INTOP(*,            false);
BUILTIN_INTOP(%,            true);

BUILTIN("-")
{
    int argCount = CHECK_ARGS_BETWEEN(1, 2);
    ARG(malInteger, lhs);
    if (argCount == 1) {
        return mal::integer(- lhs->value());
    }

    ARG(malInteger, rhs);
    return mal::integer(lhs->value() - rhs->value());
}

BUILTIN("hash-map")
{
    return mal::hash(argsBegin, argsEnd);
}

void installCore(malEnvPtr env) {
    for (auto it = handlers.begin(), end = handlers.end(); it != end; ++it) {
        malBuiltIn* handler = *it;
        env->set(handler->name(), handler);
    }
}

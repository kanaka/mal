#include "Debug.h"
#include "Environment.h"
#include "Types.h"

#include <algorithm>
#include <memory>
#include <typeinfo>

namespace mal {
    malValuePtr builtin(const String& name, malBuiltIn::ApplyFunc handler) {
        return malValuePtr(new malBuiltIn(name, handler));
    };

    malValuePtr falseValue() {
        static malValuePtr c(new malConstant("false"));
        return malValuePtr(c);
    };

    malValuePtr hash(malValueIter argsBegin, malValueIter argsEnd) {
        return malValuePtr(new malHash(argsBegin, argsEnd));
    }

    malValuePtr integer(int value) {
        return malValuePtr(new malInteger(value));
    };

    malValuePtr integer(const String& token) {
        return integer(std::stoi(token));
    };

    malValuePtr keyword(const String& token) {
        return malValuePtr(new malKeyword(token));
    };

    malValuePtr list(malValueVec* items) {
        return malValuePtr(new malList(items));
    };

    malValuePtr list(malValuePtr a) {
        malValueVec* items = new malValueVec(1);
        items->at(0) = a;
        return malValuePtr(new malList(items));
    }

    malValuePtr list(malValuePtr a, malValuePtr b) {
        malValueVec* items = new malValueVec(2);
        items->at(0) = a;
        items->at(1) = b;
        return malValuePtr(new malList(items));
    }

    malValuePtr list(malValuePtr a, malValuePtr b, malValuePtr c) {
        malValueVec* items = new malValueVec(3);
        items->at(0) = a;
        items->at(1) = b;
        items->at(2) = c;
        return malValuePtr(new malList(items));
    }

    malValuePtr nilValue() {
        static malValuePtr c(new malConstant("nil"));
        return malValuePtr(c);
    };

    malValuePtr string(const String& token) {
        return malValuePtr(new malString(token));
    }

    malValuePtr symbol(const String& token) {
        return malValuePtr(new malSymbol(token));
    };

    malValuePtr trueValue() {
        static malValuePtr c(new malConstant("true"));
        return malValuePtr(c);
    };

    malValuePtr vector(malValueVec* items) {
        return malValuePtr(new malVector(items));
    };
};

malValuePtr malBuiltIn::apply(malValueIter argsBegin,
                              malValueIter argsEnd,
                              malEnv& env) const
{
    return m_handler(m_name, argsBegin, argsEnd, env);
}

static String makeHashKey(malValuePtr key)
{
    if (const malString* skey = DYNAMIC_CAST(malString, key)) {
        return skey->print(true);
    }
    else if (const malKeyword* kkey = DYNAMIC_CAST(malKeyword, key)) {
        return kkey->print(true);
    }
    ASSERT(false, "%s is not a string or keyword", key->print(true).c_str());
}

static malHash::Map addToMap(malHash::Map& map,
    malValueIter argsBegin, malValueIter argsEnd)
{
    // This is intended to be called with pre-evaluated arguments.
    for (auto it = argsBegin; it != argsEnd; ++it) {
        String key = makeHashKey(*it++);
        map[key] = *it;
    }

    return map;
}

static malHash::Map createMap(malValueIter argsBegin, malValueIter argsEnd)
{
    ASSERT(std::distance(argsBegin, argsEnd) % 2 == 0,
            "hash-map requires an even-sized list");

    malHash::Map map;
    return addToMap(map, argsBegin, argsEnd);
}

malHash::malHash(malValueIter argsBegin, malValueIter argsEnd)
: m_map(createMap(argsBegin, argsEnd))
{

}

String malHash::print(bool readably) const
{
    String s = "{";

    auto it = m_map.begin(), end = m_map.end();
    if (it != end) {
        s += it->first + " " + it->second->print(readably);
        ++it;
    }
    for ( ; it != end; ++it) {
        s += " " + it->first + " " + it->second->print(readably);
    }

    return s + "}";
}

malValuePtr malList::eval(malEnv& env)
{
    if (count() == 0) {
        return malValuePtr(this);
    }

    std::unique_ptr<malValueVec> items(evalItems(env));
    auto it = items->begin();
    malValuePtr op = *it;
    return APPLY(op, ++it, items->end(), env);
}

String malList::print(bool readably) const
{
    return '(' + malSequence::print(readably) + ')';
}

malValuePtr malValue::eval(malEnv& env)
{
    // Default case of eval is just to return the object itself.
    return malValuePtr(this);
}

malSequence::malSequence(malValueVec* items)
: m_items(items)
{

}

malSequence::~malSequence()
{
    delete m_items;
}

malValueVec* malSequence::evalItems(malEnv& env) const
{
    malValueVec* items = new malValueVec;;
    items->reserve(count());
    for (auto it = m_items->begin(), end = m_items->end(); it != end; ++it) {
        items->push_back(EVAL(*it, env));
    }
    return items;
}

String malSequence::print(bool readably) const
{
    String str;
    auto end = m_items->cend();
    auto it = m_items->cbegin();
    if (it != end) {
        str += (*it)->print(readably);
        ++it;
    }
    for ( ; it != end; ++it) {
        str += " ";
        str += (*it)->print(readably);
    }
    return str;
}

String malString::escapedValue() const
{
    return escape(value());
}

String malString::print(bool readably) const
{
    return readably ? escapedValue() : value();
}

malValuePtr malSymbol::eval(malEnv& env)
{
    return env.get(value());
}

malValuePtr malVector::eval(malEnv& env)
{
    return mal::vector(evalItems(env));
}

String malVector::print(bool readably) const
{
    return '[' + malSequence::print(readably) + ']';
}

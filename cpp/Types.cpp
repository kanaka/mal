#include "Debug.h"
#include "Environment.h"
#include "Types.h"

#include <algorithm>
#include <memory>
#include <typeinfo>

namespace mal {
    malValuePtr boolean(bool value) {
        return value ? trueValue() : falseValue();
    }

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

    malValuePtr lambda(const StringVec& bindings,
                       malValuePtr body, malEnvPtr env) {
        return malValuePtr(new malLambda(bindings, body, env));
    }

    malValuePtr list(malValueVec* items) {
        return malValuePtr(new malList(items));
    };

    malValuePtr list(malValueIter begin, malValueIter end) {
        return malValuePtr(new malList(begin, end));
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
                              malEnvPtr env) const
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

bool malHash::doIsEqualTo(const malValue* rhs) const
{
    const malHash::Map& r_map = static_cast<const malHash*>(rhs)->m_map;
    if (m_map.size() != r_map.size()) {
        return false;
    }

    for (auto it0 = m_map.begin(), end0 = m_map.end(), it1 = r_map.begin();
         it0 != end0; ++it0, ++it1) {

        if (it0->first != it1->first) {
            return false;
        }
        if (!it0->second->isEqualTo(it1->second.ptr())) {
            return false;
        }
    }
    return true;
}

malLambda::malLambda(const StringVec& bindings,
                     malValuePtr body, malEnvPtr env)
: m_bindings(bindings)
, m_body(body)
, m_env(env)
{

}

malValuePtr malLambda::apply(malValueIter argsBegin,
                             malValueIter argsEnd,
                             malEnvPtr) const
{
    return EVAL(m_body, makeEnv(argsBegin, argsEnd));
}

malEnvPtr malLambda::makeEnv(malValueIter argsBegin, malValueIter argsEnd) const
{
    return malEnvPtr(new malEnv(m_env, m_bindings, argsBegin, argsEnd));
}

malValuePtr malList::eval(malEnvPtr env)
{
    // Note, this isn't actually called since the TCO updates, but
    // is required for the earlier steps, so don't get rid of it.
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

malValuePtr malValue::eval(malEnvPtr env)
{
    // Default case of eval is just to return the object itself.
    return malValuePtr(this);
}

bool malValue::isEqualTo(const malValue* rhs) const
{
    // Special-case. Vectors and Lists can be compared.
    bool matchingTypes = (typeid(*this) == typeid(*rhs)) ||
        (dynamic_cast<const malSequence*>(this) &&
         dynamic_cast<const malSequence*>(rhs));

    return matchingTypes && doIsEqualTo(rhs);
}

bool malValue::isTrue() const
{
    return (this != mal::falseValue().ptr())
        && (this != mal::nilValue().ptr());
}

malSequence::malSequence(malValueVec* items)
: m_items(items)
{

}

malSequence::malSequence(malValueIter begin, malValueIter end)
: m_items(new malValueVec(begin, end))
{

}

malSequence::~malSequence()
{
    delete m_items;
}

bool malSequence::doIsEqualTo(const malValue* rhs) const
{
    const malSequence* rhsSeq = static_cast<const malSequence*>(rhs);
    if (count() != rhsSeq->count()) {
        return false;
    }

    for (malValueIter it0 = m_items->begin(),
                      it1 = rhsSeq->begin(),
                      end = m_items->end(); it0 != end; ++it0, ++it1) {

        if (! (*it0)->isEqualTo((*it1).ptr())) {
            return false;
        }
    }
    return true;
}

malValueVec* malSequence::evalItems(malEnvPtr env) const
{
    malValueVec* items = new malValueVec;;
    items->reserve(count());
    for (auto it = m_items->begin(), end = m_items->end(); it != end; ++it) {
        items->push_back(EVAL(*it, env));
    }
    return items;
}

malValuePtr malSequence::first() const
{
    return count() == 0 ? mal::nilValue() : item(0);
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

malValuePtr malSequence::rest() const
{
    malValueIter start = (count() > 0) ? begin() + 1 : end();
    return mal::list(start, end());
}

String malString::escapedValue() const
{
    return escape(value());
}

String malString::print(bool readably) const
{
    return readably ? escapedValue() : value();
}

malValuePtr malSymbol::eval(malEnvPtr env)
{
    return env->get(value());
}

malValuePtr malVector::eval(malEnvPtr env)
{
    return mal::vector(evalItems(env));
}

String malVector::print(bool readably) const
{
    return '[' + malSequence::print(readably) + ']';
}

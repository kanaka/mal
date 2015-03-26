#include "Debug.h"
#include "Types.h"

#include <algorithm>
#include <memory>
#include <typeinfo>

namespace mal {
    malValuePtr falseValue() {
        static malValuePtr c(new malConstant("false"));
        return malValuePtr(c);
    };

    malValuePtr hash(malValueVec* items) {
        return malValuePtr(new malHash(items));
    };

    malValuePtr integer(int value) {
        return malValuePtr(new malInteger(value));
    }

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

static String makeHashKey(malValuePtr key)
{
    if (malString* skey = dynamic_cast<malString*>(key.ptr())) {
        return skey->print(true);
    }
    else if (malKeyword* kkey = dynamic_cast<malKeyword*>(key.ptr())) {
        return kkey->print(true);
    }
    ASSERT(false, "%s is not a string or keyword", key->print(true).c_str());
}

static malHash::Map createMap(malValueVec* items)
{
    int itemCount = items->size();
    ASSERT(itemCount % 2 == 0, "hash-map requires an even-sized list");

    malHash::Map map;
    for (int i = 0; i < itemCount; i += 2) {
        map[makeHashKey(items->at(i))] = items->at(i+1);
    }
    return map;
}

malHash::malHash(malValueVec* items)
: m_map(createMap(items))
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

String malList::print(bool readably) const
{
    return '(' + malSequence::print(readably) + ')';
}

malSequence::malSequence(malValueVec* items)
: m_items(items)
{

}

malSequence::~malSequence()
{
    delete m_items;
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

String malVector::print(bool readably) const
{
    return '[' + malSequence::print(readably) + ']';
}

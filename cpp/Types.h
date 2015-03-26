#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include "MAL.h"

#include <exception>
#include <map>

#define ARRAY_SIZE(a)   (sizeof(a)/(sizeof(*(a))))

class malEmptyInputException : public std::exception { };

class malValue : public RefCounted {
public:
    malValue() {
        TRACE_OBJECT("Creating malValue %p\n", this);
    }
    virtual ~malValue() {
        TRACE_OBJECT("Destroying malValue %p\n", this);
    }

    virtual String print(bool readably) const = 0;
};

class malConstant : public malValue {
public:
    malConstant(String name) : m_name(name) { }

    virtual String print(bool readably) const { return m_name; }

private:
    const String m_name;
};

class malInteger : public malValue {
public:
    malInteger(int value) : m_value(value) { }

    virtual String print(bool readably) const {
        return std::to_string(m_value);
    }

private:
    const int m_value;
};

class malStringBase : public malValue {
public:
    malStringBase(const String& token)
        : m_value(token) { }

    virtual String print(bool readably) const { return m_value; }

    String value() const { return m_value; }

private:
    const String m_value;
};

class malString : public malStringBase {
public:
    malString(const String& token)
        : malStringBase(token) { }

    virtual String print(bool readably) const;

    String escapedValue() const;
};

class malKeyword : public malStringBase {
public:
    malKeyword(const String& token)
        : malStringBase(token) { }
};

class malSymbol : public malStringBase {
public:
    malSymbol(const String& token)
        : malStringBase(token) { }
};

class malSequence : public malValue {
public:
    malSequence(malValueVec* items);
    virtual ~malSequence();

    virtual String print(bool readably) const;

private:
    malValueVec* const m_items;
};

class malList : public malSequence {
public:
    malList(malValueVec* items) : malSequence(items) { }

    virtual String print(bool readably) const;
};

class malVector : public malSequence {
public:
    malVector(malValueVec* items) : malSequence(items) { }

    virtual String print(bool readably) const;
};

class malHash : public malValue {
public:
    typedef std::map<String, malValuePtr> Map;

    malHash(malValueVec* items);

    virtual String print(bool readably) const;

private:
    const Map m_map;
};

namespace mal {
    malValuePtr falseValue();
    malValuePtr hash(malValueVec* items);
    malValuePtr integer(int value);
    malValuePtr integer(const String& token);
    malValuePtr keyword(const String& token);
    malValuePtr list(malValueVec* items);
    malValuePtr list(malValuePtr a);
    malValuePtr list(malValuePtr a, malValuePtr b);
    malValuePtr list(malValuePtr a, malValuePtr b, malValuePtr c);
    malValuePtr nilValue();
    malValuePtr string(const String& token);
    malValuePtr symbol(const String& token);
    malValuePtr trueValue();
    malValuePtr vector(malValueVec* items);
};

#endif // INCLUDE_TYPES_H

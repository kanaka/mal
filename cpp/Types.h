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

    bool isTrue() const;

    bool isEqualTo(const malValue* rhs) const;

    virtual malValuePtr eval(malEnvPtr env);

    virtual String print(bool readably) const = 0;

protected:
    virtual bool doIsEqualTo(const malValue* rhs) const = 0;
};

template<class T>
T* value_cast(malValuePtr obj, const char* typeName) {
    T* dest = dynamic_cast<T*>(obj.ptr());
    ASSERT(dest != NULL, "%s is not a %s", obj->print(true).c_str(), typeName);
    return dest;
}

#define VALUE_CAST(Type, Value)    value_cast<Type>(Value, #Type)
#define DYNAMIC_CAST(Type, Value)  (dynamic_cast<Type*>((Value).ptr()))
#define STATIC_CAST(Type, Value)   (static_cast<Type*>((Value).ptr()))

class malConstant : public malValue {
public:
    malConstant(String name) : m_name(name) { }

    virtual String print(bool readably) const { return m_name; }

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return this == rhs; // these are singletons
    }

private:
    const String m_name;
};

class malInteger : public malValue {
public:
    malInteger(int value) : m_value(value) { }

    virtual String print(bool readably) const {
        return std::to_string(m_value);
    }

    int value() const { return m_value; }

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return m_value == static_cast<const malInteger*>(rhs)->m_value;
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

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return value() == static_cast<const malString*>(rhs)->value();
    }
};

class malKeyword : public malStringBase {
public:
    malKeyword(const String& token)
        : malStringBase(token) { }

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return value() == static_cast<const malKeyword*>(rhs)->value();
    }
};

class malSymbol : public malStringBase {
public:
    malSymbol(const String& token)
        : malStringBase(token) { }

    virtual malValuePtr eval(malEnvPtr env);

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return value() == static_cast<const malSymbol*>(rhs)->value();
    }
};

class malSequence : public malValue {
public:
    malSequence(malValueVec* items);
    malSequence(malValueIter begin, malValueIter end);
    virtual ~malSequence();

    virtual String print(bool readably) const;

    malValueVec* evalItems(malEnvPtr env) const;
    int count() const { return m_items->size(); }
    bool isEmpty() const { return m_items->empty(); }
    malValuePtr item(int index) const { return (*m_items)[index]; }

    malValueIter begin() const { return m_items->begin(); }
    malValueIter end()   const { return m_items->end(); }

    virtual bool doIsEqualTo(const malValue* rhs) const;

    malValuePtr first() const;
    virtual malValuePtr rest() const;

private:
    malValueVec* const m_items;
};

class malList : public malSequence {
public:
    malList(malValueVec* items) : malSequence(items) { }
    malList(malValueIter begin, malValueIter end)
        : malSequence(begin, end) { }

    virtual String print(bool readably) const;
    virtual malValuePtr eval(malEnvPtr env);
};

class malVector : public malSequence {
public:
    malVector(malValueVec* items) : malSequence(items) { }

    virtual malValuePtr eval(malEnvPtr env);
    virtual String print(bool readably) const;
};

class malApplicable : public malValue {
public:
    malApplicable() { }

    virtual malValuePtr apply(malValueIter argsBegin,
                               malValueIter argsEnd,
                               malEnvPtr env) const = 0;
};

class malHash : public malValue {
public:
    typedef std::map<String, malValuePtr> Map;

    malHash(malValueIter argsBegin, malValueIter argsEnd);

    virtual String print(bool readably) const;

    virtual bool doIsEqualTo(const malValue* rhs) const;

private:
    const Map m_map;
};

class malBuiltIn : public malApplicable {
public:
    typedef malValuePtr (ApplyFunc)(const String& name,
                                    malValueIter argsBegin,
                                    malValueIter argsEnd,
                                    malEnvPtr env);

    malBuiltIn(const String& name, ApplyFunc* handler)
    : m_name(name), m_handler(handler) { }

    virtual malValuePtr apply(malValueIter argsBegin,
                              malValueIter argsEnd,
                              malEnvPtr env) const;

    virtual String print(bool readably) const {
        return STRF("#builtin-function(%s)", m_name.c_str());
    }

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return this == rhs; // these are singletons
    }

    String name() const { return m_name; }

private:
    const String m_name;
    ApplyFunc* m_handler;
};

class malLambda : public malApplicable {
public:
    malLambda(const StringVec& bindings, malValuePtr body, malEnvPtr env);

    virtual malValuePtr apply(malValueIter argsBegin,
                              malValueIter argsEnd,
                              malEnvPtr env) const;

    malValuePtr getBody() const { return m_body; }
    malEnvPtr makeEnv(malValueIter argsBegin, malValueIter argsEnd) const;

    virtual bool doIsEqualTo(const malValue* rhs) const {
        return this == rhs; // do we need to do a deep inspection?
    }

    virtual String print(bool readably) const {
        return STRF("#user-function(%p)", this);
    }

private:
    const StringVec   m_bindings;
    const malValuePtr m_body;
    const malEnvPtr   m_env;
};

namespace mal {
    malValuePtr boolean(bool value);
    malValuePtr builtin(const String& name, malBuiltIn::ApplyFunc handler);
    malValuePtr falseValue();
    malValuePtr hash(malValueIter argsBegin, malValueIter argsEnd);
    malValuePtr integer(int value);
    malValuePtr integer(const String& token);
    malValuePtr keyword(const String& token);
    malValuePtr lambda(const StringVec&, malValuePtr, malEnvPtr);
    malValuePtr list(malValueVec* items);
    malValuePtr list(malValueIter begin, malValueIter end);
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

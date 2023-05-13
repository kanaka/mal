#ifndef ENV_H
#define ENV_H

#include <functional>
#include <typeinfo>
#include <cstdarg>
#include "types.h"

enum Env_Element_Type {ENV_SYMBOL, ENV_PRIMITIVE, ENV_PROCEDURE};

class Env_Symbol
{
public:
    Env_Symbol(MalSymbol s, MalPtr v = nullptr): sym(s), val(v) {};
    virtual Env_Element_Type type() {return ENV_SYMBOL;};
    virtual MalSymbol symbol() {return sym;};
    virtual MalPtr value() {return val;};
protected:
    MalSymbol sym;
    MalPtr val;
};


class Env_Primitive: public Env_Symbol
{
public:
    Env_Primitive(MalSymbol s, std::function<TokenVector(TokenVector)> f, int a): Env_Symbol(s), fn(f), arity(a) {};
    virtual Env_Element_Type type() {return ENV_PRIMITIVE;};
    TokenVector apply(TokenVector& args);
protected:
    std::function<TokenVector(TokenVector)> fn;
    int arity;
};


class Env_Procedure: public Env_Symbol
{
public:
    Env_Procedure(MalSymbol s, TokenVector& f, int a): Env_Symbol(s), fn(f), arity(a) {};
    virtual Env_Element_Type type() {return ENV_PROCEDURE;};
    TokenVector apply(TokenVector& args);
protected:
    TokenVector fn;
    int arity;
};


typedef std::shared_ptr<Env_Symbol> EnvPtr;


class Environment
{
public:
    Environment() {};
    void append(EnvPtr element);
    EnvPtr find(MalPtr p);

private:
    std::vector<EnvPtr> env;
};


extern Environment global_env;

void init_global_environment();


#endif
#ifndef ENV_H
#define ENV_H

/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


#include <functional>
#include <typeinfo>
#include <cstdarg>
#include <gmpxx.h>
#include "types.h"


enum Env_Element_Type {ENV_SYMBOL, ENV_PRIMITIVE, ENV_PROCEDURE};



class Env_Symbol
{
public:
    Env_Symbol(MalPtr s, MalPtr v = nullptr);
    virtual Env_Element_Type type() {return ENV_SYMBOL;};
    virtual MalSymbol symbol() {return sym;};
    virtual MalPtr value() {return val;};
    virtual TokenVector apply(TokenVector& args) {return args;};
protected:
    MalSymbol sym;
    MalPtr val;
};


class Env_Primitive: public Env_Symbol
{
public:
    Env_Primitive(MalPtr s, std::function<TokenVector(TokenVector&)>& f, int a): Env_Symbol(s), fn(f), arity(a) {};
    virtual Env_Element_Type type() {return ENV_PRIMITIVE;};
    virtual TokenVector apply(TokenVector& args);
protected:
    std::function<TokenVector(TokenVector&)> fn;
    int arity;
};


class Env_Procedure: public Env_Symbol
{
public:
    Env_Procedure(MalPtr s, TokenVector& f, int a): Env_Symbol(s), fn(f), arity(a) {};
    virtual Env_Element_Type type() {return ENV_PROCEDURE;};
    virtual TokenVector apply(TokenVector& args);
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
    size_t size() const {return env.size();};
    std::vector<EnvPtr> elements() {return env;};
    std::string element_names();

private:
    std::vector<EnvPtr> env;
};


extern Environment global_env;

void init_global_environment();


#endif
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


class Env_Symbol;

typedef std::shared_ptr<Env_Symbol> EnvPtr;


class Environment
{
public:
    Environment(std::shared_ptr<Environment> p = nullptr): parent(p) {};
    void append(EnvPtr element);
    bool find(MalPtr p);
    EnvPtr get(MalPtr p);
    size_t size() const {return env.size();};
    std::vector<EnvPtr> elements() {return env;};
    std::string element_names();

private:
    std::shared_ptr<Environment> parent;
    std::vector<EnvPtr> env;
};



enum Env_Element_Type {ENV_SYMBOL, ENV_PRIMITIVE, ENV_PROCEDURE};



class Env_Symbol
{
public:
    Env_Symbol(MalPtr s, MalPtr v = nullptr);
    virtual Env_Element_Type type() {return ENV_SYMBOL;};
    virtual MalSymbol symbol() {return sym;};
    virtual MalPtr value() {return val;};
    virtual int arity() {return n_ary;};
    virtual TokenVector apply(TokenVector& args) {return args;};
protected:
    MalSymbol sym;
    MalPtr val;
    int n_ary;
};


class Env_Primitive: public Env_Symbol
{
public:
    Env_Primitive(MalPtr s, std::function<TokenVector(TokenVector)>& f, int a): Env_Symbol(s), fn(f) {n_ary = a;};
    virtual Env_Element_Type type() {return ENV_PRIMITIVE;};
    virtual TokenVector apply(TokenVector& args);

protected:
    std::function<TokenVector(TokenVector)> fn;
};


class Env_Procedure: public Env_Symbol
{
public:
    Env_Procedure(MalPtr s, TokenVector& f, int a): Env_Symbol(s), fn(f) {n_ary = a;};
    virtual Env_Element_Type type() {return ENV_PROCEDURE;};
    virtual TokenVector apply(TokenVector& args);

protected:
    TokenVector fn;
};



extern Environment repl_env;

void init_global_environment();


#endif
#ifndef MAL_TYPES_H
#define MAL_TYPES_H


/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


#include <complex>
#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <gmpxx.h>


class Environment;
class Env_Symbol;

typedef std::shared_ptr<Env_Symbol> EnvPtr;


enum MalTypeName
{
    MAL_TYPE, MAL_ATOM, MAL_SYMBOL, MAL_KEYWORD, MAL_REST_ARG,
    MAL_STRING, MAL_CHAR, MAL_BOOLEAN,
    MAL_LIST, MAL_NULL, MAL_NIL, MAL_PRINT_NIL, MAL_VECTOR, MAL_HASHMAP,
    MAL_PERIOD, MAL_COMMA,
    MAL_READER_MACRO, MAL_DEREF, MAL_UNQUOTE, MAL_SPLICE_UNQUOTE,
    MAL_QUOTE, MAL_QUASIQUOTE, MAL_META,
    MAL_NUMBER, MAL_SYSTEM_INTEGER, MAL_BINARY, MAL_OCTAL, MAL_HEX,
    MAL_INTEGER, MAL_FRACTIONAL, MAL_RATIONAL, MAL_COMPLEX,
    MAL_PROCEDURE, MAL_PRIMITIVE
};



class MalType;

typedef std::shared_ptr<MalType> MalPtr;



class TokenVector
{
public:
    TokenVector(): current_token(0) {tokens.reserve(65535);};
    TokenVector(const TokenVector& t);
    const TokenVector& operator=(const TokenVector& t);
    size_t size() const {return tokens.size();};
    size_t capacity() const {return tokens.capacity();};
    size_t append(MalPtr token);
    size_t append(const TokenVector& t);
    std::string values();
    std::string values_remainder();
    std::string types();
    MalPtr next();
    MalPtr peek();
    MalPtr car() {return tokens[0];};
    TokenVector cdr();
    TokenVector rest();
    void clear() {tokens.clear(); current_token = 0;};
    bool empty() {return tokens.size() == 0;};
    MalPtr operator[](unsigned int i);

private:
    std::vector<MalPtr> tokens;
    unsigned int current_token;
};


inline bool is_mal_numeric(MalTypeName type)
{
    return (type == MAL_NUMBER
            || type == MAL_INTEGER
            || type == MAL_SYSTEM_INTEGER
            || type == MAL_BINARY
            || type == MAL_OCTAL
            || type == MAL_HEX
            || type == MAL_FRACTIONAL
            || type == MAL_RATIONAL
            || type == MAL_COMPLEX);
}

inline bool is_mal_container(MalTypeName type)
{
    return (type == MAL_LIST
            || type == MAL_NULL
            || type == MAL_VECTOR
            || type == MAL_HASHMAP);
}


inline bool is_mal_reader_macro(MalTypeName type)
{
    return (type == MAL_READER_MACRO
            || type == MAL_QUOTE
            || type == MAL_QUASIQUOTE
            || type == MAL_UNQUOTE
            || type == MAL_SPLICE_UNQUOTE
            || type == MAL_DEREF
            || type == MAL_META
            );
}



typedef std::unordered_map<std::string, std::shared_ptr<MalType> > HashMapInternal;


typedef std::function<TokenVector(TokenVector)> Procedure;
typedef std::shared_ptr<Procedure> ProcedurePtr;



class MalType
{
public:
    MalType(std::string const r):repr(r) {};
    virtual std::string value() {return repr;};
    virtual MalTypeName type() {return MAL_TYPE;};
    virtual TokenVector raw_value() {TokenVector t, &r = t; return r;};
protected:
    std::string repr;
};


class MalPeriod: public MalType
{
public:
    MalPeriod(): MalType(".") {};
    virtual MalTypeName type() {return MAL_PERIOD;};
};

class MalReaderMacro: public MalType
{
public:
    MalReaderMacro(const TokenVector& l): MalType("Reader Macro"), list(l) {};
    virtual MalTypeName type() {return MAL_READER_MACRO;};
    virtual std::string value() {return list.values();};
    virtual TokenVector raw_value() {return list;};
    MalPtr operator[](unsigned int i);
protected:
    TokenVector list;
};


class MalDeref: public MalReaderMacro
{
public:
    MalDeref(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_DEREF;};
    virtual std::string value() {return "(deref " + list.values() + ')';};
};


class MalUnquote: public MalReaderMacro
{
public:
    MalUnquote(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_UNQUOTE;};
    virtual std::string value() {return "(unquote " + list.values() + ')';};
};


class MalSpliceUnquote: public MalReaderMacro
{
public:
    MalSpliceUnquote(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_SPLICE_UNQUOTE;};
    virtual std::string value() {return "(splice-unquote " + list.values() + ')';};
};

class MalQuote: public MalReaderMacro
{
public:
    MalQuote(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_QUOTE;};
    virtual std::string value() {return "(quote " + list.values() + ")";};
};


class MalQuasiquote: public MalReaderMacro
{
public:
    MalQuasiquote(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_QUASIQUOTE;};
    virtual std::string value() {return "(quasiquote " + list.values() + ')';};
};

class MalMeta: public MalReaderMacro
{
public:
    MalMeta(const TokenVector& seq, TokenVector& arg): MalReaderMacro(arg), sequence(seq) {};
    virtual MalTypeName type() {return MAL_META;};
    virtual TokenVector meta_target() {return sequence;};
    virtual TokenVector meta_arguments() {return list;};
    virtual std::string value() {return "(with-meta " + sequence.values() + " " + list.values() + ')';};
private:
    TokenVector sequence;
};


class MalBoolean: public MalType
{
public:
    MalBoolean(std::string r): MalType(r) {};
    virtual MalTypeName type() {return MAL_BOOLEAN;};
};


class MalList: public MalType
{
public:
    MalList() = delete;
    MalList(std::string const r) = delete;
    MalList(const TokenVector& l);
    virtual std::string value() {return "(" + list.values() + ")";};
    virtual MalTypeName type() {return MAL_LIST;};
    virtual TokenVector raw_value() {return list;};
private:
    TokenVector list;
};



class MalNull: public MalType
{
public:
    MalNull(): MalType("()") {};
    MalNull(std::string n): MalType(n) {};
    virtual MalTypeName type() {return MAL_NULL;};
};

class MalNil: public MalNull
{
public:
    MalNil(): MalNull("nil") {};
    virtual MalTypeName type() {return MAL_NIL;};
};

class MalPrintNil: public MalNull
{
public:
    MalPrintNil(): MalNull("nil") {};
    virtual MalTypeName type() {return MAL_PRINT_NIL;};
};

class MalVector: public MalType
{
public:
    MalVector() = delete;
    MalVector(std::string const r) = delete;
    MalVector(const TokenVector& v);
    virtual std::string value() {return "[" + vec.values() + "]";};
    virtual MalTypeName type() {return MAL_VECTOR;};
    virtual TokenVector raw_value() {return vec;};
private:
    TokenVector vec;
};


class MalHashmap: public MalType
{
public:
    MalHashmap(TokenVector hm);
    MalHashmap(std::unordered_map<std::string, std::shared_ptr<MalType> > hm);
    virtual MalTypeName type() {return MAL_HASHMAP;};
    virtual std::string value();
    HashMapInternal internal_map() {return hashmap;};
private:
    HashMapInternal hashmap;
};


class MalAtom: public MalType
{
public:
    MalAtom(std::string r): MalType(r) {};
    virtual MalTypeName type() {return MAL_ATOM;};
};


class MalSymbol: public MalAtom
{
public:
    MalSymbol(): MalAtom("") {};
    MalSymbol(std::string r): MalAtom(r) {};
    virtual MalTypeName type() {return MAL_SYMBOL;};
};


class MalKeyword: public MalSymbol
{
public:
    MalKeyword(std::string r): MalSymbol(r) {};
    virtual MalTypeName type() {return MAL_KEYWORD;};
};


class MalRestArg: public MalSymbol
{
public:
    MalRestArg(): MalSymbol("&") {};
    virtual MalTypeName type() {return MAL_REST_ARG;};
};


class MalChar: public MalAtom
{
public:
    MalChar(std::string r): MalAtom(r) {};
    virtual MalTypeName type() {return MAL_CHAR;};
};


class MalString: public MalAtom
{
public:
    MalString(std::string r): MalAtom(r) {};
    virtual MalTypeName type() {return MAL_STRING;};
    virtual std::string value() {return repr;};
};


class MalNumber: public MalAtom
{
public:
    MalNumber(std::string r): MalAtom(r) {};
    virtual MalTypeName type() {return MAL_NUMBER;};
};


class MalInteger: public MalNumber
{
public:
    MalInteger(std::string r): MalNumber(r), internal_value(r) {};
    MalInteger(mpz_class i): MalNumber(i.get_str()), internal_value(i) {};
    virtual std::string value() {return internal_value.get_str();};
    virtual MalTypeName type() {return MAL_INTEGER;};
    virtual mpz_class numeric_value() {return internal_value;};
protected:
    mpz_class internal_value;
};


class MalSystemInteger: public MalNumber
{
public:
    MalSystemInteger(std::string r): MalNumber(r), internal_value(stoll(r)) {};
    MalSystemInteger(unsigned long long int i): MalNumber(std::to_string(i)), internal_value(i) {};
    virtual std::string value() {return std::to_string(internal_value);};
    virtual MalTypeName type() {return MAL_SYSTEM_INTEGER;};
    virtual unsigned long long int numeric_value() {return internal_value;};
protected:
    unsigned long long int internal_value;
};


class MalHex: public MalSystemInteger
{
public:
    MalHex(std::string r): MalSystemInteger(r) { internal_value = stoll(r, nullptr, 16); };
    virtual MalTypeName type() {return MAL_HEX;};
};


class MalBinary: public MalSystemInteger
{
public:
    MalBinary(std::string r): MalSystemInteger(r) { internal_value = stoll(r, nullptr, 16); };
    virtual MalTypeName type() {return MAL_BINARY;};
};


class MalOctal: public MalSystemInteger
{
public:
    MalOctal(std::string r): MalSystemInteger(r) {internal_value = stoll(r, nullptr, 8); };
    virtual MalTypeName type() {return MAL_OCTAL;};
};


class MalFractional: public MalNumber
{
public:
    MalFractional(std::string r): MalNumber(r), internal_value(r) {};
    MalFractional(mpf_class f);
    virtual MalTypeName type() {return MAL_FRACTIONAL;};
    virtual std::string value();
    virtual mpf_class numeric_value() { return internal_value;};
protected:
    mpf_class internal_value;
};


class MalRational: public MalNumber
{
public:
    MalRational(std::string r): MalNumber(r), internal_value(r) {};
    MalRational(mpq_class r): MalNumber(r.get_str()), internal_value(r) {};
    virtual std::string value() {return internal_value.get_str();};
    virtual MalTypeName type() {return MAL_RATIONAL;};
    virtual mpq_class numeric_value() { return internal_value;};
protected:
    mpq_class internal_value;
};


class MalComplex: public MalNumber
{
public:
    MalComplex(std::string r);
    MalComplex(std::complex<mpf_class> c);
    virtual std::string value();
    virtual MalTypeName type() {return MAL_COMPLEX;};
    virtual std::complex<mpf_class> numeric_value() { return internal_value;};
protected:
    std::complex<mpf_class> internal_value;
};


class MalProcedure: public MalSymbol
{
public:
    MalProcedure(Procedure p, int a): MalSymbol("<function>"), procedure(p), arity(a) {};
    virtual MalTypeName type() {return MAL_PROCEDURE;};
    virtual TokenVector raw_value();
    virtual TokenVector fn(TokenVector args) {return procedure(args);};
protected:
    Procedure procedure;
    int arity;
};

class MalPrimitive: public MalSymbol
{
public:
    MalPrimitive(std::string r, int a): MalSymbol(r), arity(a) {};
    virtual MalTypeName type() {return MAL_PRIMITIVE;};
    virtual std::string value() {return "<primitive procedure (" + repr + " " + std::to_string(arity) + ")>";};
    virtual TokenVector raw_value();
protected:
    int arity;
};


#endif
#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <memory>
#include <string>
#include <vector>
#include <map>


enum MalTypeName
{
    MAL_TYPE, MAL_ATOM, MAL_SYMBOL, MAL_KEYWORD,
    MAL_STRING, MAL_CHAR, MAL_BOOLEAN,
    MAL_LIST, MAL_NULL, MAL_NIL, MAL_VECTOR, MAL_HASHMAP,
    MAL_PERIOD, MAL_COMMA,
    MAL_READER_MACRO, MAL_AT, MAL_TILDE, MAL_TILDE_AT,
    MAL_QUOTE, MAL_QUASIQUOTE, MAL_META,
    MAL_NUMBER, MAL_BINARY, MAL_OCTAL, MAL_HEX,
    MAL_INTEGER, MAL_FRACTIONAL, MAL_RATIONAL, MAL_COMPLEX,
    MAL_VARIADIC    // special-case marker for variadic functions in environment
};


class MalType;

typedef std::shared_ptr<MalType> MalPtr;


class TokenVector
{
public:
    TokenVector(): current_token(0) {tokens.reserve(65535);};
    size_t size() const {return tokens.size();};
    size_t capacity() const {return tokens.capacity();};
    size_t append(MalPtr token);
    size_t append(const TokenVector& t);
    std::string values();
    std::string types();
    MalPtr next();
    MalPtr peek();
    MalPtr operator[](unsigned int i);

private:
    std::vector<MalPtr> tokens;
    unsigned int current_token;
};


class MalType
{
public:
    MalType(std::string const r):repr(r) {};
    virtual std::string value() {return repr;};
    virtual MalTypeName type() {return MAL_TYPE;};
    virtual TokenVector raw_value() {TokenVector t; return t;};
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


class MalAt: public MalReaderMacro
{
public:
    MalAt(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_AT;};
    virtual std::string value() {return "(deref " + list.values() + ')';};
};


class MalTilde: public MalReaderMacro
{
public:
    MalTilde(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_TILDE;};
    virtual std::string value() {return "(unquote " + list.values() + ')';};
};


class MalTildeAt: public MalReaderMacro
{
public:
    MalTildeAt(const TokenVector& l): MalReaderMacro(l) {};
    virtual MalTypeName type() {return MAL_TILDE_AT;};
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
    virtual MalTypeName type() {return MAL_HASHMAP;};
    virtual std::string value();
private:
    std::map<std::string, std::shared_ptr<MalType> > hashmap;
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
    MalSymbol(std::string r): MalAtom(r) {};
    virtual MalTypeName type() {return MAL_SYMBOL;};
};


class MalKeyword: public MalSymbol
{
public:
    MalKeyword(std::string r): MalSymbol(r) {};
    virtual MalTypeName type() {return MAL_KEYWORD;};
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
    MalInteger(std::string r): MalNumber(r) {};
    virtual MalTypeName type() {return MAL_INTEGER;};
};


class MalHex: public MalInteger
{
public:
    MalHex(std::string r): MalInteger(r) {};
    virtual MalTypeName type() {return MAL_HEX;};
};


class MalBinary: public MalInteger
{
public:
    MalBinary(std::string r): MalInteger(r) {};
    virtual MalTypeName type() {return MAL_BINARY;};
};


class MalOctal: public MalInteger
{
public:
    MalOctal(std::string r): MalInteger(r) {};
    virtual MalTypeName type() {return MAL_OCTAL;};
};


class MalFractional: public MalNumber
{
public:
    MalFractional(std::string r): MalNumber(r) {};
    virtual MalTypeName type() {return MAL_FRACTIONAL;};
};


class MalRational: public MalNumber
{
public:
    MalRational(std::string r): MalNumber(r) {};
    virtual MalTypeName type() {return MAL_RATIONAL;};
};


class MalComplex: public MalNumber
{
public:
    MalComplex(std::string r): MalNumber(r) {};
    virtual MalTypeName type() {return MAL_COMPLEX;};
};

#endif
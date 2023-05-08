#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <memory>
#include <string>
#include <vector>
#include <map>


class MalType;

typedef std::shared_ptr<MalType> MalPtr;


class TokenVector
{
public:
    TokenVector() {tokens.reserve(65535);};
    size_t size() const {return tokens.size();};
    size_t capacity() const {return tokens.capacity();};
    size_t append(MalPtr token);
    size_t append(const TokenVector& t);
    std::string values();
    std::string types();
    MalPtr operator[](unsigned int i);

private:
    std::vector<MalPtr> tokens;
};


class MalType
{
public:
    MalType(std::string const r):repr(r) {};
    // virtual ~MalType() {std::cout << "Deleting " << value() << '\n'; };
    virtual std::string value() {return repr;};
    virtual std::string type() {return "{base}";};
    virtual TokenVector raw_value() {TokenVector t; return t;};
protected:
    std::string repr;
};


class MalPeriod: public MalType
{
public:
    MalPeriod(): MalType(".") {};
    virtual std::string type() {return "Period";};
};

class MalReaderMacro: public MalType
{
public:
    MalReaderMacro(const TokenVector& l);
    virtual std::string type() {return "Reader Macro";};
    virtual std::string value();
    virtual TokenVector raw_value();
    MalPtr operator[](unsigned int i);
protected:
    TokenVector list;
};


class MalAt: public MalReaderMacro
{
public:
    MalAt(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Deref";};
};


class MalTildeAt: public MalReaderMacro
{
public:
    MalTildeAt(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Splice-unquote";};
};

class MalQuote: public MalReaderMacro
{
public:
    MalQuote(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Quote";};
};


class MalQuasiquote: public MalReaderMacro
{
public:
    MalQuasiquote(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Quasiquote";};
};


class MalTilde: public MalReaderMacro
{
public:
    MalTilde(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Splice";};
};

class MalComma: public MalReaderMacro
{
public:
    MalComma(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Unquote";};
};

class MalMeta: public MalReaderMacro
{
public:
    MalMeta(const TokenVector& l): MalReaderMacro(l) {};
    virtual std::string type() {return "Meta";};
};

class MalNull: public MalType
{
public:
    MalNull(): MalType("nil") {};
    virtual std::string type() {return "Null";};
};

class MalBoolean: public MalType
{
public:
    MalBoolean(std::string r): MalType(r) {};
    virtual std::string type() {return repr;};
};


class MalList: public MalType
{
public:
    MalList() = delete;
    MalList(std::string const r) = delete;
    MalList(const TokenVector& l);
    virtual std::string value();
    virtual std::string type() {return "List";};
    virtual TokenVector raw_value();
private:
    TokenVector list;
};


class MalVector: public MalType
{
public:
    MalVector() = delete;
    MalVector(std::string const r) = delete;
    MalVector(const TokenVector& v);
    virtual std::string value();
    virtual std::string type() {return "Vector";};
    virtual TokenVector raw_value();
private:
    TokenVector vec;
};


class MalHashmap: public MalType
{
public:
    MalHashmap(TokenVector hm);
    virtual std::string type() {return "Hash Map";};
    virtual std::string value();
private:
    std::map<std::string, std::shared_ptr<MalType> > hashmap;
};


class MalAtom: public MalType
{
public:
    MalAtom(std::string r): MalType(r) {};
    virtual std::string type() {return "Atom";};
};


class MalSymbol: public MalAtom
{
public:
    MalSymbol(std::string r): MalAtom(r) {};
    virtual std::string type() {return "Symbol";};
};


class MalKeyword: public MalSymbol
{
public:
    MalKeyword(std::string r): MalSymbol(r) {};
    virtual std::string type() {return "Keyword";};
};


class MalChar: public MalAtom
{
public:
    MalChar(std::string r): MalAtom(r) {};
    virtual std::string type() {return "Char";};
};


class MalString: public MalAtom
{
public:
    MalString(std::string r): MalAtom(r) {};
    virtual std::string type() {return "String";};
};


class MalNumber: public MalAtom
{
public:
    MalNumber(std::string r): MalAtom(r) {};
    virtual std::string type() {return "Number";};
};


class MalInteger: public MalNumber
{
public:
    MalInteger(std::string r): MalNumber(r) {};
    virtual std::string type() {return "Integer";};
};


class MalHex: public MalInteger
{
public:
    MalHex(std::string r): MalInteger(r) {};
    virtual std::string type() {return "Hex Integer";};
};


class MalBinary: public MalInteger
{
public:
    MalBinary(std::string r): MalInteger(r) {};
    virtual std::string type() {return "Binary Integer";};
};


class MalOctal: public MalInteger
{
public:
    MalOctal(std::string r): MalInteger(r) {};
    virtual std::string type() {return "Octal Integer";};
};


class MalDecimal: public MalNumber
{
public:
    MalDecimal(std::string r): MalNumber(r) {};
    virtual std::string type() {return "Decimal";};
};


class MalRational: public MalNumber
{
public:
    MalRational(std::string r): MalNumber(r) {};
    virtual std::string type() {return "Rational";};
};


class MalComplex: public MalNumber
{
public:
    MalComplex(std::string r): MalNumber(r) {};
    virtual std::string type() {return "Complex";};
};

#endif
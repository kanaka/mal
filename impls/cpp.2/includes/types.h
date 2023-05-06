#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <memory>
#include <string>
#include <vector>

class MalType;

typedef std::unique_ptr<MalType> MalPtr;


class TokenVector
{
public:
    TokenVector() {tokens.reserve(65535);};
    size_t size() const {return tokens.size();};
    size_t capacity() const {return tokens.capacity();};
    size_t append(MalPtr token);
    size_t append(TokenVector& t);
    std::string values();

private:
    std::vector<MalPtr> tokens;
};


class MalType
{
public:
    MalType(std::string const r):repr(r) {};
    MalType() = delete;
    MalType(MalType const & t) = delete;
    virtual std::string value() {return repr;};
    virtual std::string type() {return "{base}";};
protected:
    std::string repr;
};


class MalPeriod: public MalType
{
public:
    MalPeriod(): MalType(".") {};
    virtual std::string type() {return "Period";};
};


class MalAt: public MalType
{
public:
    MalAt(): MalType("@") {};
    virtual std::string type() {return "At";};
};


class MalQuote: public MalType
{
public:
    MalQuote(): MalType("\'") {};
    virtual std::string type() {return "Quote";};
};


class MalQuasiquote: public MalType
{
public:
    MalQuasiquote(): MalType("`") {};
    virtual std::string type() {return "Quasiquote";};
};


class MalTilde: public MalType
{
public:
    MalTilde(): MalType("~") {};
    virtual std::string type() {return "Tilde";};
};

class MalComma: public MalType
{
public:
    MalComma(): MalType(",") {};
    virtual std::string type() {return "Comma";};
};


class MalList: public MalType
{
public:
    MalList(TokenVector& l);
    void append(TokenVector& t);
    virtual std::string value();
    virtual std::string type() {return "List";};
private:
    TokenVector list;
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
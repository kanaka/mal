#ifndef EXCEPTIONS_H
#define EXCEPTIONS_H

#include <string>

class UnbalancedParenthesesException
{
public:
    UnbalancedParenthesesException() {};
};


class UnbalancedVectorException
{
public:
    UnbalancedVectorException() {};
};


class UnbalancedStringException
{
public:
    UnbalancedStringException() {};
};


class UnbalancedHashmapException
{
public:
    UnbalancedHashmapException() {};
};


class IncompleteComplexNumberException
{
public:
    IncompleteComplexNumberException() {};
};

class InvalidNumberException
{
public:
    InvalidNumberException(std::string v): number_value(v) {};
    std::string value() const { return number_value; };
protected:
    std::string number_value;
};

class InvalidBinaryNumberException: public InvalidNumberException
{
public:
    InvalidBinaryNumberException(std::string v): InvalidNumberException(v) {};
};

class InvalidOctalNumberException: public InvalidNumberException
{
public:
    InvalidOctalNumberException(std::string v): InvalidNumberException(v) {};
};

class InvalidHexNumberException: public InvalidNumberException
{
public:
    InvalidHexNumberException(std::string v): InvalidNumberException(v) {};
};

class InvalidComplexNumberException: public InvalidNumberException
{
public:
    InvalidComplexNumberException(std::string v): InvalidNumberException(v) {};
};

class InvalidHashmapException
{
public:
    InvalidHashmapException() {};
};


class IncompleteEscapeException
{
public:
    IncompleteEscapeException() {};
};


class InvalidMetaException
{
public:
    InvalidMetaException() {};
};


class InvalidEnvironmentSymbolException
{
public:
    InvalidEnvironmentSymbolException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};

class InvalidPrimitiveException
{
public:
    InvalidPrimitiveException() {};
};


class InvalidFunctionArgumentException
{
public:
    InvalidFunctionArgumentException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class MissingFunctionArgumentException
{
public:
    MissingFunctionArgumentException() {};
};



class TooManyInputsException
{
public:
    TooManyInputsException() {};
};


class ArityMismatchException
{
public:
    ArityMismatchException() {};
};


class ApplyingNonFunctionException
{
public:
    ApplyingNonFunctionException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class ProcedureNotFoundException
{
public:
    ProcedureNotFoundException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class SymbolNotInitializedException
{
public:
    SymbolNotInitializedException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class InvalidDefineException
{
public:
    InvalidDefineException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class InvalidLetException
{
public:
    InvalidLetException(std::string sym): symbol_value(sym) {};
    std::string value() const { return symbol_value; };
protected:
    std::string symbol_value;
};


class UnequalBindExprListsException
{
public:
    UnequalBindExprListsException(std::string b, std::string e): binds(b), exprs(e) {};
    std::string value() const { return binds + ": " + exprs; };
protected:
    std::string binds, exprs;
};


class InvalidBindExprListsException
{
public:
    InvalidBindExprListsException(std::string b, std::string e): binds(b), exprs(e) {};
    std::string value() const { return binds + ": " + exprs; };
protected:
    std::string binds, exprs;
};


class NonNumericComparisonException
{
public:
    NonNumericComparisonException(std::string b, std::string e): car(b), cdr(e) {};
    std::string value() const { return car + ": " + cdr; };
protected:
    std::string car, cdr;
};




class NullTokenException
{
public:
    NullTokenException() {};
};


class InvalidConsPairException
{
public:
    InvalidConsPairException(std::string p): pair(p) {};
    std::string value() const { return pair; };
protected:
    std::string pair;
};

#endif
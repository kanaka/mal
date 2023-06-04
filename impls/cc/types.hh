#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

class MalType
{
public:
    enum class Type
    {
        Int,
        Symbol,
        List,
        Func
    };

    MalType(Type t) : type_(t) {}
    virtual ~MalType() = default;

    Type type() const { return type_; }

private:
    Type type_;
};

class MalInt : public MalType
{
public:
    MalInt(int val = 0)
        : MalType(Type::Int), val_(val) {}

    operator int() const { return val_; }

private:
    int val_;
};

class MalSymbol : public MalType
{
public:
    MalSymbol(const std::string &symbol)
        : MalType(Type::Symbol), symbol_(symbol) {}

    MalSymbol &operator=(const std::string &symbol)
    {
        symbol_ = symbol;
        return *this;
    }

    operator std::string() const { return symbol_; }

    bool operator==(const std::string &str) const noexcept { return symbol_ == str; }
    bool operator==(const char *str) const noexcept { return symbol_ == str; }
    bool operator!=(const std::string &str) const noexcept { return symbol_ != str; }
    bool operator!=(const char *str) const noexcept { return symbol_ != str; }

private:
    std::string symbol_;
};

class MalList : public MalType
{
public:
    MalList(char lparen, char rparen)
        : MalType(Type::List), lparen_(lparen), rparen_(rparen) {}

    const MalType &operator[](std::size_t pos) const { return *list_[pos]; }
    [[nodiscard]] bool empty() const noexcept { return list_.empty(); }
    std::size_t size() const noexcept { return list_.size(); }
    void push_back(std::unique_ptr<MalType> value) { return list_.push_back(std::move(value)); }

    std::vector<std::unique_ptr<MalType>>::iterator begin() noexcept { return list_.begin(); }
    std::vector<std::unique_ptr<MalType>>::const_iterator begin() const noexcept { return list_.begin(); }
    std::vector<std::unique_ptr<MalType>>::iterator end() noexcept { return list_.end(); }
    std::vector<std::unique_ptr<MalType>>::const_iterator end() const noexcept { return list_.end(); }

    char lparen() const { return lparen_; }
    char rparen() const { return rparen_; }

private:
    char lparen_;
    char rparen_;
    std::vector<std::unique_ptr<MalType>> list_;
};

class MalFunc : public MalType
{
public:
    MalFunc(const std::function<int(std::vector<int>)> &func)
        : MalType(Type::Func), func_(func) {}

    int operator()(std::vector<int> args) const { return func_(args); }

private:
    const std::function<int(std::vector<int>)> &func_;
};

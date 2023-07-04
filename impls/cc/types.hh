#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

class Env;

class MalType
{
public:
    enum class Type
    {
        Int,
        Symbol,
        List,
        Func,
        Atom
    };

    MalType(Type t) : type_(t) {}
    virtual ~MalType() = default;

    virtual bool operator==(const MalType &rhs) const noexcept = 0;

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

    virtual bool operator==(const MalType &rhs) const noexcept override { return val_ == static_cast<const MalInt &>(rhs).val_; };

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

    operator std::string() const { return symbol_[0] == '"' ? symbol_.substr(1, symbol_.length() - 2) : symbol_; }

    bool operator==(const std::string &str) const noexcept { return symbol_ == str; }
    bool operator==(const char *str) const noexcept { return symbol_ == str; }
    bool operator!=(const std::string &str) const noexcept { return symbol_ != str; }
    bool operator!=(const char *str) const noexcept { return symbol_ != str; }

    virtual bool operator==(const MalType &rhs) const noexcept override { return symbol_ == static_cast<const MalSymbol &>(rhs).symbol_; };

    bool is_string() const { return symbol_[0] == '"'; }
    bool is_keyword() const { return symbol_[0] == ':'; }

    operator bool() const { return symbol_ != "nil" && symbol_ != "false"; }

private:
    std::string symbol_;
};

class MalList : public MalType
{
public:
    MalList(char lparen, char rparen)
        : MalType(Type::List), lparen_(lparen), rparen_(rparen) {}

    const std::shared_ptr<MalType> &operator[](std::size_t pos) const { return list_[pos]; }
    [[nodiscard]] bool empty() const noexcept { return list_.empty(); }
    std::size_t size() const noexcept { return list_.size(); }
    void push_back(std::shared_ptr<MalType> value) { return list_.push_back(value); }

    std::vector<std::shared_ptr<MalType>>::iterator begin() noexcept { return list_.begin(); }
    std::vector<std::shared_ptr<MalType>>::const_iterator begin() const noexcept { return list_.begin(); }
    std::vector<std::shared_ptr<MalType>>::iterator end() noexcept { return list_.end(); }
    std::vector<std::shared_ptr<MalType>>::const_iterator end() const noexcept { return list_.end(); }

    char lparen() const { return lparen_; }
    char rparen() const { return rparen_; }

    bool is_list() const { return lparen_ == '('; }
    bool is_vector() const { return lparen_ == '['; }
    bool is_map() const { return lparen_ == '{'; }

    virtual bool operator==(const MalType &rhs) const noexcept override
    {
        auto rhs_list = static_cast<const MalList &>(rhs);

        if (rhs_list.size() != list_.size())
            return false;

        for (unsigned i = 0; i < list_.size(); ++i)
        {
            if (rhs_list[i]->type() != list_[i]->type())
                return false;
            if (!(*(rhs_list[i]) == *(list_[i])))
                return false;
        }

        return true;
    };

private:
    char lparen_;
    char rparen_;
    std::vector<std::shared_ptr<MalType>> list_;
};

class MalFunc : public MalType
{
public:
    MalFunc(const std::function<std::shared_ptr<MalType>(std::vector<std::shared_ptr<MalType>>)> &func, std::shared_ptr<MalType> ast = nullptr, std::shared_ptr<MalType> params = nullptr, std::shared_ptr<Env> env = nullptr)
        : MalType(Type::Func), func_(func), ast_(ast), params_(params), env_(env) {}

    std::shared_ptr<MalType> operator()(std::vector<std::shared_ptr<MalType>> args) const { return func_(args); }

    virtual bool operator==(const MalType &rhs) const noexcept override { return false; }

    bool is_fn() const { return ast_ && params_ && env_; }
    std::shared_ptr<MalType> ast() const { return ast_; }
    std::shared_ptr<MalType> params() const { return params_; }
    std::shared_ptr<Env> env() const { return env_; }

private:
    std::function<std::shared_ptr<MalType>(std::vector<std::shared_ptr<MalType>>)> func_;
    std::shared_ptr<MalType> ast_;
    std::shared_ptr<MalType> params_;
    std::shared_ptr<Env> env_;
};

class MalAtom : public MalType
{
public:
    MalAtom(std::shared_ptr<MalType> a)
        : MalType(Type::Atom), a_(a) {}

    virtual bool operator==(const MalType &rhs) const noexcept override { return *a_ == rhs; }

    std::shared_ptr<MalType> deref() const { return a_; }
    std::shared_ptr<MalType> reset(std::shared_ptr<MalType> a) { return a_ = a; }

private:
    std::shared_ptr<MalType> a_;
};

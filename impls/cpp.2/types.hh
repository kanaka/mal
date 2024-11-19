#pragma once

#include <functional>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
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
        Map,
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

    bool operator<(const MalSymbol &str) const noexcept { return symbol_ < str.symbol_; }

    bool operator==(const std::string &str) const noexcept { return symbol_ == str; }
    bool operator==(const char *str) const noexcept { return symbol_ == str; }
    bool operator!=(const std::string &str) const noexcept { return symbol_ != str; }
    bool operator!=(const char *str) const noexcept { return symbol_ != str; }

    virtual bool operator==(const MalType &rhs) const noexcept override { return symbol_ == static_cast<const MalSymbol &>(rhs).symbol_; };

    bool is_string() const { return symbol_[0] == '"'; }
    bool is_keyword() const { return symbol_[0] == ':'; }
    bool is_reserved() const { return symbol_ == "true" || symbol_ == "false" || symbol_ == "nil"; }

    operator bool() const { return symbol_ != "nil" && symbol_ != "false"; }

private:
    std::string symbol_;
};

namespace std
{
    template <>
    struct hash<MalSymbol>
    {
        typedef MalSymbol argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const &s) const
        {
            return std::hash<std::string>()(s);
        }
    };
}

class MalList : public MalType
{
public:
    MalList(char lparen, char rparen)
        : MalType(Type::List), lparen_(lparen), rparen_(rparen) {}

    const std::shared_ptr<MalType> &operator[](std::size_t pos) const { return list_.at(pos); }
    [[nodiscard]] bool empty() const noexcept { return list_.empty(); }
    std::size_t size() const noexcept { return list_.size(); }
    void push_back(std::shared_ptr<MalType> value) { return list_.push_back(value); }

    std::vector<std::shared_ptr<MalType>>::iterator begin() noexcept { return list_.begin(); }
    std::vector<std::shared_ptr<MalType>>::const_iterator begin() const noexcept { return list_.begin(); }
    std::vector<std::shared_ptr<MalType>>::iterator end() noexcept { return list_.end(); }
    std::vector<std::shared_ptr<MalType>>::const_iterator end() const noexcept { return list_.end(); }

    char lparen() const { return lparen_; }
    char rparen() const { return rparen_; }

    bool is_list() const { return lparen_ == '(' && rparen_ == ')'; }
    bool is_vector() const { return lparen_ == '[' && rparen_ == ']'; }

    void set_meta(std::shared_ptr<MalType> meta) { meta_ = meta; }
    std::shared_ptr<MalType> get_meta() const { return meta_; }

    std::shared_ptr<MalList> to_list() const
    {
        auto ret = std::make_shared<MalList>(*this);
        ret->lparen_ = '(';
        ret->rparen_ = ')';
        return ret;
    }

    std::shared_ptr<MalList> to_vector() const
    {
        auto ret = std::make_shared<MalList>(*this);
        ret->lparen_ = '[';
        ret->rparen_ = ']';
        return ret;
    }

    virtual bool operator==(const MalType &rhs) const noexcept override
    {
        auto &rhs_list = static_cast<const MalList &>(rhs);

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
    }

private:
    char lparen_;
    char rparen_;
    std::vector<std::shared_ptr<MalType>> list_;
    std::shared_ptr<MalType> meta_ = std::make_shared<MalSymbol>("nil");
};

class MalMap : public MalType
{
public:
    // using map_t = std::unordered_map<MalSymbol, std::shared_ptr<MalType>>;
    using map_t = std::map<MalSymbol, std::shared_ptr<MalType>>;

    MalMap()
        : MalType(Type::Map) {}

    std::shared_ptr<MalType> &operator[](const MalSymbol &key) { return map_[key]; }
    [[nodiscard]] bool empty() const noexcept { return map_.empty(); }
    std::size_t size() const noexcept { return map_.size(); }
    void erase(const MalSymbol &key) { map_.erase(key); }

    map_t::iterator begin() noexcept { return map_.begin(); }
    map_t::const_iterator begin() const noexcept { return map_.begin(); }
    map_t::iterator end() noexcept { return map_.end(); }
    map_t::const_iterator end() const noexcept { return map_.end(); }

    map_t::const_iterator find(const MalSymbol &key) const { return map_.find(key); }

    void set_meta(std::shared_ptr<MalType> meta) { meta_ = meta; }
    std::shared_ptr<MalType> get_meta() const { return meta_; }

    virtual bool operator==(const MalType &rhs) const noexcept override
    {
        auto &rhs_map = static_cast<const MalMap &>(rhs);

        if (rhs_map.size() != map_.size())
            return false;

        for (auto [key, val] : map_)
        {
            auto result = rhs_map.find(key);
            if (result == rhs_map.end())
                return false;
            if (result->second->type() != val->type())
                return false;
            if (!(*result->second == *val))
                return false;
        }

        return true;
    }

private:
    map_t map_;
    std::shared_ptr<MalType> meta_ = std::make_shared<MalSymbol>("nil");
};

class MalFunc : public MalType
{
public:
    MalFunc(const std::function<std::shared_ptr<MalType>(std::vector<std::shared_ptr<MalType>>)> &func)
        : MalType(Type::Func), func_(func) {}

    MalFunc(const std::function<std::shared_ptr<MalType>(std::vector<std::shared_ptr<MalType>>)> &func, std::shared_ptr<MalType> ast, std::shared_ptr<MalType> params, std::shared_ptr<Env> env)
        : MalType(Type::Func), func_(func), ast_(ast), params_(params), env_(env), is_fn_(true) {}

    std::shared_ptr<MalType> operator()(std::vector<std::shared_ptr<MalType>> args) const { return func_(args); }

    virtual bool operator==(const MalType &rhs) const noexcept override { return false; }

    bool is_fn() const { return is_fn_; }
    std::shared_ptr<MalType> ast() const { return ast_; }
    std::shared_ptr<MalType> params() const { return params_; }
    std::shared_ptr<Env> env() const { return env_.lock(); }

    void set_meta(std::shared_ptr<MalType> meta) { meta_ = meta; }
    std::shared_ptr<MalType> get_meta() const { return meta_; }

    bool is_macro = false;

private:
    std::function<std::shared_ptr<MalType>(std::vector<std::shared_ptr<MalType>>)> func_;
    std::shared_ptr<MalType> meta_ = std::make_shared<MalSymbol>("nil");
    std::shared_ptr<MalType> ast_;
    std::shared_ptr<MalType> params_;
    std::weak_ptr<Env> env_;
    bool is_fn_ = false;
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

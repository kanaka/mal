#pragma once

#include "types.hh"
#include <iostream>
#include <map>
#include <memory>
#include <string>

class Env
{
public:
    explicit Env(std::shared_ptr<Env> outer = nullptr)
        : outer_(outer) {}

    Env(const MalList &binds, std::vector<std::shared_ptr<MalType>> exprs, std::shared_ptr<Env> outer)
        : outer_(outer)
    {
        for (unsigned i = 0; i < binds.size(); ++i)
        {
            auto symbol = static_cast<const MalSymbol &>(*binds[i]);
            if (symbol == "&")
            {
                auto rest = std::make_shared<MalList>('(', ')');
                for (unsigned j = i; j < exprs.size(); ++j)
                    rest->push_back(exprs[j]);
                set(static_cast<const MalSymbol &>(*binds[i + 1]), rest);
                break;
            }
            set(symbol, exprs[i]);
        }
    }

    void set(const std::string &key, std::shared_ptr<MalType> value) { data_[key] = value; }

    const Env *find(const std::string &key) const
    {
        if (data_.find(key) != data_.end())
            return this;
        return outer_ ? outer_->find(key) : nullptr;
    }

    std::shared_ptr<MalType> get(const std::string &key) const
    {
        auto env = find(key);
        if (!env)
            throw std::runtime_error('\'' + key + "' not found");
        return env->data_.at(key);
    }

    bool is_root() const { return !outer_; }

private:
    std::shared_ptr<Env> outer_;
    std::map<std::string, std::shared_ptr<MalType>> data_;
};

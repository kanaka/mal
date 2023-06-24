#pragma once

#include "types.hh"
#include <iostream>
#include <map>
#include <memory>
#include <string>

class Env
{
public:
    explicit Env(const Env *outer)
        : outer_(outer) {}

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
        {
            std::cerr << "key " << key << " not found";
            return nullptr;
        }
        return env->data_.at(key);
    }

private:
    const Env *outer_;
    std::map<std::string, std::shared_ptr<MalType>> data_;
};

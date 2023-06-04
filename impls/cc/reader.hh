#pragma once

#include "types.hh"
#include <regex>

class Reader
{
public:
    Reader(const std::string &input, std::regex regex)
        : regex_(regex)
    {
        iter_ = std::sregex_iterator(input.begin(), input.end(), regex_);
    }

    std::string next() { return iter_++->str(1); }
    std::string peak() const { return iter_->str(1); }
    bool empty() const
    {
        return iter_ == std::sregex_iterator() ||
               iter_->empty() ||
               !iter_->operator[](1).matched ||
               iter_->operator[](1).length() == 0;
    }

private:
    std::regex regex_;
    std::sregex_iterator iter_;
};

std::shared_ptr<MalType> read_str(const std::string &input);

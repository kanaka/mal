#ifndef PARSE_NUMBERS_H
#define PARSE_NUMBERS_H

#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "reader.h"
#include "types.h"


void read_number(std::string input_stream, char leading, TokenVector& tokens);
void read_based_integer(std::string input_stream, TokenVector& tokens);
void read_binary(std::string input_stream, TokenVector & tokens);
void read_octal(std::string input_stream, char leading, TokenVector & tokens);
void read_hex(std::string input_stream, TokenVector & tokens);
void read_trailing_zeroes(std::string input_stream, TokenVector& tokens);
void read_decimal(std::string input_stream, char leading, TokenVector& tokens);
void read_fractional(std::string input_stream, std::string leading, TokenVector& tokens);
void read_rational(std::string input_stream, std::string leading, TokenVector& tokens);
void read_complex(std::string input_stream, std::string leading, char trailing, TokenVector& tokens);

#endif
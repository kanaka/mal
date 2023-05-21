#ifndef READER_H
#define READER_H

#include <string>
#include <memory>
#include <vector>
#include "lineedit.h"
#include "types.h"

extern unsigned int paren_count, square_bracket_count, hm_count, s_index;


TokenVector tokenize(std::string input_stream);
TokenVector read_str(std::string s);

void read_whitespace(std::string input_stream, char leading);
void read_comment(std::string input_stream);
void read_string(std::string input_stream, TokenVector& tokens);
void read_symbol(std::string input_stream, char leading, TokenVector& tokens);


#endif
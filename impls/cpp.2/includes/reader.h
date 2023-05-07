#ifndef READER_H
#define READER_H

#include <string>
#include <memory>
#include <vector>
#include "lineedit.h"
#include "types.h"


TokenVector tokenize(std::string input_stream, LineEdit& line);
TokenVector read_str(std::string s, LineEdit& line);

#endif
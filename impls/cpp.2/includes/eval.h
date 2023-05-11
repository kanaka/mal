#ifndef EVAL_H
#define EVAL_H

#include "env.h"
#include "types.h"

TokenVector eval_ast(TokenVector input, Environment env);
TokenVector eval_list(TokenVector input, Environment env);

#endif
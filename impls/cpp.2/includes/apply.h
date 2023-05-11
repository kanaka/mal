#ifndef EVAL_H
#define EVAL_H

#include <functional>
#include <typeinfo>
#include <cstdarg>
#include "types.h"
#include "eval.h"
#include "env.h"


TokenVector apply_fn(TokenVector fn, TokenVector args);

#endif


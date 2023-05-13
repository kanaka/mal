#ifndef APPLY_H
#define APPLY_H

#include "types.h"
#include "eval.h"
#include "env.h"
#include "exceptions.h"


TokenVector apply_fn(EnvPtr fn, TokenVector& args);

TokenVector apply_fn(TokenVector fn, TokenVector& args);

#endif

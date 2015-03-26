#ifndef INCLUDE_VALIDATION_H
#define INCLUDE_VALIDATION_H

#include "String.h"

#define ASSERT(condition, ...)  \
    if (!(condition)) { throw STRF(__VA_ARGS__); } else { }

#endif // INCLUDE_VALIDATION_H

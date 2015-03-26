#ifndef INCLUDE_VALIDATION_H
#define INCLUDE_VALIDATION_H

#include "String.h"

#define ASSERT(condition, ...)  \
    if (!(condition)) { throw STRF(__VA_ARGS__); } else { }

extern int checkArgsIs(const char* name, int expected, int got);
extern int checkArgsBetween(const char* name, int min, int max, int got);
extern int checkArgsAtLeast(const char* name, int min, int got);
extern int checkArgsEven(const char* name, int got);

#endif // INCLUDE_VALIDATION_H

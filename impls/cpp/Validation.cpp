#include "Validation.h"

int checkArgsIs(const char* name, int expected, int got)
{
    MAL_CHECK(got == expected,
           "\"%s\" expects %d arg%s, %d supplied",
           name, expected, PLURAL(expected), got);
    return got;
}

int checkArgsBetween(const char* name, int min, int max, int got)
{
    MAL_CHECK((got >= min) && (got <= max),
           "\"%s\" expects between %d and %d arg%s, %d supplied",
           name, min, max, PLURAL(max), got);
    return got;
}

int checkArgsAtLeast(const char* name, int min, int got)
{
    MAL_CHECK(got >= min,
           "\"%s\" expects at least %d arg%s, %d supplied",
           name, min, PLURAL(min), got);
    return got;
}

int checkArgsEven(const char* name, int got)
{
    MAL_CHECK(got % 2 == 0,
           "\"%s\" expects an even number of args, %d supplied",
           name, got);
    return got;
}

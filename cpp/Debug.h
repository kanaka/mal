#ifndef INCLUDE_DEBUG_H
#define INCLUDE_DEBUG_H

#include <stdio.h>
#include <stdlib.h>

#define DEBUG_TRACE                    1
//#define DEBUG_OBJECT_LIFETIMES         1
//#define DEBUG_ENV_LIFETIMES            1

#define DEBUG_TRACE_FILE    stderr

#define NOOP    do { } while (false)
#define NOTRACE(...)    NOOP

#if DEBUG_TRACE
    #define TRACE(...) fprintf(DEBUG_TRACE_FILE, __VA_ARGS__)
#else
    #define TRACE NOTRACE
#endif

#if DEBUG_OBJECT_LIFETIMES
    #define TRACE_OBJECT TRACE
#else
    #define TRACE_OBJECT NOTRACE
#endif

#if DEBUG_ENV_LIFETIMES
    #define TRACE_ENV TRACE
#else
    #define TRACE_ENV NOTRACE
#endif

#define _ASSERT(file, line, condition, ...) \
    if (!(condition)) { \
        printf("Assertion failed at %s(%d): ", file, line); \
        printf(__VA_ARGS__); \
        exit(1); \
    } else { }


#define ASSERT(condition, ...) \
    _ASSERT(__FILE__, __LINE__, condition, __VA_ARGS__)

#endif // INCLUDE_DEBUG_H

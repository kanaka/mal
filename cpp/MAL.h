#ifndef INCLUDE_MAL_H
#define INCLUDE_MAL_H

#include "Debug.h"
#include "RefCountedPtr.h"
#include "String.h"
#include "Validation.h"

#include <vector>

class malValue;
typedef RefCountedPtr<malValue>  malValuePtr;
typedef std::vector<malValuePtr> malValueVec;
typedef malValueVec::iterator    malValueIter;

class malEnv;
typedef RefCountedPtr<malEnv>     malEnvPtr;

// step*.cpp
extern malValuePtr APPLY(malValuePtr op,
                         malValueIter argsBegin, malValueIter argsEnd,
                         malEnvPtr env);
extern malValuePtr EVAL(malValuePtr ast, malEnvPtr env);
extern malValuePtr readline(const String& prompt);
extern String rep(const String& input, malEnvPtr env);

// Core.cpp
extern void installCore(malEnvPtr env);

// Reader.cpp
extern malValuePtr readStr(const String& input);

#endif // INCLUDE_MAL_H

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

// step*.cpp
extern malValuePtr APPLY(malValuePtr op,
                         malValueIter argsBegin, malValueIter argsEnd,
                         malEnv& env);
extern malValuePtr EVAL(malValuePtr ast, malEnv& env);
extern String rep(const String& input, malEnv& env);

// Reader.cpp
extern malValuePtr readStr(const String& input);

#endif // INCLUDE_MAL_H

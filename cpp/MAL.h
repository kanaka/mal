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

// step*.cpp
extern malValuePtr EVAL(malValuePtr ast);
extern String rep(const String& input);

// Reader.cpp
extern malValuePtr readStr(const String& input);

#endif // INCLUDE_MAL_H

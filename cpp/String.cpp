#include "String.h"

#include <stdlib.h>

String copyAndFree(char* mallocedString)
{
    String ret(mallocedString);
    free(mallocedString);
    return ret;
}

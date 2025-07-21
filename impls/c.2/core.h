#ifndef _MAL_CORE_H
#define _MAL_CORE_H

#include "types.h"

typedef const struct ns_s* ns;

struct ns_s {

  const char* key;
  function_t value;

};

void ns_make_core(ns* core, size_t* size);

#endif

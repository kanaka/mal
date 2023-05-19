#ifndef APPLY_H
#define APPLY_H

/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


#include "types.h"
#include "eval.h"
#include "env.h"
#include "exceptions.h"


TokenVector apply_fn(EnvPtr fn, TokenVector args);

// TokenVector apply_fn(ProcedurePtr p, TokenVector args);

#endif

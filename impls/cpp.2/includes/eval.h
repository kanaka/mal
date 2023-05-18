#ifndef EVAL_H
#define EVAL_H

/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


#include "env.h"
#include "types.h"


TokenVector EVAL(TokenVector input, Environment& env);
TokenVector eval_ast(TokenVector input, Environment env);
// TokenVector eval_list(TokenVector& input, Environment env);
TokenVector eval_vec(TokenVector input, Environment env);
TokenVector eval_hashmap(HashMapInternal input, Environment env);
TokenVector eval_quasiquoted(TokenVector input, Environment env, bool islist = false);

#endif
#ifndef __readline__
#define __readline__

readline: procedure /* readline(prompt) */
  call charout , arg(1)
  return linein()

#endif

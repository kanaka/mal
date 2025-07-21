#ifndef _PRINTER_H
#define _PRINTER_H

#include "types.h"

// This function must be called during startup.
void printer_init();
// It adds the following conversion specifiers (requires GNU libc).
//   %M  for MalType
//   ('#' means unreadably).
//   %N  for list    ('#' means unreadably, ' ' requires space separators).

// Both accept the '#' modifier, for which requires the strings to be
// escaped/unreadably.

// %N accepts the ' ' modifier, which requires that values are
// %separated by spaces instead of directly concatenated.

//  Similar to asprintf, except that
//    the memory is allocated with GC_MALLOC instead of malloc,
//    errors crash the program instead of being reported.
const char* mal_printf(const char* fmt, ...);

#endif

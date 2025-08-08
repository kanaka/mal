#ifndef _PRINTER_H
#define _PRINTER_H

// This function must be called during startup.
void printer_init();
// It adds the following conversion specifiers (requires GNU libc).

// specifier type            modifiers      meaning
//
// %M        MalType         #              no string escape
//                           positive width right padding
// %N        list            #              no string escape
//                           ' '            no space separator
// %T        enum mal_type_t
// %H        hashmap         #              no string escape

//  Similar to asprintf, except that
//    the memory is allocated with GC_MALLOC instead of malloc,
//    errors crash the program instead of being reported.
const char* mal_printf(const char* fmt, ...);

#endif

# x86_64 NASM implementation

Notes and known issues:

* No library dependencies, only Linux system calls

* Simple readline implemented, just supporting backspace for editing

* Reference counting used for memory management. No attempt is made
  to find circular references, so leaks are possible. In particular
  defining a function with def! creates a circular reference loop.
  
* The exception/error handling just resets the stack and jumps to a handler,
  so does not release memory

* Memory is allocated by two fixed-size allocators (`Cons` and `Array` objects)
  which have limits specified in types.asm. If more memory is needed
  then this must currently be done at compile-time, but adding sys_brk
  calls could be done.

* The hash map implementation is just a list of key-value pairs.
  Moving symbols around in the core environment makes a significant difference
  (20-30%) to the performance test. A simple optimisation could be to
  move items when found to the start of the list so that frequently
  searched keys are nearer the front.

* `conj` function not yet implemented

* `*env*`  symbol evaluates to current Environment.


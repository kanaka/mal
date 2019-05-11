# malcc

Mal (Make A Lisp) Compiler in C

[![build status](https://builds.sr.ht/~tim/malcc.svg)](https://builds.sr.ht/~tim/malcc)

## Overview

[Mal](https://github.com/kanaka/mal) is Clojure inspired Lisp interpreter
created by Joel Martin.

**malcc** is an incremental compiler implementation for the Mal language.
It uses the [Tiny C Compiler](https://bellard.org/tcc/) as the compiler backend
and has full support for the Mal language, including macros, tail-call elimination,
and even run-time eval.

malcc can also be used as an ahead-of-time compiler for Mal, producing a single
binary file for distribution (though using it this way sacrifices run-time eval
functionality since the compiler is not shipped in the resulting binary).

## Building and Running

malcc has been tested on Ubuntu 18.04 and macOS 10.14 Mojave.

**Prerequisites on Mac:**

```bash
sudo xcode-select --install
brew install pcre libgc
```

On Macos 10.14.4, there is a problem building TinyCC. This fixes that:

```bash
cd /usr/local/lib
sudo ln -s ../../lib/libSystem.B.dylib libgcc_s.10.4.dylib
```

**Prerequisites on Ubuntu/Debian:**

```bash
apt-get install libpcre3-dev libedit-dev libgc-dev
```

**Building malcc:**

```bash
git submodule update --init
make all
```

**Running the REPL:**

```bash
→ ./malcc
Mal [malcc]
user> (+ 1 2)
3
user> ^D
```

**Running a mal file:**

```bash
→ ./malcc examples/fib.mal
55
12586269025
```

**Ahead-of-Time compiling a mal file:**

```bash
→ ./malcc --compile examples/fib.mal fib
→ ./fib
55
12586269025
```

## Speed

malcc is fast! Running the microbenchmarks on my Macbook Pro yields an
order-of-magnitude speedup for long-running code vs the C++ implementation:

**C++:**

```bash
→ ../cpp/stepA_mal perf1.mal
"Elapsed time: 1 msecs"
→ ../cpp/stepA_mal perf2.mal
"Elapsed time: 2 msecs"
→ ../cpp/stepA_mal perf3.mal
iters over 10 seconds: 12415
```

**malcc:**

```bash
→ ../../stepA_mal perf1.mal
"Elapsed time: 0 msecs"
→ ../../stepA_mal perf2.mal
"Elapsed time: 3 msecs"
→ ../../stepA_mal perf3.mal
iters over 10 seconds: 226216
```

Note: I'm not sure if this is a fair comparison, but I could not coax the C
interpreter implementation of mal to run the perf3 test, so I figured the C++
implementation was the next-best thing.

## Approach

I followed the Mal guide to implement malcc, using the same steps that any
other implementation would follow. Naturally, since malcc is a compiler
rather than a straight interpreter, there are some differences from other
implementations:

1. Additional functions not specified in the Mal guide are employed to
   generate the C code that is passed to the TinyCC compiler. These functions
   all start with `gen_` and should be fairly self-explanatory.
2. `load-file` is implemented as a special form rather than a simple function.
   This is because macros defined in a file must be found and compiled during
   code generation--they cannot be discovered at run-time.
3. I chose to publish malcc in a separate repository and have structured it to
   suit my taste. A copy of the mal implementation of mal and the mal tests
   were copied into the `mal` directory to provide test coverage.

## Contributing

Send patches/bug reports to [~tim/public-inbox@lists.sr.ht](mailto:~tim/public-inbox@lists.sr.ht).

## License

This project's sourcecode is copyrighted by Tim Morgan and licensed under the
MIT license, included in the `LICENSE` file in this repository.

The subdirectory `mal` contains a copy of the Mal language repository and is
copyrighted by Joel Martin and released under the Mozilla Public License 2.0
(MPL 2.0). The text of the MPL 2.0 license is included in the `mal/LICENSE`
file.

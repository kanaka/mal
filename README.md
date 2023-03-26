# mal - Make a Lisp

[![Build Status](https://travis-ci.org/kanaka/mal.svg?branch=master)](https://travis-ci.org/kanaka/mal)

## Description

**1. Mal is a Clojure inspired Lisp interpreter**

**2. Mal is a learning tool**

Each implementation of mal is separated into
11 incremental, self-contained (and testable) steps that demonstrate
core concepts of Lisp. The last step is capable of self-hosting
(running the mal implementation of mal). See the [make-a-lisp process
guide](process/guide.md).

The make-a-lisp steps are:

* [step0_repl](process/guide.md#step0)
* [step1_read_print](process/guide.md#step1)
* [step2_eval](process/guide.md#step2)
* [step3_env](process/guide.md#step3)
* [step4_if_fn_do](process/guide.md#step4)
* [step5_tco](process/guide.md#step5)
* [step6_file](process/guide.md#step6)
* [step7_quote](process/guide.md#step7)
* [step8_macros](process/guide.md#step8)
* [step9_try](process/guide.md#step9)
* [stepA_mal](process/guide.md#stepA)

Each make-a-lisp step has an associated architectural diagram. That elements
that are new for that step are highlighted in red.
Here is the final diagram for [step A](process/guide.md#stepA):

![stepA_mal architecture](process/stepA_mal.png)

If you are interested in creating a mal implementation (or just
interested in using mal for something), you are welcome to to join our
[Discord](https://discord.gg/CKgnNbJBpF) or join #mal on
[libera.chat](https://libera.chat/). In addition to the [make-a-lisp
process guide](process/guide.md) there is also a [mal/make-a-lisp
FAQ](docs/FAQ.md) where I attempt to answer some common questions.


**3. Mal is implemented in 87 languages (93 different implementations and 115 runtime modes)**

| Language | Creator |
| -------- | ------- |
| [Ada](#ada) | [Chris Moore](https://github.com/zmower) |
| [Ada #2](#ada2) | [Nicolas Boulenguez](https://github.com/asarhaddon) |
| [GNU Awk](#gnu-awk) | [Miutsuru Kariya](https://github.com/kariya-mitsuru) |
| [Bash 4](#bash-4) | [Joel Martin](https://github.com/kanaka)  |
| [BASIC](#basic-c64-and-qbasic) (C64 &amp; QBasic) | [Joel Martin](https://github.com/kanaka) |
| [BBC BASIC V](#bbc-basic-v) | [Ben Harris](https://github.com/bjh21) |
| [C](#c) | [Joel Martin](https://github.com/kanaka)  |
| [C #2](#c2) | [Duncan Watts](https://github.com/fungiblecog)  |
| [C++](#c-1) | [Stephen Thirlwall](https://github.com/sdt) |
| [C#](#c-2) | [Joel Martin](https://github.com/kanaka)  |
| [ChucK](#chuck) | [Vasilij Schneidermann](https://github.com/wasamasa) |
| [Clojure](#clojure) (Clojure &amp; ClojureScript) | [Joel Martin](https://github.com/kanaka) |
| [CoffeeScript](#coffeescript) | [Joel Martin](https://github.com/kanaka)  |
| [Common Lisp](#common-lisp) | [Iqbal Ansari](https://github.com/iqbalansari) |
| [Crystal](#crystal) | [Linda_pp](https://github.com/rhysd) |
| [D](#d) | [Dov Murik](https://github.com/dubek) |
| [Dart](#dart) | [Harry Terkelsen](https://github.com/hterkelsen) |
| [Elixir](#elixir) | [Martin Ek](https://github.com/ekmartin) |
| [Elm](#elm) | [Jos van Bakel](https://github.com/c0deaddict) |
| [Emacs Lisp](#emacs-lisp) | [Vasilij Schneidermann](https://github.com/wasamasa) |
| [Erlang](#erlang) | [Nathan Fiedler](https://github.com/nlfiedler) |
| [ES6](#es6-ecmascript-2015) (ECMAScript 2015) | [Joel Martin](https://github.com/kanaka) |
| [F#](#f) | [Peter Stephens](https://github.com/pstephens) |
| [Factor](#factor) | [Jordan Lewis](https://github.com/jordanlewis) |
| [Fantom](#fantom) | [Dov Murik](https://github.com/dubek) |
| [Fennel](#fennel) | [sogaiu](https://github.com/sogaiu) |
| [Forth](#forth) | [Chris Houser](https://github.com/chouser) |
| [GNU Guile](#gnu-guile-21) | [Mu Lei](https://github.com/NalaGinrut) |
| [GNU Smalltalk](#gnu-smalltalk) | [Vasilij Schneidermann](https://github.com/wasamasa) |
| [Go](#go) | [Joel Martin](https://github.com/kanaka)  |
| [Groovy](#groovy) | [Joel Martin](https://github.com/kanaka)  |
| [Haskell](#haskell) | [Joel Martin](https://github.com/kanaka)  |
| [Haxe](#haxe-neko-python-c-and-javascript) (Neko, Python, C++, &amp; JS) | [Joel Martin](https://github.com/kanaka) |
| [Hy](#hy) | [Joel Martin](https://github.com/kanaka)  |
| [Io](#io) | [Dov Murik](https://github.com/dubek) |
| [Janet](#janet) | [sogaiu](https://github.com/sogaiu) |
| [Java](#java-17) | [Joel Martin](https://github.com/kanaka)  |
| [Java](#java-using-truffle-for-graalvm) (Truffle/GraalVM) | [Matt McGill](https://github.com/mmcgill)
| [JavaScript](#javascriptnode) ([Demo](http://kanaka.github.io/mal)) | [Joel Martin](https://github.com/kanaka) |
| [jq](#jq) | [Ali MohammadPur](https://github.com/alimpfard) |
| [Julia](#julia) | [Joel Martin](https://github.com/kanaka)  |
| [Kotlin](#kotlin) | [Javier Fernandez-Ivern](https://github.com/ivern) |
| [LiveScript](#livescript) | [Jos van Bakel](https://github.com/c0deaddict) |
| [Logo](#logo) | [Dov Murik](https://github.com/dubek) |
| [Lua](#lua) | [Joel Martin](https://github.com/kanaka)  |
| [GNU Make](#gnu-make-381) | [Joel Martin](https://github.com/kanaka)  |
| [mal itself](#mal) | [Joel Martin](https://github.com/kanaka)  |
| [MATLAB](#matlab-gnu-octave-and-matlab) (GNU Octave &amp; MATLAB) | [Joel Martin](https://github.com/kanaka) |
| [miniMAL](#minimal) ([Repo](https://github.com/kanaka/miniMAL), [Demo](https://kanaka.github.io/miniMAL/)) | [Joel Martin](https://github.com/kanaka) |
| [NASM](#nasm) | [Ben Dudson](https://github.com/bendudson) |
| [Nim](#nim-104) | [Dennis Felsing](https://github.com/def-) |
| [Object Pascal](#object-pascal) | [Joel Martin](https://github.com/kanaka)  |
| [Objective C](#objective-c) | [Joel Martin](https://github.com/kanaka)  |
| [OCaml](#ocaml-4010) | [Chris Houser](https://github.com/chouser) |
| [Perl](#perl-5) | [Joel Martin](https://github.com/kanaka)  |
| [Perl 6](#perl-6) | [Hinrik Örn Sigurðsson](https://github.com/hinrik) |
| [PHP](#php-53) | [Joel Martin](https://github.com/kanaka)  |
| [Picolisp](#picolisp) | [Vasilij Schneidermann](https://github.com/wasamasa) |
| [Pike](#pike) | [Dov Murik](https://github.com/dubek) |
| [PL/pgSQL](#plpgsql-postgresql-sql-procedural-language) (PostgreSQL) | [Joel Martin](https://github.com/kanaka) |
| [PL/SQL](#plsql-oracle-sql-procedural-language) (Oracle) | [Joel Martin](https://github.com/kanaka) |
| [PostScript](#postscript-level-23) | [Joel Martin](https://github.com/kanaka)  |
| [PowerShell](#powershell) | [Joel Martin](https://github.com/kanaka)  |
| [Prolog](#prolog-logical-language) | [Nicolas Boulenguez](https://github.com/asarhaddon) |
| [PureScript](#purescript) | [mrsekut](https://github.com/mrsekut) |
| [Python](#python-2x-and-3x) (2.X &amp; 3.X) | [Joel Martin](https://github.com/kanaka) |
| [Python #2](#python2-3x) (3.X) | [Gavin Lewis](https://github.com/epylar) |
| [RPython](#rpython) | [Joel Martin](https://github.com/kanaka)  |
| [R](#r) | [Joel Martin](https://github.com/kanaka)  |
| [Racket](#racket-53) | [Joel Martin](https://github.com/kanaka)  |
| [Rexx](#rexx) | [Dov Murik](https://github.com/dubek) |
| [Ruby](#ruby-19) | [Joel Martin](https://github.com/kanaka)  |
| [Ruby #2](#ruby) | [Ryan Cook](https://github.com/cookrn)  |
| [Rust](#rust-138) | [Joel Martin](https://github.com/kanaka)  |
| [Scala](#scala) | [Joel Martin](https://github.com/kanaka)  |
| [Scheme (R7RS)](#scheme-r7rs) | [Vasilij Schneidermann](https://github.com/wasamasa) |
| [Skew](#skew) | [Dov Murik](https://github.com/dubek) |
| [Standard ML](#sml) | [Fabian Bergström](https://github.com/fabjan) |
| [Swift 2](#swift) | [Keith Rollin](https://github.com/keith-rollin) |
| [Swift 3](#swift-3) | [Joel Martin](https://github.com/kanaka)  |
| [Swift 4](#swift-4) | [陆遥](https://github.com/LispLY)  |
| [Swift 5](#swift-5) | [Oleg Montak](https://github.com/MontakOleg)  |
| [Tcl](#tcl-86) | [Dov Murik](https://github.com/dubek) |
| [TypeScript](#typescript) | [Masahiro Wakame](https://github.com/vvakame) |
| [Vala](#vala) | [Simon Tatham](https://github.com/sgtatham) |
| [VHDL](#vhdl) | [Dov Murik](https://github.com/dubek) |
| [Vimscript](#vimscript) | [Dov Murik](https://github.com/dubek) |
| [Visual Basic.NET](#visual-basicnet) | [Joel Martin](https://github.com/kanaka)  |
| [WebAssembly](#webassembly-wasm) (wasm) | [Joel Martin](https://github.com/kanaka) |
| [Wren](#wren) | [Dov Murik](https://github.com/dubek) |
| [XSLT](#xslt) | [Ali MohammadPur](https://github.com/alimpfard) |
| [Yorick](#yorick) | [Dov Murik](https://github.com/dubek) |
| [Zig](#zig) | [Josh Tobin](https://github.com/rjtobin) |


## Presentations

Mal was presented publicly for the first time in a lightning talk at
Clojure West 2014 (unfortunately there is no video). See
examples/clojurewest2014.mal for the presentation that was given at the
conference (yes, the presentation is a mal program).

At Midwest.io 2015, Joel Martin gave a presentation on Mal titled
"Achievement Unlocked: A Better Path to Language Learning".
[Video](https://www.youtube.com/watch?v=lgyOAiRtZGw),
[Slides](http://kanaka.github.io/midwest.io.mal/).

More recently Joel gave a presentation on "Make Your Own Lisp Interpreter
in 10 Incremental Steps" at LambdaConf 2016:
[Part 1](https://www.youtube.com/watch?v=jVhupfthTEk),
[Part 2](https://www.youtube.com/watch?v=X5OQBMGpaTU),
[Part 3](https://www.youtube.com/watch?v=6mARZzGgX4U),
[Part 4](https://www.youtube.com/watch?v=dCO1SYR5kDU),
[Slides](http://kanaka.github.io/lambdaconf/).

## Building/running implementations

The simplest way to run any given implementation is to use docker.
Every implementation has a docker image pre-built with language
dependencies installed. You can launch the REPL using a convenient
target in the top level Makefile (where IMPL is the implementation
directory name and stepX is the step to run):

```
make DOCKERIZE=1 "repl^IMPL^stepX"
    # OR stepA is the default step:
make DOCKERIZE=1 "repl^IMPL"
```

## External Implementations

The following implementations are maintained as separate projects:

### HolyC

* [by Alexander Bagnalla](https://github.com/bagnalla/holyc_mal)

### Rust

* [by Tim Morgan](https://github.com/seven1m/mal-rust)
* [by vi](https://github.com/vi/mal-rust-vi) - using [Pest](https://pest.rs/) grammar, not using typical Mal infrastructure (cargo-ized steps and built-in converted tests).

### Q

* [by Ali Mohammad Pur](https://github.com/alimpfard/mal/tree/q/impls/q) - The Q implementation works fine but it requires a proprietary manual download that can't be Dockerized (or integrated into the mal CI pipeline) so for now it remains a separate project.


## Other mal Projects

 * [malc](https://github.com/dubek/malc) - Mal (Make A Lisp) compiler. Compiles a Mal program to LLVM assembly language, then binary.
 * [malcc](https://github.com/seven1m/malcc) - malcc is an incremental compiler implementation for the Mal language. It uses the Tiny C Compiler as the compiler backend and has full support for the Mal language, including macros, tail-call elimination, and even run-time eval. ["I Built a Lisp Compiler"](https://mpov.timmorgan.org/i-built-a-lisp-compiler/) post about the process.
 * [frock](https://github.com/chr15m/frock) - Clojure-flavoured PHP. Uses mal/php to run programs.
 * [flk](https://github.com/chr15m/flk) - A LISP that runs wherever Bash is
 * [glisp](https://github.com/baku89/glisp) - Self-bootstrapping graphic design tool on Lisp. [Live Demo](https://baku89.com/glisp/)


## Implementation Details

### Ada

The Ada implementation was developed with GNAT 4.9 on debian. It also
compiles unchanged on windows if you have windows versions of git,
GNAT and (optionally) make.  There are no external dependencies
(readline not implemented).

```
cd impls/ada
make
./stepX_YYY
```

### Ada.2

The second Ada implementation was developed with GNAT 8 and links with
the GNU readline library.

```
cd impls/ada
make
./stepX_YYY
```

### GNU awk

The GNU awk implementation of mal has been tested with GNU awk 4.1.1.

```
cd impls/gawk
gawk -O -f stepX_YYY.awk
```

### Bash 4

```
cd impls/bash
bash stepX_YYY.sh
```

### BASIC (C64 and QBasic)

The BASIC implementation uses a preprocessor that can generate BASIC
code that is compatible with both C64 BASIC (CBM v2) and QBasic. The
C64 mode has been tested with
[cbmbasic](https://github.com/kanaka/cbmbasic) (the patched version is
currently required to fix issues with line input) and the QBasic mode
has been tested with [qb64](http://www.qb64.net/).

Generate C64 code and run it using cbmbasic:

```
cd impls/basic
make stepX_YYY.bas
STEP=stepX_YYY ./run
```

Generate QBasic code and load it into qb64:

```
cd impls/basic
make MODE=qbasic stepX_YYY.bas
./qb64 stepX_YYY.bas
```

Thanks to [Steven Syrek](https://github.com/sjsyrek) for the original
inspiration for this implementation.

### BBC BASIC V

The BBC BASIC V implementation can run in the Brandy interpreter:

```
cd impls/bbc-basic
brandy -quit stepX_YYY.bbc
```

Or in ARM BBC BASIC V under RISC OS 3 or later:

```
*Dir bbc-basic.riscos
*Run setup
*Run stepX_YYY
```

### C

The C implementation of mal requires the following libraries (lib and
header packages): glib, libffi6, libgc, and either the libedit or GNU readline
library.

```
cd impls/c
make
./stepX_YYY
```

### C.2

The second C implementation of mal requires the following libraries (lib and
header packages): libedit, libgc, libdl, and libffi.

```
cd impls/c.2
make
./stepX_YYY
```


### C++

The C++ implementation of mal requires g++-4.9 or clang++-3.5 and
a readline compatible library to build. See the `cpp/README.md` for
more details:

```
cd impls/cpp
make
    # OR
make CXX=clang++-3.5
./stepX_YYY
```


### C# ###

The C# implementation of mal has been tested on Linux using the Mono
C# compiler (mcs) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the C# implementation.

```
cd impls/cs
make
mono ./stepX_YYY.exe
```

### ChucK

The ChucK implementation has been tested with ChucK 1.3.5.2.

```
cd impls/chuck
./run
```

### Clojure

For the most part the Clojure implementation requires Clojure 1.5,
however, to pass all tests, Clojure 1.8.0-RC4 is required.

```
cd impls/clojure
lein with-profile +stepX trampoline run
```

### CoffeeScript

```
sudo npm install -g coffee-script
cd impls/coffee
coffee ./stepX_YYY
```

### Common Lisp

The implementation has been tested with SBCL, CCL, CMUCL, GNU CLISP, ECL and
Allegro CL on Ubuntu 16.04 and Ubuntu 12.04, see
the [README](impls/common-lisp/README.org) for more details. Provided you have the
dependencies mentioned installed, do the following to run the implementation

```
cd impls/common-lisp
make
./run
```

### Crystal

The Crystal implementation of mal has been tested with Crystal 0.26.1.

```
cd impls/crystal
crystal run ./stepX_YYY.cr
    # OR
make   # needed to run tests
./stepX_YYY
```

### D

The D implementation of mal was tested with GDC 4.8.  It requires the GNU
readline library.

```
cd impls/d
make
./stepX_YYY
```

### Dart

The Dart implementation has been tested with Dart 1.20.

```
cd impls/dart
dart ./stepX_YYY
```

### Emacs Lisp

The Emacs Lisp implementation of mal has been tested with Emacs 24.3
and 24.5.  While there is very basic readline editing (`<backspace>`
and `C-d` work, `C-c` cancels the process), it is recommended to use
`rlwrap`.

```
cd impls/elisp
emacs -Q --batch --load stepX_YYY.el
# with full readline support
rlwrap emacs -Q --batch --load stepX_YYY.el
```

### Elixir

The Elixir implementation of mal has been tested with Elixir 1.0.5.

```
cd impls/elixir
mix stepX_YYY
# Or with readline/line editing functionality:
iex -S mix stepX_YYY
```

### Elm

The Elm implementation of mal has been tested with Elm 0.18.0

```
cd impls/elm
make stepX_YYY.js
STEP=stepX_YYY ./run
```

### Erlang

The Erlang implementation of mal requires [Erlang/OTP R17](http://www.erlang.org/download.html)
and [rebar](https://github.com/rebar/rebar) to build.

```
cd impls/erlang
make
    # OR
MAL_STEP=stepX_YYY rebar compile escriptize # build individual step
./stepX_YYY
```

### ES6 (ECMAScript 2015)

The ES6 / ECMAScript 2015 implementation uses the
[babel](https://babeljs.io) compiler to generate ES5 compatible
JavaScript. The generated code has been tested with Node 0.12.4.

```
cd impls/es6
make
node build/stepX_YYY.js
```


### F# ###

The F# implementation of mal has been tested on Linux using the Mono
F# compiler (fsharpc) and the Mono runtime (version 3.12.1). The mono C#
compiler (mcs) is also necessary to compile the readline dependency. All are
required to build and run the F# implementation.

```
cd impls/fsharp
make
mono ./stepX_YYY.exe
```

### Factor

The Factor implementation of mal has been tested with Factor 0.97
([factorcode.org](http://factorcode.org)).

```
cd impls/factor
FACTOR_ROOTS=. factor -run=stepX_YYY
```

### Fantom

The Fantom implementation of mal has been tested with Fantom 1.0.70.

```
cd impls/fantom
make lib/fan/stepX_YYY.pod
STEP=stepX_YYY ./run
```

### Fennel

The Fennel implementation of mal has been tested with Fennel version
0.9.1 on Lua 5.4.

```
cd impls/fennel
fennel ./stepX_YYY.fnl
```

### Forth

```
cd impls/forth
gforth stepX_YYY.fs
```

### GNU Guile 2.1+

```
cd impls/guile
guile -L ./ stepX_YYY.scm
```

### GNU Smalltalk

The Smalltalk implementation of mal has been tested with GNU Smalltalk 3.2.91.

```
cd impls/gnu-smalltalk
./run
```

### Go

The Go implementation of mal requires that go is installed on on the
path. The implementation has been tested with Go 1.3.1.

```
cd impls/go
make
./stepX_YYY
```


### Groovy

The Groovy implementation of mal requires Groovy to run and has been
tested with Groovy 1.8.6.

```
cd impls/groovy
make
groovy ./stepX_YYY.groovy
```

### Haskell

The Haskell implementation requires the ghc compiler version 7.10.1 or
later and also the Haskell parsec and readline (or editline) packages.

```
cd impls/haskell
make
./stepX_YYY
```

### Haxe (Neko, Python, C++ and JavaScript)

The Haxe implementation of mal requires Haxe version 3.2 to compile.
Four different Haxe targets are supported: Neko, Python, C++, and
JavaScript.

```
cd impls/haxe
# Neko
make all-neko
neko ./stepX_YYY.n
# Python
make all-python
python3 ./stepX_YYY.py
# C++
make all-cpp
./cpp/stepX_YYY
# JavaScript
make all-js
node ./stepX_YYY.js
```

### Hy

The Hy implementation of mal has been tested with Hy 0.13.0.

```
cd impls/hy
./stepX_YYY.hy
```

### Io

The Io implementation of mal has been tested with Io version 20110905.

```
cd impls/io
io ./stepX_YYY.io
```

### Janet

The Janet implementation of mal has been tested with Janet version 1.12.2.

```
cd impls/janet
janet ./stepX_YYY.janet
```

### Java 1.7

The Java implementation of mal requires maven2 to build.

```
cd impls/java
mvn compile
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY
    # OR
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY -Dexec.args="CMDLINE_ARGS"
```

### Java, using Truffle for GraalVM

This Java implementation will run on OpenJDK, but can run
as much as 30x faster on GraalVM thanks to the Truffle framework.
It's been tested with OpenJDK 11, GraalVM CE 20.1.0, and
GraalVM CE 21.1.0.

```
cd impls/java-truffle
./gradlew build
STEP=stepX_YYY ./run
```

### JavaScript/Node

```
cd impls/js
npm install
node stepX_YYY.js
```

### Julia

The Julia implementation of mal requires Julia 0.4.

```
cd impls/julia
julia stepX_YYY.jl
```

### jq

Tested against version 1.6, with a lot of cheating in the IO department

```
cd impls/jq
STEP=stepA_YYY ./run
    # with Debug
DEBUG=true STEP=stepA_YYY ./run
```

### Kotlin

The Kotlin implementation of mal has been tested with Kotlin 1.0.

```
cd impls/kotlin
make
java -jar stepX_YYY.jar
```

### LiveScript

The LiveScript implementation of mal has been tested with LiveScript 1.5.

```
cd impls/livescript
make
node_modules/.bin/lsc stepX_YYY.ls
```

### Logo

The Logo implementation of mal has been tested with UCBLogo 6.0.

```
cd impls/logo
logo stepX_YYY.lg
```

### Lua

The Lua implementation of mal has been tested with Lua 5.3.5 The
implementation requires luarocks to be installed.

```
cd impls/lua
make  # to build and link linenoise.so and rex_pcre.so
./stepX_YYY.lua
```

### Mal

Running the mal implementation of mal involves running stepA of one of
the other implementations and passing the mal step to run as a command
line argument.

```
cd impls/IMPL
IMPL_STEPA_CMD ../mal/stepX_YYY.mal

```

### GNU Make 3.81

```
cd impls/make
make -f stepX_YYY.mk
```

### NASM

The NASM implementation of mal is written for x86-64 Linux, and has been tested
with Linux 3.16.0-4-amd64 and NASM version 2.11.05.

```
cd impls/nasm
make
./stepX_YYY
```

### Nim 1.0.4

The Nim implementation of mal has been tested with Nim 1.0.4.

```
cd impls/nim
make
  # OR
nimble build
./stepX_YYY
```

### Object Pascal

The Object Pascal implementation of mal has been built and tested on
Linux using the Free Pascal compiler version 2.6.2 and 2.6.4.

```
cd impls/objpascal
make
./stepX_YYY
```

### Objective C

The Objective C implementation of mal has been built and tested on
Linux using clang/LLVM 3.6. It has also been built and tested on OS
X using XCode 7.

```
cd impls/objc
make
./stepX_YYY
```

### OCaml 4.01.0

```
cd impls/ocaml
make
./stepX_YYY
```

### MATLAB (GNU Octave and MATLAB)

The MatLab implementation has been tested with GNU Octave 4.2.1.
It has also been tested with MATLAB version R2014a on Linux. Note that
MATLAB is a commercial product.

```
cd impls/matlab
./stepX_YYY
octave -q --no-gui --no-history --eval "stepX_YYY();quit;"
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY();quit;"
    # OR with command line arguments
octave -q --no-gui --no-history --eval "stepX_YYY('arg1','arg2');quit;"
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY('arg1','arg2');quit;"
```

### miniMAL

[miniMAL](https://github.com/kanaka/miniMAL) is small Lisp interpreter
implemented in less than 1024 bytes of JavaScript. To run the miniMAL
implementation of mal you need to download/install the miniMAL
interpreter (which requires Node.js).
```
cd impls/miniMAL
# Download miniMAL and dependencies
npm install
export PATH=`pwd`/node_modules/minimal-lisp/:$PATH
# Now run mal implementation in miniMAL
miniMAL ./stepX_YYY
```

### Perl 5

The Perl 5 implementation should work with perl 5.19.3 and later.

For readline line editing support, install Term::ReadLine::Perl or
Term::ReadLine::Gnu from CPAN.

```
cd impls/perl
perl stepX_YYY.pl
```

### Perl 6

The Perl 6 implementation was tested on Rakudo Perl 6 2016.04.

```
cd impls/perl6
perl6 stepX_YYY.pl
```

### PHP 5.3

The PHP implementation of mal requires the php command line interface
to run.

```
cd impls/php
php stepX_YYY.php
```

### Picolisp

The Picolisp implementation requires libreadline and Picolisp 3.1.11
or later.

```
cd impls/picolisp
./run
```

### Pike

The Pike implementation was tested on Pike 8.0.

```
cd impls/pike
pike stepX_YYY.pike
```

### PL/pgSQL (PostgreSQL SQL Procedural Language)

The PL/pgSQL implementation of mal requires a running PostgreSQL server
(the "kanaka/mal-test-plpgsql" docker image automatically starts
a PostgreSQL server). The implementation connects to the PostgreSQL server
and create a database named "mal" to store tables and stored
procedures. The wrapper script uses the psql command to connect to the
server and defaults to the user "postgres" but this can be overridden
with the PSQL_USER environment variable. A password can be specified
using the PGPASSWORD environment variable. The implementation has been
tested with PostgreSQL 9.4.

```
cd impls/plpgsql
./wrap.sh stepX_YYY.sql
    # OR
PSQL_USER=myuser PGPASSWORD=mypass ./wrap.sh stepX_YYY.sql
```

### PL/SQL (Oracle SQL Procedural Language)

The PL/SQL implementation of mal requires a running Oracle DB
server (the "kanaka/mal-test-plsql" docker image automatically
starts an Oracle Express server). The implementation connects to the
Oracle server to create types, tables and stored procedures. The
default SQL\*Plus logon value (username/password@connect_identifier) is
"system/oracle" but this can be overridden with the ORACLE_LOGON
environment variable. The implementation has been tested with Oracle
Express Edition 11g Release 2. Note that any SQL\*Plus connection
warnings (user password expiration, etc) will interfere with the
ability of the wrapper script to communicate with the DB.

```
cd impls/plsql
./wrap.sh stepX_YYY.sql
    # OR
ORACLE_LOGON=myuser/mypass@ORCL ./wrap.sh stepX_YYY.sql
```

### PostScript Level 2/3

The PostScript implementation of mal requires Ghostscript to run. It
has been tested with Ghostscript 9.10.

```
cd impls/ps
gs -q -dNODISPLAY -I./ stepX_YYY.ps
```

### PowerShell

The PowerShell implementation of mal requires the PowerShell script
language. It has been tested with PowerShell 6.0.0 Alpha 9 on Linux.

```
cd impls/powershell
powershell ./stepX_YYY.ps1
```

### Prolog

The Prolog implementation uses some constructs specific to SWI-Prolog,
includes readline support and has been tested on Debian GNU/Linux with
version 8.2.1.

```
cd impls/prolog
swipl stepX_YYY
```

### PureScript
The PureScript implementation requires the spago compiler version 0.20.2.

```
cd impls/purs
make
node ./stepX_YYY.js
```

### Python (2.X and 3.X)

```
cd impls/python
python stepX_YYY.py
```

### Python.2 (3.X)

The second Python implementation makes heavy use of type annotations and uses the Arpeggio parser library.

```
# Recommended: do these steps in a Python virtual environment.
pip3 install Arpeggio==1.9.0
python3 stepX_YYY.py
```

### RPython

You must have [rpython](https://rpython.readthedocs.org/) on your path
(included with [pypy](https://bitbucket.org/pypy/pypy/)).

```
cd impls/rpython
make        # this takes a very long time
./stepX_YYY
```

### R

The R implementation of mal requires R (r-base-core) to run.

```
cd impls/r
make libs  # to download and build rdyncall
Rscript stepX_YYY.r
```

### Racket (5.3)

The Racket implementation of mal requires the Racket
compiler/interpreter to run.

```
cd impls/racket
./stepX_YYY.rkt
```

### Rexx

The Rexx implementation of mal has been tested with Regina Rexx 3.6.

```
cd impls/rexx
make
rexx -a ./stepX_YYY.rexxpp
```

### Ruby (1.9+)

```
cd impls/ruby
ruby stepX_YYY.rb
```

### Ruby #2

A second Ruby implementation with the following goals:

- No global variables
- No modification (monkey-patching) of core Ruby classes
- Modularized into the `Mal` module namespace

```
cd impls/ruby.2
ruby stepX_YYY.rb
```

### Rust (1.38+)

The rust implementation of mal requires the rust compiler and build
tool (cargo) to build.

```
cd impls/rust
cargo run --release --bin stepX_YYY
```

### Scala ###

Install scala and sbt (http://www.scala-sbt.org/0.13/tutorial/Installing-sbt-on-Linux.html):

```
cd impls/scala
sbt 'run-main stepX_YYY'
    # OR
sbt compile
scala -classpath target/scala*/classes stepX_YYY
```

### Scheme (R7RS) ###

The Scheme implementation of MAL has been tested with Chibi-Scheme
0.10, Kawa 3.1.1, Gauche 0.9.6, CHICKEN 5.1.0, Sagittarius 0.9.7,
Cyclone 0.32.0 (Git version), Foment 0.4 (Git version) and Guile 3.0.1.  You should
be able to get it running on other conforming R7RS implementations
after figuring out how libraries are loaded and adjusting the
`Makefile` and `run` script accordingly.

```
cd impls/scheme
# chibi
scheme_MODE=chibi ./run
# kawa
make kawa
scheme_MODE=kawa ./run
# gauche
scheme_MODE=gauche ./run
# chicken
make chicken
scheme_MODE=chicken ./run
# sagittarius
scheme_MODE=sagittarius ./run
# cyclone
make cyclone
scheme_MODE=cyclone ./run
# foment
scheme_MODE=foment ./run
# guile
scheme_MODE=guile ./run
```

### Skew ###

The Skew implementation of mal has been tested with Skew 0.7.42.

```
cd impls/skew
make
node stepX_YYY.js
```


### Standard ML (Poly/ML, MLton, Moscow ML)

The Standard ML implementation of mal requires an
[SML97](https://github.com/SMLFamily/The-Definition-of-Standard-ML-Revised)
implementation. The Makefile supports Poly/ML, MLton, Moscow ML, and has
been tested with Poly/ML 5.8.1, MLton 20210117, and Moscow ML version 2.10.

```
cd impls/sml
# Poly/ML
make sml_MODE=polyml
./stepX_YYY
# MLton
make sml_MODE=mlton
./stepX_YYY
# Moscow ML
make sml_MODE=mosml
./stepX_YYY
```


### Swift

The Swift implementation of mal requires the Swift 2.0 compiler (XCode
7.0) to build. Older versions will not work due to changes in the
language and standard library.

```
cd impls/swift
make
./stepX_YYY
```

### Swift 3

The Swift 3 implementation of mal requires the Swift 3.0 compiler. It
has been tested with Swift 3 Preview 3.

```
cd impls/swift3
make
./stepX_YYY
```

### Swift 4

The Swift 4 implementation of mal requires the Swift 4.0 compiler. It
has been tested with Swift 4.2.3 release.

```
cd impls/swift4
make
./stepX_YYY
```

### Swift 5

The Swift 5 implementation of mal requires the Swift 5.0 compiler. It
has been tested with Swift 5.1.1 release.

```
cd impls/swift5
swift run stepX_YYY
```

### Tcl 8.6

The Tcl implementation of mal requires Tcl 8.6 to run.  For readline line
editing support, install tclreadline.

```
cd impls/tcl
tclsh ./stepX_YYY.tcl
```

### TypeScript

The TypeScript implementation of mal requires the TypeScript 2.2 compiler.
It has been tested with Node.js v6.

```
cd impls/ts
make
node ./stepX_YYY.js
```

### Vala

The Vala implementation of mal has been tested with the Vala 0.40.8
compiler. You will need to install `valac` and `libreadline-dev` or
equivalent.

```
cd impls/vala
make
./stepX_YYY
```

### VHDL

The VHDL implementation of mal has been tested with GHDL 0.29.

```
cd impls/vhdl
make
./run_vhdl.sh ./stepX_YYY
```

### Vimscript

The Vimscript implementation of mal requires Vim 8.0 to run.

```
cd impls/vimscript
./run_vimscript.sh ./stepX_YYY.vim
```

### Visual Basic.NET ###

The VB.NET implementation of mal has been tested on Linux using the Mono
VB compiler (vbnc) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the VB.NET implementation.

```
cd impls/vb
make
mono ./stepX_YYY.exe
```

### WebAssembly (wasm) ###

The WebAssembly implementation is written in
[Wam](https://github.com/kanaka/wam) (WebAssembly Macro language) and
runs under several different non-web embeddings (runtimes):
[node](https://nodejs.org),
[wasmtime](https://github.com/CraneStation/wasmtime),
[wasmer](https://wasmer.io),
[lucet](https://github.com/fastly/lucet),
[wax](https://github.com/kanaka/wac),
[wace](https://github.com/kanaka/wac),
[warpy](https://github.com/kanaka/warpy).

```
cd impls/wasm
# node
make wasm_MODE=node
./run.js ./stepX_YYY.wasm
# wasmtime
make wasm_MODE=wasmtime
wasmtime --dir=./ --dir=../ --dir=/ ./stepX_YYY.wasm
# wasmer
make wasm_MODE=wasmer
wasmer run --dir=./ --dir=../ --dir=/ ./stepX_YYY.wasm
# lucet
make wasm_MODE=lucet
lucet-wasi --dir=./:./ --dir=../:../ --dir=/:/ ./stepX_YYY.so
# wax
make wasm_MODE=wax
wax ./stepX_YYY.wasm
# wace
make wasm_MODE=wace_libc
wace ./stepX_YYY.wasm
# warpy
make wasm_MODE=warpy
warpy --argv --memory-pages 256 ./stepX_YYY.wasm
```

### XSLT

The XSLT implementation of mal is written with XSLT 3 and tested on Saxon 9.9.1.6 Home Edition.

```
cd impls/xslt
STEP=stepX_YY ./run
```

### Wren

The Wren implementation of mal was tested on Wren 0.2.0.

```
cd impls/wren
wren ./stepX_YYY.wren
```

### Yorick

The Yorick implementation of mal was tested on Yorick 2.2.04.

```
cd impls/yorick
yorick -batch ./stepX_YYY.i
```

### Zig

The Zig implementation of mal was tested on Zig 0.5.

```
cd impls/zig
zig build stepX_YYY
```



## Running tests

The top level Makefile has a number of useful targets to assist with
implementation development and testing. The `help` target provides
a list of the targets and options:

```
make help
```

### Functional tests

The are almost 800 generic functional tests (for all implementations)
in the `tests/` directory. Each step has a corresponding test file
containing tests specific to that step. The `runtest.py` test harness
launches a Mal step implementation and then feeds the tests one at
a time to the implementation and compares the output/return value to
the expected output/return value.

* To run all the tests across all implementations (be prepared to wait):

```
make test
```

* To run all tests against a single implementation:

```
make "test^IMPL"

# e.g.
make "test^clojure"
make "test^js"
```

* To run tests for a single step against all implementations:

```
make "test^stepX"

# e.g.
make "test^step2"
make "test^step7"
```

* To run tests for a specific step against a single implementation:

```
make "test^IMPL^stepX"

# e.g
make "test^ruby^step3"
make "test^ps^step4"
```

### Self-hosted functional tests

* To run the functional tests in self-hosted mode, you specify `mal`
  as the test implementation and use the `MAL_IMPL` make variable
  to change the underlying host language (default is JavaScript):
```
make MAL_IMPL=IMPL "test^mal^step2"

# e.g.
make "test^mal^step2"   # js is default
make MAL_IMPL=ruby "test^mal^step2"
make MAL_IMPL=python "test^mal^step2"
```

### Starting the REPL

* To start the REPL of an implementation in a specific step:

```
make "repl^IMPL^stepX"

# e.g
make "repl^ruby^step3"
make "repl^ps^step4"
```

* If you omit the step, then `stepA` is used:

```
make "repl^IMPL"

# e.g
make "repl^ruby"
make "repl^ps"
```

* To start the REPL of the self-hosted implementation, specify `mal` as the
  REPL implementation and use the `MAL_IMPL` make variable to change the
  underlying host language (default is JavaScript):
```
make MAL_IMPL=IMPL "repl^mal^stepX"

# e.g.
make "repl^mal^step2"   # js is default
make MAL_IMPL=ruby "repl^mal^step2"
make MAL_IMPL=python "repl^mal"
```

### Performance tests

Warning: These performance tests are neither statistically valid nor
comprehensive; runtime performance is a not a primary goal of mal. If
you draw any serious conclusions from these performance tests, then
please contact me about some amazing oceanfront property in Kansas
that I'm willing to sell you for cheap.

* To run performance tests against a single implementation:
```
make "perf^IMPL"

# e.g.
make "perf^js"
```

* To run performance tests against all implementations:
```
make "perf"
```

### Generating language statistics

* To report line and byte statistics for a single implementation:
```
make "stats^IMPL"

# e.g.
make "stats^js"
```

## Dockerized testing

Every implementation directory contains a Dockerfile to create
a docker image containing all the dependencies for that
implementation. In addition, the top-level Makefile contains support
for running the tests target (and perf, stats, repl, etc) within
a docker container for that implementation by passing *"DOCKERIZE=1"*
on the make command line. For example:

```
make DOCKERIZE=1 "test^js^step3"
```

Existing implementations already have docker images built and pushed
to the docker registry. However, if
you wish to build or rebuild a docker image locally, the toplevel
Makefile provides a rule for building docker images:

```
make "docker-build^IMPL"
```


**Notes**:
* Docker images are named *"kanaka/mal-test-IMPL"*
* JVM-based language implementations (Groovy, Java, Clojure, Scala):
  you will probably need to run this command once manually
  first `make DOCKERIZE=1 "repl^IMPL"` before you can run tests because
  runtime dependencies need to be downloaded to avoid the tests timing
  out. These dependencies are downloaded to dot-files in the /mal
  directory so they will persist between runs.


## License

Mal (make-a-lisp) is licensed under the MPL 2.0 (Mozilla Public
License 2.0). See LICENSE.txt for more details.

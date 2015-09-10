# mal - Make a Lisp

## Description

Mal is a Clojure inspired Lisp interpreter.

Mal is implemented in 40 different languages:

* GNU awk
* Bash shell
* C
* C++
* C#
* Clojure
* CoffeeScript
* Crystal
* Elixir
* Erlang
* ES6 (ECMAScript 6 / ECMAScript 2015)
* F#
* Factor
* Forth
* Go
* Groovy
* GNU Guile
* Haskell
* Java
* JavaScript ([Online Demo](http://kanaka.github.io/mal))
* Julia
* Lua
* GNU Make
* mal itself
* MATLAB
* [miniMAL](https://github.com/kanaka/miniMAL)
* Nim
* OCaml
* Perl
* PHP
* Postscript
* Python
* RPython
* R
* Racket
* Ruby
* Rust
* Scala
* Swift
* Visual Basic.NET


Mal is a learning tool. See the [make-a-lisp process
guide](process/guide.md). Each implementation of mal is separated into
11 incremental, self-contained (and testable) steps that demonstrate
core concepts of Lisp. The last step is capable of self-hosting
(running the mal implementation of mal).

The mal (make a lisp) steps are:

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


Mal was presented publicly for the first time in a lightning talk at
Clojure West 2014 (unfortunately there is no video). See
mal/clojurewest2014.mal for the presentation that was given at the
conference (yes the presentation is a mal program).

If you are interesting in creating a mal implementation (or just
interested in using mal for something), please drop by the #mal
channel on freenode. In addition to the [make-a-lisp process
guide](process/guide.md) there is also a [mal/make-a-lisp
FAQ](docs/FAQ.md) where I attempt to answer some common questions.

## Building/running implementations

### GNU awk

*The GNU awk implemenation was created by [Miutsuru kariya](https://github.com/kariya-mitsuru)*

The GNU awk implementation of mal has been tested with GNU awk 4.1.1.

```
cd gawk
gawk -O -f stepX_YYY.awk
```

### Bash 4

```
cd bash
bash stepX_YYY.sh
```

### C

The C implementation of mal requires the following libraries (lib and
header packages): glib, libffi6 and either the libedit or GNU readline library.

```
cd c
make
./stepX_YYY
```

### C++

*The C++ implementation was created by [Stephen Thirlwall (sdt)](https://github.com/sdt)*

The C++ implementation of mal requires g++-4.9 or clang++-3.5 and
a readline compatible library to build. See the `cpp/README.md` for
more details:

```
cd cpp
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
cd cs
make
mono ./stepX_YYY.exe
```


### Clojure

```
cd clojure
lein with-profile +stepX trampoline run
```

### CoffeeScript

```
sudo npm install -g coffee-script
cd coffee
coffee ./stepX_YYY
```

### Crystal

*The Crystal implementation of mal was created by [Linda_pp](https://github.com/rhysd)*

The Crystal implemenation of mal has been tested with Crystal 0.7.2.

```
cd crystal
crystal run ./stepX_YYY.cr
    # OR
make   # needed to run tests
./stepX_YYY
```

### Elixir

*The Elixir implementation was created by [Martin Ek (ekmartin)](https://github.com/ekmartin)*

The Elixir implementation of mal has been tested with Elixir 1.0.5.

```
cd elixir
mix stepX_YYY
# Or with readline/line editing functionality:
iex -S mix stepX_YYY
```

### Erlang

*The Erlang implementation was created by [Nathan Fiedler (nlfiedler)](https://github.com/nlfiedler)*

The Erlang implementation of mal requires [Erlang/OTP R17](http://www.erlang.org/download.html) and [rebar](https://github.com/rebar/rebar) to build.

```
cd erlang
make
    # OR
MAL_STEP=stepX_YYY rebar compile escriptize # build individual step
./stepX_YYY
```

### ES6 (ECMAScript 6 / ECMAScript 2015)

The ES6 implementation uses the [babel](https://babeljs.io) compiler
to generate ES5 compatible JavaScript. The generated code has been
tested with Node 0.12.4.

```
cd es6
make
node build/stepX_YYY.js
```


### F# ###

*The F# implementation was created by [Peter Stephens (pstephens)](https://github.com/pstephens)*

The F# implementation of mal has been tested on Linux using the Mono
F# compiler (fsharpc) and the Mono runtime (version 3.12.1). The mono C#
compiler (mcs) is also necessary to compile the readline dependency. All are
required to build and run the F# implementation.

```
cd fsharp
make
mono ./stepX_YYY.exe
```

### Factor

*The Factor implementation was created by [Jordan Lewis (jordanlewis)](https://github.com/jordanlewis)*

The Factor implementation of mal has been tested with Factor 0.97
([factorcode.org](factorcode.org)).

```
cd factor
FACTOR_ROOTS=src factor -run=stepX_YYY
```

### Forth

*The Forth implementation was created by [Chris Houser (chouser)](https://github.com/chouser)*

```
cd forth
gforth stepX_YYY.fs
```

### Go

The Go implementation of mal requires that go is installed on on the
path. The implementation has been tested with Go 1.3.1.

```
cd go
make
./stepX_YYY
```


### Groovy

The Groovy implementation of mal requires Groovy to run and has been
tested with Groovy 1.8.6.

```
cd groovy
make
groovy ./stepX_YYY.groovy
```

### GNU Guile 2.1+

*The Guile implementation was created by [Mu Lei (NalaGinrut)](https://github.com/NalaGinrut).*

```
cd guile
guile -L ./ stepX_YYY.scm
```

### Haskell

Install the Haskell compiler (ghc/ghci), the Haskell platform and
either the editline package (BSD) or the readline package (GPL). On
Ubuntu these packages are: ghc, haskell-platform,
libghc-readline-dev/libghc-editline-dev

```
cd haskell
make
./stepX_YYY
```


### Java 1.7

The Java implementation of mal requires maven2 to build.

```
cd java
mvn compile
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY
    # OR
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY -Dexec.args="CMDLINE_ARGS"
```

### JavaScript/Node

```
cd js
npm update
node stepX_YYY.js
```

### Julia

The Julia implementation of mal has been tested with Julia 0.3.7.

```
cd julia
julia stepX_YYY.jl
```

### Lua

Running the Lua implementation of mal requires lua 5.1 or later,
luarocks and the lua-rex-pcre library installed.

```
cd lua
make  # to build and link linenoise.so
./stepX_YYY.lua
```

### Mal

Running the mal implementation of mal involves running stepA of one of
the other implementations and passing the mal step to run as a command
line argument.

```
cd IMPL
IMPL_STEPA_CMD ../mal/stepX_YYY.mal

```

### GNU Make 3.81

```
cd make
make -f stepX_YYY.mk
```

### Nim 0.11.0

*The Nim implementation was created by [Dennis Felsing (def-)](https://github.com/def-)*

Running the Nim implementation of mal requires Nim 0.11.0 or later.

```
cd nim
make
  # OR
nimble build
./stepX_YYY
```

### OCaml 4.01.0

*The OCaml implementation was created by [Chris Houser (chouser)](https://github.com/chouser)*

```
cd ocaml
make
./stepX_YYY
```

### MATLAB

The MATLAB implementation of mal has been tested with MATLAB version
R2014a on Linux. Note that MATLAB is a commercial product. It should
be fairly simple to support GNU Octave once it support classdef object
syntax.

```
cd matlab
./stepX_YYY
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY();quit;"
    # OR with command line arguments
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY('arg1','arg2');quit;"
```

### miniMAL

[miniMAL](https://github.com/kanaka/miniMAL) is small Lisp interpreter
implemented in less than 1024 bytes of JavaScript. To run the miniMAL
implementation of mal you need to download/install the miniMAL
interpreter (which requires Node.js).
```
cd miniMAL
# Download miniMAL and dependencies
npm install
export PATH=`pwd`/node_modules/minimal-lisp/:$PATH
# Now run mal implementation in miniMAL
miniMAL ./stepX_YYY
```

### Perl 5.8

For readline line editing support, install Term::ReadLine::Perl or
Term::ReadLine::Gnu from CPAN.

```
cd perl
perl stepX_YYY.pl
```


### PHP 5.3

The PHP implementation of mal requires the php command line interface
to run.

```
cd php
php stepX_YYY.php
```

### Postscript Level 2/3

The Postscript implementation of mal requires ghostscript to run. It
has been tested with ghostscript 9.10.

```
cd ps
gs -q -dNODISPLAY -I./ stepX_YYY.ps
```

### Python (2.X or 3.X)

```
cd python
python stepX_YYY.py
```

### RPython

You must have [rpython](https://rpython.readthedocs.org/) on your path
(included with [pypy](https://bitbucket.org/pypy/pypy/)).

```
cd rpython
make        # this takes a long time
./stepX_YYY
```

### R

The R implementation of mal requires R (r-base-core) to run.

```
cd r
make libs  # to download and build rdyncall
Rscript stepX_YYY.r
```

### Racket (5.3)

The Racket implementation of mal requires the Racket
compiler/interpreter to run.

```
cd racket
./stepX_YYY.rkt
```

### Ruby (1.9+)

```
cd ruby
ruby stepX_YYY.rb
```

### Rust (1.0.0 nightly)

The rust implementation of mal requires the rust compiler and build
tool (cargo) to build.

```
cd rust
cargo run --release --bin stepX_YYY
```

### Scala ###

Install scala and sbt (http://www.scala-sbt.org/0.13/tutorial/Installing-sbt-on-Linux.html):

```
cd scala
sbt 'run-main stepX_YYY'
    # OR
sbt compile
scala -classpath target/scala*/classes stepX_YYY
```

### Swift

*The Swift implementation was created by [Keith Rollin](https://github.com/keith-rollin)*

The Swift implemenation of mal requires the Swift 1.2 compiler (XCode
6.3) to build.

```
cd swift
make
./stepX_YYY
```

### Visual Basic.NET ###

The VB.NET implementation of mal has been tested on Linux using the Mono
VB compiler (vbnc) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the VB.NET implementation.

```
cd vb
make
mono ./stepX_YYY.exe
```



## Running tests

### Functional tests

The are nearly 500 generic functional tests (for all implementations)
in the `tests/` directory. Each step has a corresponding test file
containing tests specific to that step. The `runtest.py` test harness
launches a Mal step implementation and then feeds the tests one at
a time to the implementation and compares the output/return value to
the expected output/return value.

To simplify the process of running tests, a top level Makefile is
provided with convenient test targets.

* To run all the tests across all implementations (be prepared to wait):

```
make test
```

* To run all tests against a single implementation:

```
make test^IMPL

# e.g.
make test^clojure
make test^js
```

* To run tests for a single step against all implementations:

```
make test^stepX

# e.g.
make test^step2
make test^step7
```

* To run tests for a specifc step against a single implementation:

```
make test^IMPL^stepX

# e.g
make test^ruby^step3
make test^ps^step4
```

### Self-hosted functional tests

* To run the functional tests in self-hosted mode, you specify `mal`
  as the test implementation and use the `MAL_IMPL` make variable
  to change the underlying host language (default is JavaScript):
```
make MAL_IMPL=IMPL test^mal^step2

# e.g.
make test^mal^step2   # js is default
make MAL_IMPL=ruby test^mal^step2
make MAL_IMPL=python test^mal^step2
```


### Performance tests

Warning: These performance tests are neither statistically valid nor
comprehensive; runtime performance is a not a primary goal of mal. If
you draw any serious conclusions from these performance tests, then
please contact me about some amazing oceanfront property in Kansas
that I'm willing to sell you for cheap.

* To run performance tests against a single implementation:
```
make perf^IMPL

# e.g.
make perf^js
```

* To run performance tests against all implementations:
```
make perf
```

### Generating language statistics

* To report line and byte stastics for a single implementation:
```
make stats^IMPL

# e.g.
make stats^js
```

* To report line and bytes stastics for general Lisp code (env, core
  and stepA):
```
make stats-lisp^IMPL

# e.g.
make stats-lisp^js
```

## Docker test environment

There is a Dockerfile included in the `tests/docker` directory that
builds a docker image based on Ubuntu Utopic that contains everything
needed to run tests against all the implementations (except for MATLAB
which is proprietary/licensed).

Build the the docker image using a provided script. WARNING: this will
likely take over an hour to build from scratch and use more 3 GB of disk:
```bash
./tests/docker-build.sh
```

Launch a docker container from that image built above. This will
volume mount the mal directory to `/mal` and then give you a bash
prompt in the container. You can then run individual mal
implementations and tests:
```bash
./tests/docker-run.sh
```

You can also specify a command to run within the container. For
example, to run step2 tests for every implementation (except MATLAB):
```bash
./tests/docker-run.sh make SKIP_IMPLS="matlab" test^step2
```

**Notes**:
* JVM-based language implementations (Java, Clojure, Scala): you will
  need to run these implementations once manually first before you can
  run tests because runtime dependencies need to be downloaded to
  avoid the tests timing out. These dependencies are download to
  dot-files in the /mal directory so they will persist between runs.
* Compiled languages: if your host system is different enough from
  Ubuntu Utopic then you may need to re-compile your compiled
  languages from within the container to avoid linker version
  mismatches.


## License

Mal (make-a-lisp) is licensed under the MPL 2.0 (Mozilla Public
License 2.0). See LICENSE.txt for more details.

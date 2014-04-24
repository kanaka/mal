# mal - Make a Lisp

## Description

Mal is an interpreter for a subset of the Clojure programming
language. Mal is implemented from scratch in 13 different languages:

* Bash shell
* C
* C#
* Clojure
* Java
* Javascript
* GNU Make
* mal itself
* Perl
* PHP
* Postscript
* Python
* Ruby


Mal is also a learning tool. Each implentation of mal is separated
into 11 incremental, self-contained (and testable) steps that
demonstrate core concepts of Lisp. The last step is capable of
self-hosting (running the mal implemenation of mal).

The mal (make a lisp) steps are:

* step0_repl
* step1_read_print
* step2_eval
* step3_env
* step4_if_fn_do
* step5_tco
* step6_file
* step7_quote
* step8_macros
* step9_interop
* stepA_more


Mal was presented publicly for the first time in a lightning talk at
Clojure West 2014 (unfortunately there is no video). See
mal/clojurewest2014.mal for the presentation that was given at the
conference (yes the presentation is a mal program).

## Building/running implementations

### Bash 4

```
cd bash
bash stepX_YYY.sh
```

### C

The C implementation of mal requires the following libraries: glib,
libffi6 and either the libedit or GNU readline library.

```
cd c
make
./stepX_YYY
```

### C# ###

The C# implementation of mal has been tested on Linux using the Mono
C# compiler (mcs) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the C# implementation.

```
cd cs
make
mono ./stepX_YYY
```


### Clojure

```
cd clojure
lein with-profile +stepX trampoline run
```

### Java 1.7

```
cd java
mvn compile
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY
    # OR
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY -Dexec.args="CMDLINE_ARGS"
```

### Javascript/Node

```
cd js
npm update
node stepX_YYY.js
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

### Perl 5.8

For readline line editing support, install Term::ReadLine::Perl or
Term::ReadLine::Gnu from CPAN.

```
cd perl
perl stepX_YYY.pl
```


### PHP 5.3

```
cd php
php stepX_YYY.php
```

### Postscript Level 2/3

```
cd ps
gs -q -dNODISPLAY stepX_YYY.ps
```

### Python (2 or 3)

```
cd python
python stepX_YYY.py
```

### Ruby (1.8)

```
cd ruby
ruby stepX_YYY.rb
```

## Running tests

The are nearly 400 generic Mal tests (for all implementations) in the
`tests/` directory. Each step has a corresponding test file containing
tests specific to that step. The `runtest.py` test harness uses
pexpect to launch a Mal step implementation and then feeds the tests
one at a time to the implementation and compares the output/return
value to the expected output/return value.

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

* To run a specifc step against a single implementation:

```
make test^IMPL^stepX

# e.g
make test^ruby^step3
make test^ps^step4
```

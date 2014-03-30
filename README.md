# mal - Make a Lisp

## Description

Mal is an interpreter for a subset of the Clojure programming
language. Mal is implemetated from scratch in 10 different languages:

* Javascript
* Python
* Clojure
* C
* Java
* PHP
* Bash shell
* GNU Make
* mal itself
* Postscript (in-progress)


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

### Python 2.X

```
cd python
python stepX_YYY.py
```

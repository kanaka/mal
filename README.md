# mal - Make a Lisp

See mal/presentation.mal

## Building/running implementations

* Bash 4

```
cd bash
bash stepX_YYY.sh
```

* C

```
cd c
make
./stepX_YYY
```

* Clojure

```
cd clojure
lein with-profile +stepX trampoline run
```

* Java 1.7

```
cd java
mvn compile
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY
    # OR
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY -Dexec.args="CMDLINE_ARGS"
```

* Javascript/Node

```
cd js
node stepX_YYY.js
```

* Mal

Running the mal implementation of mal involves running stepA of one of
the other implementations and passing the mal step to run as a command
line argument.

```
cd IMPL
IMPL_STEPA_CMD ../mal/stepX_YYY.mal

```

* GNU Make 3.81

```
cd make
make -f stepX_YYY.mk
```

* PHP 5.3

```
cd php
php stepX_YYY.php
```

* Postscript Level 2/3

```
cd ps
gs -q -dNODISPLAY stepX_YYY.ps
```

* Python 2.X

```
cd python
python stepX_YYY.py
```

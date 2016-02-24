
TESTS =


SOURCES_BASE = src/main/java/mal/readline.java src/main/java/mal/types.java \
               src/main/java/mal/reader.java src/main/java/mal/printer.java
SOURCES_LISP = src/main/java/mal/env.java src/main/java/mal/core.java \
               src/main/java/mal/stepA_mal.java
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	mvn install

dist: mal.jar mal

mal.jar: target/classes/mal/stepA_mal.class
	mvn assembly:assembly
	cp target/mal-0.0.1.jar $@

SHELL := bash
mal: mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') mal.jar > $@
	chmod +x mal

src/main/mal/%.java:
	mvn install

target/classes/mal/step%.class: src/main/mal/step%.java ${SOURCES}
	mvn install

clean:
	mvn clean
	rm -f mal.jar mal

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

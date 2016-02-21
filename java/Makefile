
TESTS =


SOURCES_BASE = src/main/java/mal/readline.java src/main/java/mal/types.java \
               src/main/java/mal/reader.java src/main/java/mal/printer.java
SOURCES_LISP = src/main/java/mal/env.java src/main/java/mal/core.java \
               src/main/java/mal/stepA_mal.java
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	mvn install

src/main/mal/%.java:
	mvn install

target/classes/mal/step%.class: src/main/mal/step%.java ${SOURCES}
	mvn install

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

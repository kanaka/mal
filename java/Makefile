
TESTS =


SOURCES_BASE = src/main/java/mal/readline.java src/main/java/mal/types.java \
               src/main/java/mal/reader.java src/main/java/mal/printer.java
SOURCES_LISP = src/main/java/mal/env.java src/main/java/mal/core.java \
               src/main/java/mal/stepA_mal.java
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

SOURCES_BASE = src/readline.clj src/reader.clj src/printer.clj
SOURCES_LISP = src/env.clj src/core.clj src/stepA_mal.clj
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: deps

dist: mal.jar mal

deps:
	lein deps

mal.jar: $(SOURCES)
	lein with-profile +jar uberjar
	cp target/mal-0.0.1-SNAPSHOT-standalone.jar $@

SHELL := bash
mal: mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') mal.jar > $@
	chmod +x mal

clean:
	rm -rf target/ mal.jar mal

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

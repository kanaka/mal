SOURCES_BASE = src/readline.clj src/reader.clj src/printer.clj
SOURCES_LISP = src/env.clj src/core.clj src/stepA_mal.clj
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	lein deps

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

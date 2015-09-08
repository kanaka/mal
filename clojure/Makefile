SOURCES_BASE = src/readline.clj src/reader.clj src/printer.clj
SOURCES_LISP = src/env.clj src/core.clj src/stepA_mal.clj
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

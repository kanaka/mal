
TESTS =

SOURCES_BASE = src/readline.clj src/reader.clj src/printer.clj
SOURCES_LISP = src/env.clj src/core.clj src/stepA_more.clj
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	lein with-profile XXX$@XXX trampoline run || exit 1; \

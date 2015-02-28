
TESTS =

SOURCES_BASE = readline.php types.php reader.php printer.php
SOURCES_LISP = env.php core.php stepA_mal.php
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	php $@ || exit 1; \

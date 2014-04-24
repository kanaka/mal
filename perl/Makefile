TESTS =

SOURCES_BASE = readline.pm types.pm reader.pm printer.pm \
               interop.pm
SOURCES_LISP = env.pm core.pm stepA_more.pl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#all: mal.pl

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	ruby $@ || exit 1; \

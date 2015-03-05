
TESTS =

SOURCES_BASE = types.ps reader.ps printer.ps
SOURCES_LISP = env.ps core.ps stepA_mal.ps
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	gs -q -dNODISPLAY -- $@ || exit 1; \

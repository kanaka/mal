
TESTS =

SOURCES_BASE =
SOURCES_LISP = env.mal core.mal stepA_more.mal
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

#tests: $(TESTS)
#
#$(TESTS):
#	@echo "Running $@"; \
#	python $@ || exit 1; \

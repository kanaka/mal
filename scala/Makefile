TESTS =

SOURCES_BASE = types.scala reader.scala printer.scala
SOURCES_LISP = env.scala core.scala stepA_mal.scala
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	sbt 'run-main stepA_mal ../tests/incA.mal'

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	ruby $@ || exit 1; \

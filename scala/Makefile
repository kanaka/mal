TESTS =

SOURCES_BASE = types.scala reader.scala printer.scala
SOURCES_LISP = env.scala core.scala stepA_mal.scala
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	sbt 'run-main stepA_mal ../tests/incA.mal'

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	ruby $@ || exit 1; \

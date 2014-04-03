
TESTS =

SOURCES = types.ps reader.ps printer.ps env.ps core.ps stepA_more.ps

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	gs -q -dNODISPLAY -- $@ || exit 1; \

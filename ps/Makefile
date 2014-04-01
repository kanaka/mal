
TESTS =

SOURCES = types.ps reader.ps step8_macros.ps

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	gs -q -dNODISPLAY -- $@ || exit 1; \

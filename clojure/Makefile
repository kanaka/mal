
TESTS =

SOURCES = src/types.clj src/readline.clj src/reader.clj src/stepA_more.clj

all:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	lein with-profile XXX$@XXX trampoline run || exit 1; \

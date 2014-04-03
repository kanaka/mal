
TESTS =

SOURCES = readline.php types.php reader.php printer.php \
	  env.php core.php stepA_more.php

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	php $@ || exit 1; \

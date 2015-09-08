
TESTS =


SOURCES_BASE = types.awk reader.awk printer.awk
SOURCES_LISP = env.awk core.awk stepA_mal.awk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	python $@ || exit 1; \

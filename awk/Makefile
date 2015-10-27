
TESTS =


SOURCES_BASE = types.awk reader.awk printer.awk
SOURCES_LISP = env.awk core.awk stepA_mal.awk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	python $@ || exit 1; \

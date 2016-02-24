
TESTS =

SOURCES_BASE = readline.php types.php reader.php printer.php
SOURCES_LISP = env.php core.php stepA_mal.php
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal.php mal

mal.php: $(SOURCES)
	cat $+ | grep -v "^require_once" > $@

mal: mal.php
	echo "#!/usr/bin/env php" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.php mal

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*//|^[[:space:]]/\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*//|^[[:space:]]/\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	php $@ || exit 1; \

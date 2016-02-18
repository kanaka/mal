TESTS =

SOURCES_BASE = readline.pm types.pm reader.pm printer.pm \
               interop.pm
SOURCES_LISP = env.pm core.pm stepA_mal.pl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal.pl

mal.pl: $(SOURCES)
	echo "#!/usr/bin/env perl" > $@
	fatpack pack stepA_mal.pl >> $@

clean:
	rm -f mal.pl

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
	ruby $@ || exit 1; \

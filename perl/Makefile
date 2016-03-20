TESTS =

SOURCES_BASE = readline.pm types.pm reader.pm printer.pm \
               interop.pm
SOURCES_LISP = env.pm core.pm stepA_mal.pl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal.pl mal

mal.pl: $(SOURCES)
	#fatpack pack ./stepA_mal.pl > $@
	fatpack trace ./stepA_mal.pl
	fatpack packlists-for `cat fatpacker.trace` > packlists
	fatpack tree `cat packlists`
	cp $+ fatlib/
	(fatpack file; cat ./stepA_mal.pl) > mal.pl

mal: mal.pl
	echo "#!/usr/bin/env perl" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.pl mal fatpacker.trace packlists fatlib/*
	[ -d fatlib ] && rmdir fatlib || true

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


TESTS =

SOURCES_BASE = types.ps reader.ps printer.ps
SOURCES_LISP = env.ps core.ps stepA_mal.ps
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.ps

mal.ps: $(SOURCES)
	echo "#!/bin/sh" > $@
	echo "\":\" pop pop pop pop %#; exec gs -d'#!'=null -d'\":\"'=null -q -dNODISPLAY -- \"\$$0\" \"\$$@\"" >> $@
	cat $+ | grep -v "runlibfile$$" >> $@
	chmod +x $@

clean:
	rm -f mal.ps


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	gs -q -dNODISPLAY -- $@ || exit 1; \

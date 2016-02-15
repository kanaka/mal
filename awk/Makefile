
TESTS =


SOURCES_BASE = types.awk reader.awk printer.awk
SOURCES_LISP = env.awk core.awk stepA_mal.awk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.awk

mal.awk: $(SOURCES)
	echo '#!/bin/sh' > $@
	echo 'arbitrary_long_name==0 "exec" "/usr/bin/gawk" "-O" "-f" "$$0" "$$@"' >> $@
	cat $+ | grep -v "^@include " >> $@
	chmod +x $@

clean:
	rm -f mal.awk


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

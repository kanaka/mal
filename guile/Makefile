SOURCES_BASE = readline.scm types.scm reader.scm printer.scm
SOURCES_LISP = env.scm core.scm stepA_mal.scm
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.scm

mal.scm: $(SOURCES)
	echo "#! /usr/bin/env guile" > $@
	echo "!#" >> $@
	cat $+ | sed $(foreach f,$(+),-e 's/(readline)//') >> $@
	chmod +x $@

clean:
	rm -f mal.scm

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

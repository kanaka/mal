SOURCES_BASE = str.fs types.fs reader.fs printer.fs
SOURCES_LISP = env.fs core.fs stepA_mal.fs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.fs

mal.fs: $(SOURCES)
	echo "#! /usr/bin/env gforth" > $@
	cat $+ | egrep -v "^require |^droprequire " >> $@
	chmod +x $@

clean:
	rm -f mal.fs

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*[\\]|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*[\\]|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

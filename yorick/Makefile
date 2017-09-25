SOURCES_BASE = hash.i types.i reader.i printer.i
SOURCES_LISP = env.i core.i stepA_mal.i
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: all dist clean stats stats-lisp

all: dist

dist: mal

mal: $(SOURCES)
	echo "#!/usr/bin/yorick -batch" > $@
	cat $+ | grep -v "^require," >> $@
	chmod +x $@

clean:
	rm -f mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

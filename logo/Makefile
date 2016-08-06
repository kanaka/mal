SOURCES_BASE = readline.lg types.lg reader.lg printer.lg
SOURCES_LISP = env.lg core.lg stepA_mal.lg
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: all dist clean stats stats-lisp

all:
	@true

dist: mal.lg mal

mal.lg: $(SOURCES)
	cat $+ | grep -v "^load " > $@

mal: mal.lg
	echo "#!/usr/bin/env logo" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.lg mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

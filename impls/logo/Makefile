SOURCES_BASE = readline.lg types.lg reader.lg printer.lg
SOURCES_LISP = env.lg core.lg stepA_mal.lg
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: all dist clean

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

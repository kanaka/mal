SOURCES_BASE = types.awk reader.awk printer.awk
SOURCES_LISP = env.awk core.awk stepA_mal.awk
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.awk mal

mal.awk: $(SOURCES)
	echo 'arbitrary_long_name==0 "exec" "/usr/bin/gawk" "-O" "-f" "$$0" "$$@"' > $@
	cat $+ | grep -v "^@include " >> $@

mal: mal.awk
	echo '#!/bin/sh' > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.awk mal

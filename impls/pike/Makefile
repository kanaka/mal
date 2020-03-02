SOURCES_BASE = readline.pike types.pike reader.pike printer.pike
SOURCES_LISP = env.pike core.pike stepA_mal.pike
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.pike mal

mal.pike: $(SOURCES)
	cat $+ | grep -v "^#include" > $@

mal: mal.pike
	echo "#!/usr/bin/env pike" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.pike mal

SOURCES_BASE = mal_readline.py mal_types.py reader.py printer.py
SOURCES_LISP = env.py core.py stepA_mal.py
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.pyz mal

SHELL := bash
mal.pyz: $(SOURCES)
	cp stepA_mal.py __main__.py
	zip -q - __main__.py $+ > $@
	rm __main__.py

mal: mal.pyz
	echo '#!/usr/bin/env python' > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.pyz mal

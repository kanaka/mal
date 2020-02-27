SOURCES_BASE = types.ps reader.ps printer.ps
SOURCES_LISP = env.ps core.ps stepA_mal.ps
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.ps mal

mal.ps: $(SOURCES)
	cat $+ | grep -v "runlibfile$$" > $@

mal: mal.ps
	echo "#!/bin/sh" > $@
	echo "\":\" pop pop pop pop %#; exec gs -d'#!'=null -d'\":\"'=null -q -dNODISPLAY -- \"\$$0\" \"\$$@\"" >> $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.ps mal

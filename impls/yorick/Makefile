SOURCES_BASE = hash.i types.i reader.i printer.i
SOURCES_LISP = env.i core.i stepA_mal.i
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: all dist clean

all: dist

dist: mal

mal: $(SOURCES)
	echo "#!/usr/bin/yorick -batch" > $@
	cat $+ | grep -v "^require," >> $@
	chmod +x $@

clean:
	rm -f mal

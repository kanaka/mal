SOURCES_BASE = types.sh reader.sh printer.sh
SOURCES_LISP = env.sh core.sh stepA_mal.sh
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.sh mal

mal.sh: $(SOURCES)
	cat $+ | grep -v "^source " > $@

mal: mal.sh
	echo "#!/usr/bin/env bash" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.sh mal

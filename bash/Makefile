SOURCES_BASE = types.sh reader.sh printer.sh
SOURCES_LISP = env.sh core.sh stepA_mal.sh
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: mal.sh

mal.sh: $(SOURCES)
	cat $+ > $@
	echo "#!/usr/bin/env bash" > $@
	cat $+ | grep -v "^source " >> $@
	chmod +x $@

clean:
	rm -f mal.sh

.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

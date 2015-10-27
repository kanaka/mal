SOURCES_BASE = types.sh reader.sh printer.sh
SOURCES_LISP = env.sh core.sh stepA_mal.sh
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: mal.sh

mal.sh: $(SOURCES)
	echo "#!/usr/bin/env bash" > $@
	cat $+ | grep -v "^source " >> $@
	chmod +x $@

clean:
	rm -f mal.sh

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

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

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

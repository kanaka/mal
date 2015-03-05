
TESTS =


SOURCES_BASE = mal_readline.py mal_types.py reader.py printer.py
SOURCES_LISP = env.py core.py stepA_mal.py
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


#all: mal.sh
#
#mal.sh: $(SOURCES)
#	cat $+ > $@
#	echo "#!/bin/bash" > $@
#	cat $+ | grep -v "^source " >> $@
#	chmod +x $@
#
#clean:
#	rm -f mal.sh

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	python $@ || exit 1; \

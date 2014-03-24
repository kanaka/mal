
TESTS =


SOURCES = mal_types.py mal_readline.py reader.py stepA_more.py

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

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	python $@ || exit 1; \

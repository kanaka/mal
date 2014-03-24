TESTS = tests/types.sh tests/reader.sh

SOURCES = types.sh reader.sh stepA_more.sh

all: mal.sh

mal.sh: $(SOURCES)
	cat $+ > $@
	echo "#!/bin/bash" > $@
	cat $+ | grep -v "^source " >> $@
	chmod +x $@

clean:
	rm -f mal.sh

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	bash $@ || exit 1; \

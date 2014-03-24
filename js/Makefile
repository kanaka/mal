
TESTS = tests/types.js tests/reader.js tests/step5_tco.js

SOURCES = node_readline.js types.js reader.js stepA_more.js
WEB_SOURCES = $(SOURCES:node_readline.js=josh_readline.js)

all: mal.js mal_web.js

mal.js: $(SOURCES)
	echo "#!/usr/bin/env node" > $@
	cat $+ | grep -v "= *require('./" >> $@
	chmod +x $@

mal_web.js: $(WEB_SOURCES)
	cat $+ | grep -v "= *require('./" > $@

clean:
	rm -f mal.js mal_web.js

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	node $@ || exit 1; \


TESTS = tests/types.js tests/reader.js

SOURCES_BASE = node_readline.js types.js reader.js printer.js
SOURCES_LISP = env.js core.js stepA_more.js
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)
WEB_SOURCES = $(SOURCES:node_readline.js=jq_readline.js)

all: node_modules mal.js web/mal.js

node_modules:
	npm install

mal.js: $(SOURCES)
	echo "#!/usr/bin/env node" > $@
	cat $+ | grep -v "= *require('./" >> $@
	chmod +x $@

web/mal.js: $(WEB_SOURCES)
	cat $+ | grep -v "= *require('./" > $@

clean:
	rm -f mal.js web/mal.js

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	node $@ || exit 1; \

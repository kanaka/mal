
SOURCES_BASE = node_readline.js miniMAL-core.json \
	       types.json reader.json printer.json
SOURCES_LISP = env.json core.json stepA_mal.json
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

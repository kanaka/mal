SOURCES_BASE = types.rkt reader.rkt printer.rkt
SOURCES_LISP = env.rkt core.rkt stepA_mal.rkt
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

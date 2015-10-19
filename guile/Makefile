SOURCES_BASE = types.scm reader.scm printer.scm
SOURCES_LISP = env.scm core.scm stepA_mal.scm
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

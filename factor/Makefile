TESTS =

SOURCES_BASE = src/types/types.factor src/reader/reader.factor src/printer/printer.factor
SOURCES_LISP = src/malenv/malenv.factor src/core/core.factor src/stepA_mal/stepA_mal.factor
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^


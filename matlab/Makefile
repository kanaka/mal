SOURCES_BASE = Reader.m types/Symbol.m reader.m printer.m
#SOURCES_LISP = env.m core.m stepA_interop.m
SOURCES_LISP = stepA_interop.m
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

stats-lisp: $(SOURCES_LISP)
	@wc $^

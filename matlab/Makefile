SOURCES_BASE = types.m types/Nil.m types/MalException.m \
	       types/Symbol.m types/List.m types/Vector.m \
	       types/HashMap.m types/Function.m types/Atom.m \
	       types/Reader.m reader.m printer.m
SOURCES_LISP = Env.m core.m stepA_mal.m
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

stats-lisp: $(SOURCES_LISP)
	@wc $^

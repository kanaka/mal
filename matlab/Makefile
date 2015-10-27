SOURCES_BASE = types.m types/Nil.m types/MalException.m \
	       types/Symbol.m types/List.m types/Vector.m \
	       types/HashMap.m types/Function.m types/Atom.m \
	       types/Reader.m reader.m printer.m
SOURCES_LISP = Env.m core.m stepA_mal.m
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*%|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

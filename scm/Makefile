SOURCES_BASE = reader.scm printer.scm types.scm
SOURCES_LISP = env.scm func.scm core.scm stepA_mal.scm
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

clean:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


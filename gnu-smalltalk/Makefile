SOURCES_BASE = readline.st reader.st printer.st types.st util.st
SOURCES_LISP = env.st func.st core.st stepA_mal.st
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

clean:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


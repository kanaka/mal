SOURCES_BASE = mal/reader.el mal/printer.el mal/types.el
SOURCES_LISP = mal/env.el mal/func.el mal/core.el stepA_mal.el
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


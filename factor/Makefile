TESTS =

SOURCES_BASE = mal/types/types.factor mal/reader/reader.factor mal/printer/printer.factor
SOURCES_LISP = mal/env/env.factor mal/core/core.factor stepA_mal/stepA_mal.factor
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


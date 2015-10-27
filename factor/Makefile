TESTS =

SOURCES_BASE = src/types/types.factor src/reader/reader.factor src/printer/printer.factor
SOURCES_LISP = src/malenv/malenv.factor src/core/core.factor src/stepA_mal/stepA_mal.factor
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\!|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


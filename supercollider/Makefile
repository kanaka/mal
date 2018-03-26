SOURCES_BASE = reader.sc printer.sc types.sc
SOURCES_LISP = env.sc core.sc stepA_mal.sc
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

clean:

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all clean stats stats-lisp

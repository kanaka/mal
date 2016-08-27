SOURCES_BASE = utils.lisp types.lisp reader.lisp printer.lisp
SOURCES_LISP = env.lisp core.lisp stepA_mal.lisp
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

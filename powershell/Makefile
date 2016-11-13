SOURCES_BASE = types.psm1 reader.psm1 printer.psm1
SOURCES_LISP = env.psm1 core.psm1 stepA_mal.ps1
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

SOURCES_BASE = types.fs str.fs reader.fs printer.fs
SOURCES_LISP = env.fs core.fs stepA_mal.fs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*[\\]|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*[\\]|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

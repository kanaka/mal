SOURCES_BASE = types.rkt reader.rkt printer.rkt
SOURCES_LISP = env.rkt core.rkt stepA_mal.rkt
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:

dist: mal

mal: $(SOURCES)
	raco exe stepA_mal.rkt
	mv stepA_mal $@

clean:
	rm -f mal

.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

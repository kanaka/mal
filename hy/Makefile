SOURCES_BASE = mal_types.hy reader.hy printer.hy
SOURCES_LISP = env.hy core.hy stepA_mal.hy
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: mal.hy

mal.hy: stepA_mal.hy
	cp $< $@

clean:
	rm -f mal.hy *.pyc

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

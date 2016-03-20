
TESTS =

SOURCES_BASE =
SOURCES_LISP = env.mal core.mal stepA_mal.mal
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: mal.mal

mal.mal: stepA_mal.mal
	cp $< $@

clean:
	rm -f mal.mal

#.PHONY: stats tests $(TESTS)
.PHONY: stats

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

#tests: $(TESTS)
#
#$(TESTS):
#	@echo "Running $@"; \
#	python $@ || exit 1; \

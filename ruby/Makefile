TESTS =

SOURCES_BASE = mal_readline.rb types.rb reader.rb printer.rb
SOURCES_LISP = env.rb core.rb stepA_mal.rb
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.rb mal

mal.rb: $(SOURCES)
	cat $+ | grep -v "^require_relative" > $@

mal: mal.rb
	echo "#!/usr/bin/env ruby" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.rb mal

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	ruby $@ || exit 1; \

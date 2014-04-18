TESTS =

SOURCES_BASE = mal_readline.rb types.rb reader.rb printer.rb
SOURCES_LISP = env.rb core.rb stepA_more.rb
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#all: mal.rb

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	ruby $@ || exit 1; \

TESTS =

SOURCES_BASE = node_readline.coffee types.coffee \
	       reader.coffee printer.coffee
SOURCES_LISP = env.coffee core.coffee stepA_mal.coffee
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#all: mal.rb

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

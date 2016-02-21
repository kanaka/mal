TESTS =

SOURCES_BASE = node_readline.coffee types.coffee \
	       reader.coffee printer.coffee
SOURCES_LISP = env.coffee core.coffee stepA_mal.coffee
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: node_modules

node_modules:
	npm install


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"

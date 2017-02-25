SOURCES_BASE = types.ts reader.ts printer.ts
SOURCES_LISP = env.ts core.ts stepA_mal.ts
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: node_modules ts

node_modules:
	npm install

ts: node_modules
	$(npm bin)/tsc -p ./

clean:
	rm -f *.js mal


.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `egrep "^\w*#|^\w*$$" $^ | wc` "[comments/blanks]"

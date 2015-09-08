TESTS =

SOURCES_BASE = utils.lua types.lua reader.lua printer.lua
SOURCES_LISP = env.lua core.lua stepA_mal.lua
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libs

.PHONY: stats tests $(TESTS)

clean:
	rm -f linenoise.so

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^


.PHONY: libs
libs: linenoise.so

linenoise.so:
	luarocks install --tree=./ linenoise
	ln -sf lib/lua/5.1/linenoise.so $@


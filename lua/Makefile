TESTS =

SOURCES_BASE = utils.lua types.lua reader.lua printer.lua
SOURCES_LISP = env.lua core.lua stepA_mal.lua
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libs

.PHONY: stats tests $(TESTS)

clean:
	rm -f linenoise.so
	rm -rf lib/lua/5.1

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


.PHONY: libs
libs: linenoise.so

linenoise.so:
	luarocks install --tree=./ linenoise
	ln -sf lib/lua/5.1/linenoise.so $@


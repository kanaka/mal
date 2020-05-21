SOURCES_BASE = utils.lua types.lua reader.lua printer.lua
SOURCES_LISP = env.lua core.lua stepA_mal.lua
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libs

dist: mal.lua mal

SOURCE_NAMES = $(patsubst %.lua,%,$(SOURCES))
mal.lua: $(SOURCES)
	echo "local $(foreach n,$(SOURCE_NAMES),$(n),) M" > $@
	echo "M={} $(foreach n,$(SOURCE_NAMES),$(n)=M);" >> $@
	cat $+ | grep -v -e "return M$$" \
	    -e "return Env" \
	    -e "local M =" \
	    -e "^#!" \
	    $(foreach n,$(SOURCE_NAMES),-e "require('$(n)')") >> $@

mal: mal.lua
	echo "#!/usr/bin/env lua" > $@
	cat $< >> $@
	chmod +x $@


clean:
	rm -f linenoise.so rex_pcre.so mal.lua mal
	rm -rf lib

.PHONY: libs
libs: linenoise.so rex_pcre.so

linenoise.so:
	luarocks install --tree=./ linenoise
	ln -sf $$(find . -name linenoise.so) $@

rex_pcre.so:
	luarocks install --tree=./ lrexlib-pcre
	ln -sf $$(find . -name rex_pcre.so) $@


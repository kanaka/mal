SOURCES_BASE = utils.lua types.lua reader.lua printer.lua
SOURCES_LISP = env.lua core.lua stepA_mal.lua
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

libraries := linenoise.so rex_pcre.so
linenoise.so_package := linenoise
rex_pcre.so_package := lrexlib-pcre

all: $(libraries)

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
	rm -f $(libraries) mal.lua mal
	rm -rf lib

$(libraries):
	luarocks install --tree=./ $($@_package)
	find . -name $@ | xargs ln -s

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

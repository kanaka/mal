SOURCES_BASE = mal_readline.tcl types.tcl reader.tcl printer.tcl
SOURCES_LISP = env.tcl core.tcl stepA_mal.tcl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	true

dist: mal.tcl mal

mal.tcl: $(SOURCES)
	cat $+ | grep -v "^source " > $@

mal: mal.tcl
	echo "#!/usr/bin/env tclsh" > $@
	cat $< >> $@
	chmod +x $@

clean:
	rm -f mal.tcl mal

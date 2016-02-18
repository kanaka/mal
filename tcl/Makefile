SOURCES_BASE = mal_readline.tcl types.tcl reader.tcl printer.tcl
SOURCES_LISP = env.tcl core.tcl stepA_mal.tcl
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

.PHONY: stats stats-lisp

all:
	true

dist: mal.tcl

mal.tcl: $(SOURCES)
	echo "#!/usr/bin/env tclsh" > $@
	cat $+ | grep -v "^source " >> $@
	chmod +x $@

clean:
	rm -f mal.tcl


stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

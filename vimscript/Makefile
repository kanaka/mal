SOURCES_BASE = readline.vim types.vim reader.vim printer.vim
SOURCES_LISP = env.vim core.vim stepA_mal.vim
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libvimextras.so

dist: mal.vim mal

mal.vim: $(SOURCES)
	cat $+ | grep -v "^source " > $@

mal: mal.vim
	echo "#!/bin/sh" > $@
	echo "\":\" ; rundir=\`dirname \$$0\`" >> $@
	echo "\":\" ; export LD_LIBRARY_PATH=\`readlink -f \$$rundir\`" >> $@
	echo "\":\" ; exec vim -i NONE -V1 -nNesS \"\$$0\" -- \"\$$@\" 2>/dev/null" >> $@
	cat $< >> $@
	chmod +x $@


libvimextras.so: vimextras.o
	$(CC) -g -shared -o $@ $< -lreadline

vimextras.o: vimextras.c
	$(CC) -g -fPIC -c $< -o $@

clean:
	rm -f vimextras.o libvimextras.so mal.vim mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\"|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: stats stats-lisp clean

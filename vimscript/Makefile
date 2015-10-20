SOURCES_BASE = readline.vim types.vim reader.vim printer.vim
SOURCES_LISP = env.vim core.vim stepA_mal.vim
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: libvimreadline.so

libvimreadline.so: vimreadline.o
	$(CC) -g -shared -o $@ $< -lreadline

vimreadline.o: vimreadline.c
	$(CC) -g -fPIC -c $< -o $@

clean:
	rm -f vimreadline.o libvimreadline.so

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

.PHONY: stats stats-lisp clean

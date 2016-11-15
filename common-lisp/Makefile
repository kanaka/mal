ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SOURCES_LISP = env.lisp core.lisp stepA_mal.lisp
SOURCES = utils.lisp types.lisp reader.lisp printer.lisp $(SOURCES_LISP)
LISP ?= sbcl

all : stepA_mal

.PHONY: stats

step% : step%.lisp utils.lisp types.lisp env.lisp printer.lisp reader.lisp core.lisp
	cl-launch -v -l $(LISP) +Q -S $(ROOT_DIR) -s $@ -d $@.image -o $@ -E 'mal:main' -e '(load "~/quicklisp/setup.lisp")'

clean:
	find . -name 'step*' -executable -exec git check-ignore \{\} \; -delete
	rm -f *.lib *.fas[l]

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

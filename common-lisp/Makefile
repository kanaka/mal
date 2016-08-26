ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

step% : step%.lisp utils.lisp types.lisp printer.lisp reader.lisp
	cl-launch -v -l sbcl +Q -S $(ROOT_DIR) -s $@ -d $@.image -o $@ -E 'mal:main'

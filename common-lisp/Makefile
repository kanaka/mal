ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

step% : step%.lisp utils.lisp types.lisp env.lisp printer.lisp reader.lisp core.lisp
	cl-launch -v -l sbcl +Q -S $(ROOT_DIR) -s $@ -d $@.image -o $@ -E 'mal:main'

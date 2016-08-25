ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

step% : step%.lisp
	cl-launch -v -l sbcl +Q -S $(ROOT_DIR) -s $@ -d $@.image -o $@ -E 'mal:main'

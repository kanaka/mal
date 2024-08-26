all:
	emacs -Q --batch -L . --eval '(byte-recompile-directory "." 0)'

# For debugging, it is sometimes useful to attempt a run without byte compation.
nocompile: clean
	exec emacs -Q --batch -L . --eval "(setq text-quoting-style 'straight)" --load stepA_mal.el

clean:
	rm -f *.elc *~ mal/*.elc mal/*~

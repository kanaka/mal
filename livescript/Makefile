TESTS =

SOURCES_BASE = 
SOURCES_LISP = stepA_mal.ls
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: node_modules

node_modules:
	npm install

%.js: %.ls
	lsc -d -c $(@:%.js=%.ls)

clean:
	rm -f *.js

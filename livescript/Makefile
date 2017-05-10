TESTS =

SOURCES_BASE = reader.js printer.js env.js core.js utils.js
SOURCES_LISP =
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all: $(SOURCES) node_modules

node_modules:
	npm install

%.js: %.ls $(SOURCES)
	lsc -d -c $(@:%.js=%.ls)

clean:
	rm -f *.js

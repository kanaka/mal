SOURCES_BASE = lib/mal/types.ex lib/mal/reader.ex lib/mal/printer.ex
SOURCES_LISP = lib/mal/env.ex lib/mal/core.ex lib/mix/tasks/stepA_mal.ex
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	mix compile

dist: mal

mal: $(SOURCES)
	mix escript.build

clean:
	mix clean
	rm -f mal

.PHONY: clean

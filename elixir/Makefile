SOURCES_BASE = lib/mal/types.ex lib/mal/reader.ex lib/mal/printer.ex
SOURCES_LISP = lib/mal/env.ex lib/mal/core.ex lib/mix/tasks/stepA_mal.ex
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

all:
	mix compile

clean:
	mix clean

stats: $(SOURCES)
	@wc $^

stats-lisp: $(SOURCES_LISP)
	@wc $^

.PHONY: clean stats stats-lisp

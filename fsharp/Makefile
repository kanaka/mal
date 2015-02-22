#####################

DEBUG =

TESTS =

SOURCES_LISP =

#####################

SRCS = step0_repl.fs 

FLAGS = $(if $(strip $(DEBUG)),-debug+,)

#####################

all: $(patsubst %.fs,%.exe,$(SRCS))

%.exe: %.fs
	fsharpc $(FLAGS) $<

clean:
	rm -f *.dll *.exe *.mdb

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \

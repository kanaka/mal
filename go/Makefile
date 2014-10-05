export GOPATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

#####################

SOURCES_BASE = src/types/types.go src/reader/reader.go
SOURCES_LISP = src/step1_read_print/step1_read_print.go
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.go step1_read_print.go
BINS = $(SRCS:%.go=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): $(SOURCES_BASE) $(SOURCES_LISP)
	go build $@

clean:
	rm -f $(BINS) mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

#####################

SOURCES_BASE = 
SOURCES_LISP = step0_repl.go
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)


#####################

SRCS = step0_repl.go
BINS = $(SRCS:%.go=%)

#####################

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.go
	go build -o $@ $+ 

clean:
	rm -f $(BINS) mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

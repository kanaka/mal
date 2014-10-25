#####################

SOURCES_BASE = src/types.rs src/readline.rs \
	       src/reader.rs src/printer.rs
SOURCES_LISP = src/step3_env.rs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.rs step1_read_print.rs step2_eval.rs step3_env.rs
BINS = $(SRCS:%.rs=target/%)

#####################

all: $(BINS) mal

mal: ${SOURCES_BASE} ${SOURCES_LISP}
	cargo build
	cp $(word $(words ${BINS}),${BINS})  $@

clean:
	cargo clean
	rm -f mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

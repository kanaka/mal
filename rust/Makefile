#####################

SOURCES_BASE = src/types.rs src/readline.rs \
	       src/reader.rs src/printer.rs \
	       src/env.rs src/core.rs
SOURCES_LISP = src/step6_file.rs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.rs step1_read_print.rs step2_eval.rs step3_env.rs \
       step4_if_fn_do.rs step5_tco.rs step6_file.rs
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

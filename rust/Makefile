#####################

SOURCES_BASE = src/types.rs src/readline.rs \
	       src/reader.rs src/printer.rs \
	       src/env.rs src/core.rs
SOURCES_LISP = src/env.rs src/core.rs src/bin/stepA_mal.rs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step1_read_print.rs step2_eval.rs step3_env.rs \
       step4_if_fn_do.rs step5_tco.rs step6_file.rs step7_quote.rs \
       step8_macros.rs step9_try.rs stepA_mal.rs
BINS = $(SRCS:%.rs=target/release/%)

#####################

all: mal

mal: ${SOURCES_BASE} $(word $(words ${SOURCES_LISP}),${SOURCES_LISP})
	cargo build --release
	cp $(word $(words ${BINS}),${BINS})  $@

#$(BINS): target/release/%: src/%.rs
#	cargo build --release $*

clean:
	cargo clean
	rm -f mal

.PHONY: stats stats-lisp mal

stats: $(SOURCES)
	@wc $^
stats-lisp: $(SOURCES_LISP)
	@wc $^

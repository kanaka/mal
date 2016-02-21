STEPS = step0_repl.ml step1_read_print.ml step2_eval.ml step3_env.ml \
        step4_if_fn_do.ml step5_tco.ml step6_file.ml step7_quote.ml \
	step8_macros.ml step9_try.ml stepA_mal.ml
MODULES = types.ml reader.ml printer.ml env.ml core.ml
LIBS = str.cmxa unix.cmxa
MAL_LIB = mal_lib.cmxa

STEP_BINS = $(STEPS:%.ml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS) mal

mal: $(LAST_STEP_BIN)
	cp $< $@

# ocaml repl apparently needs bytecode, not native, compilation.
# Just do it all right here:
repl:
	ocamlc -c $(LIBS:%.cmxa=%.cma) $(MODULES) $(STEPS)
	rlwrap ocaml $(LIBS:%.cmxa=%.cma) $(MODULES:%.ml=%.cmo)

$(MAL_LIB): $(MODULES)
	ocamlopt -a $(MODULES) -o $@

$(STEP_BINS): %: %.ml $(MAL_LIB)
	ocamlopt $(LIBS) $(MAL_LIB) $< -o $@

clean:
	rm -f $(STEP_BINS) mal mal_lib.* *.cmo *.cmx *.cmi *.o

stats: $(MODULES) stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: env.ml core.ml stepA_mal.ml
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*\(\*|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all repl clean stats stats-lisp

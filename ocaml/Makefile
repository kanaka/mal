STEPS = step0_repl.ml step1_read_print.ml step2_eval.ml step3_env.ml
MODULES = types.ml reader.ml printer.ml env.ml
LIBS = str.cma 

STEP_BINS = $(STEPS:%.ml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))
MODULE_BINS = $(MODULES:%.ml=%.cmo)

all: $(STEP_BINS) mal

mal: $(LAST_STEP_BIN)
	cp $< $@

repl: $(MODULES)
	rlwrap ocaml $(LIBS) $(MODULE_BINS)

$(STEP_BINS): %: %.ml $(MODULES)
	ocamlc $(LIBS) $(MODULES) $< -o $@

clean:
	rm -f $(STEP_BINS) mal *.cmi *.cmo

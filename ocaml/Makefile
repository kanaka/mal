STEPS = step0_repl.ml step1_read_print.ml step2_eval.ml
MODULES = types.ml reader.ml printer.ml
LIBS = str.cma 

BINS = $(STEPS:%.ml=%)

all: $(BINS) mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

repl: $(MODULES)
	rlwrap ocaml $(LIBS) $(MODULES:%.ml=%.cmo)

$(BINS): %: %.ml $(MODULES)
	ocamlc $(LIBS) $(MODULES) $< -o $@

clean:
	rm -f $(BINS) mal *.cmi *.cmo

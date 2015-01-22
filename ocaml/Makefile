STEPS = step0_repl.ml step1_read_print.ml
MODULES = types.ml reader.ml printer.ml
LIBS = str.cma 

BINS = $(STEPS:%.ml=%)

all: $(BINS) mal oc

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.ml $(MODULES)
	ocamlc $(LIBS) $(MODULES) $< -o $@

clean:
	rm -f $(BINS) mal *.cmi *.cmo

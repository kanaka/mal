STEP_BINS = step0_repl step1_read_print

all: $(STEP_BINS)

step0_repl: step0_repl.mlb step0_repl.sml main.sml
	mlton -output $@ $<

step1_read_print: step1_read_print.mlb step1_read_print.sml reader.sml printer.sml types.sml util.sml
	mlton -output $@ $<

clean:
	rm -f $(STEP_BINS)

.PHONY: all clean

STEP_BINS = step0_repl

all: $(STEP_BINS)

step0_repl: step0_repl.mlb step0_repl.sml main.sml
	mlton -output $@ $<

clean:
	rm -f $(STEP_BINS)

.PHONY: all clean

STEPS = step0_repl.sml

STEP_BINS = $(STEPS:%.sml=%)

all: $(STEP_BINS)

$(STEP_BINS): %: %.sml
	mosmlc $< main.sml -o $@

clean:
	rm -f $(STEP_BINS) *.ui *.uo

.PHONY: all clean

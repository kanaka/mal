STEP_BINS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do

all: $(STEP_BINS)

step0_repl: step0_repl.mlb step0_repl.sml main.sml
	mlton -output $@ $<

step1_read_print: step1_read_print.mlb step1_read_print.sml reader.sml printer.sml types.sml util.sml main.sml
	mlton -output $@ $<

step2_eval: step2_eval.mlb step2_eval.sml env.sml reader.sml printer.sml types.sml util.sml main.sml
	mlton -output $@ $<

step3_env: step3_env.mlb step3_env.sml env.sml reader.sml printer.sml types.sml util.sml main.sml
	mlton -output $@ $<

step4_if_fn_do: step4_if_fn_do.mlb step4_if_fn_do.sml env.sml reader.sml printer.sml types.sml util.sml main.sml
	mlton -output $@ $<

clean:
	rm -f $(STEP_BINS)

.PHONY: all clean

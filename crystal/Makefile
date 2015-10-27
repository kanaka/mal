STEPS = step0_repl.cr step1_read_print.cr step2_eval.cr step3_env.cr \
        step4_if_fn_do.cr step5_tco.cr step6_file.cr step7_quote.cr \
	step8_macros.cr step9_try.cr stepA_mal.cr

STEP_BINS = $(STEPS:%.cr=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS) mal

mal: $(LAST_STEP_BIN)
	cp $< $@

$(STEP_BINS): %: %.cr $(MAL_LIB)
	crystal build --release $<

clean:
	rm -rf $(STEP_BINS) mal .crystal

stats: types.cr error.cr readline.cr reader.cr printer.cr env.cr core.cr stepA_mal.cr
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: env.cr core.cr stepA_mal.cr
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*#|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

.PHONY: all clean stats stats-lisp


CC= hare build

BINS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

.PHONY: all
all: $(BINS)

%: %.ha $(wildcard mal/*.ha)
	$(CC) -o $@ $<

.PHONY: clean
clean:
	rm $(BINS)

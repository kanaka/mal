STEPS = step0_repl step1_read_print step2_eval step3_env \
  step4_if_fn_do step5_tco step6_file step7_quote step8_macros \
  step9_try stepA_mal
all: $(STEPS)
$(STEPS):
	shards build $@ --release
clean:
	rm -rf .cache/crystal/ .cache/shards/ bin/ lib/
.PHONY: all clean $(STEPS)

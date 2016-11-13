DIRS=obj
PROGS=step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
      step5_tco step6_file step7_quote step8_macros step9_try

SOURCES_LISP=envs.ad[bs] eval_callback.ads core.ad[bs] stepa_mal.adb
SOURCES=$(SOURCES_LISP) \
        types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
	reader.ad[bs] printer.ad[bs] smart_pointers.ad[bs]

all:	${DIRS} ${PROGS} stepA_mal

${DIRS}:
	mkdir -p $@

# stepA_mal is awkward because GNAT requires the filename to be lowercase
${PROGS} stepa_mal:	force
	gnatmake -O3 -gnata $@.adb -D obj

# so we make stepa_mal and just move it.
stepA_mal:	stepa_mal
	mv $< $@

clean:
	rm -f ${PROGS}
	rm -rf obj

.PHONY: stats stats-lisp force

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

force:

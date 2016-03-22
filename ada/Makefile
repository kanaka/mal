DIRS=obj
PROGS=step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
      step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

STEP0_DEPS=${DIRS}
STEP1_DEPS=${STEP0_DEPS} types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
	   reader.ad[bs] printer.ad[bs] smart_pointers.ad[bs]
STEP2_DEPS=${STEP1_DEPS}
STEP3_DEPS=${STEP2_DEPS} envs.ad[bs]
STEP4_DEPS=${STEP3_DEPS} core.ad[bs]
STEP5_DEPS=${STEP4_DEPS} eval_callback.ads

all:	${DIRS} ${PROGS}

${DIRS}:
	mkdir -p $@

step%:
	gnatmake -g -gnata -o $@ -P$@

step0_repl:	step0_repl.adb ${STEP0_DEPS}
step1_read_print:	step1_read_print.adb ${STEP1_DEPS}
step2_eval:	step2_eval.adb ${STEP2_DEPS}
step3_env:	step3_env.adb ${STEP3_DEPS}
step4_if_fn_do:	step4_if_fn_do.adb ${STEP4_DEPS}
step5_tco:	step5_tco.adb ${STEP5_DEPS}
step6_file:	step6_file.adb ${STEP5_DEPS}
step7_quote:	step7_quote.adb ${STEP5_DEPS}
step8_macros:	step8_macros.adb ${STEP5_DEPS}
step9_try:	step9_try.adb ${STEP5_DEPS}
stepA_mal:	stepa_mal.adb ${STEP5_DEPS}

clean:
	rm -f ${PROGS}
	rm -rf obj

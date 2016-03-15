DIRS=obj
PROGS=step0_repl step1_read_print step2_eval step3_env step4_if_fn_do \
	step5_tco step6_file step7_quote step8_macros step9_try stepA_mal

all:	${DIRS} ${PROGS}

${DIRS}:
	mkdir -p $@

step0_repl:	step0_repl.adb
	gnatmake -g -P$@

step1_read_print:	step1_read_print.adb types.ad[bs] types-vector.ad[bs] \
			types-hash_map.ad[bs] reader.ad[bs] printer.ad[bs] \
			smart_pointers.ad[bs]
	gnatmake -g -P$@

step2_eval:	step2_eval.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -P$@

step3_env:	step3_env.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step4_if_fn_do:	step4_if_fn_do.adb types.ad[bs] types-vector.ad[bs] \
		types-hash_map.ad[bs]  reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step5_tco:	step5_tco.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step6_file:	step6_file.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step7_quote:	step7_quote.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step8_macros:	step8_macros.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] \
		smart_pointers.ad[bs] envs.ad[bs] eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

step9_try:	step9_try.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] smart_pointers.ad[bs] envs.ad[bs] \
		eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -P$@

stepA_mal:	stepa_mal.adb types.ad[bs] types-vector.ad[bs] types-hash_map.ad[bs] \
		reader.ad[bs] printer.ad[bs] smart_pointers.ad[bs] envs.ad[bs] \
		eval_callback.ads core.ad[bs]
	gnatmake -g -gnata -o $@ -PstepA_mal

clean:
	rm -f ${PROGS}
	rm -rf obj

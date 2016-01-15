# Python step rules
s%.py: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -python $@

step1_read_print.py: types/Types.hx reader/Reader.hx printer/Printer.hx
step2_eval.py: types/Types.hx reader/Reader.hx printer/Printer.hx
step3_env.py: types/Types.hx reader/Reader.hx printer/Printer.hx env/Env.hx
step4_if_fn_do.py: types/Types.hx reader/Reader.hx printer/Printer.hx env/Env.hx core/Core.hx
step5_tco.py: types/Types.hx reader/Reader.hx printer/Printer.hx env/Env.hx core/Core.hx
step6_file.py: types/Types.hx reader/Reader.hx printer/Printer.hx env/Env.hx core/Core.hx

clean:
	rm -r *.py

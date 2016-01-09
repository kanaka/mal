# Python step rules
s%.py: S%.hx
	haxe -main $(patsubst %.hx,%,$<) -python $@

step1_read_print.py: types/Types.hx reader/Reader.hx printer/Printer.hx

clean:
	rm -r *.py

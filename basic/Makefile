basic_MODE = cbm
BASICPP_OPTS = --mode $(basic_MODE)

QB64 = qb64

STEPS4_A = step4_if_fn_do.bas step5_tco.bas step6_file.bas \
           step7_quote.bas step8_macros.bas step9_try.bas stepA_mal.bas
STEPS3_A = step3_env.bas $(STEPS4_A)
STEPS1_A = step1_read_print.bas step2_eval.bas $(STEPS3_A)
STEPS0_A = step0_repl.bas $(STEPS1_A)

all: $(if $(filter qbasic,$(basic_MODE)),$(subst .bas,,$(STEPS0_A)),$(STEPS0_A))

$(STEPS0_A): readline.in.bas readline_line.in.bas readline_char.in.bas
$(STEPS1_A): debug.in.bas mem.in.bas types.in.bas reader.in.bas printer.in.bas
$(STEPS3_A): env.in.bas
$(STEPS4_A): core.in.bas



step%.bas: step%.in.bas
	./basicpp.py $(BASICPP_OPTS) $< > $@

tests/%.bas: tests/%.in.bas
	./basicpp.py $(BASICPP_OPTS) $< > $@

# QBasic specific compilation rule
step%: step%.bas
	$(QB64) -x $(abspath $<) -o $(abspath $@)

# CBM/C64 image rules

%.prg: %.bas
	cat $< | tr "A-Z" "a-z" > $<.tmp
	#cat $< | sed 's/["]\@<!\<\w\+\>["]\@!/\L&/g' > $<.tmp
	petcat -w2 -nc -o $@ $<.tmp
	#rm $<.tmp

mal.prg: stepA_mal.prg
	cp $< $@

.args.mal.prg: .args.mal
	petcat -text -w2 -o $@ $<

core.mal.prg: ../core.mal
	petcat -text -w2 -o $@ $<

mal.d64: mal.prg .args.mal.prg core.mal.prg
	c1541 -format "mal,01" d64 $@ \
	    -attach $@ \
	    -write $< mal \
	    -write .args.mal.prg .args.mal \
	    -write core.mal.prg core.mal


# Clean and Stats rules

.PHONY: clean

clean:
	rm -f $(STEPS0_A) $(subst .bas,,$(STEPS0_A)) *.d64 *.prg qb64
	rm -rf ./internal

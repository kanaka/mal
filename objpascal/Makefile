STEPS = step0_repl.pas step1_read_print.pas step2_eval.pas \
	step3_env.pas step4_if_fn_do.pas step5_tco.pas \
	step6_file.pas step7_quote.pas step8_macros.pas \
	step9_try.pas stepA_mal.pas

STEP0_DEPS = pas-readline/src/readline.pas regexpr/Source/RegExpr.pas
STEP1_DEPS = $(STEP0_DEPS) mal_types.pas reader.pas printer.pas
STEP3_DEPS = $(STEP1_DEPS) mal_env.pas
STEP4_DEPS = $(STEP3_DEPS) core.pas

SOURCES = mal_types.pas mal_func.pas reader.pas printer.pas \
	  mal_env.pas core.pas stepA_mal.pas
SOURCES_LISP = mal_env.pas core.pas stepA_mal.pas

#####################

DEBUG = -gl

FPC = fpc -MOBJFPC -ve $(DEBUG) -Fupas-readline/src -Furegexpr/Source

all: $(patsubst %.pas,%,$(STEPS))

# Downloaded units
pas-readline: pas-readline/src/readline.pas
pas-readline/src/readline.pas:
	git clone https://github.com/hansiglaser/pas-readline pas-readline

#regexpr: regexpr/Source/RegExpr.pas
#regexpr/Source/RegExpr.pas: regexpr-linux.patch
#	mkdir -p regexpr
#	curl -O http://regexpstudio.com/Downloads/regexpr.zip
#	cd regexpr && unzip ../regexpr.zip
#	rm regexpr.zip
#	patch -p1 < regexpr-linux.patch

step%: step%.pas
	$(FPC) $<

step0_repl: $(STEP0_DEPS)
step1_read_print step2_eval: $(STEP1_DEPS)
step3_env: $(STEP3_DEPS)
step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal: $(STEP4_DEPS)

clean:
	rm -f $(STEPS:%.pas=%) *.o *.ppu regexpr/Source/*.o regexpr/Source/*.ppu mal

.PHONY: stats stats-lisp

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


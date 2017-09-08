SOURCES_BASE = lib/reader.sld lib/printer.sld lib/types.sld
SOURCES_LISP = lib/env.sld lib/core.sld stepA_mal.scm
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)
BINS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco
BINS += step6_file step7_quote step8_macros step9_try stepA_mal

SYMLINK = ln -sfr
RM = rm -f
RMR = rm -rf

KAWA = kawa --r7rs --no-warn-unused -d out -C
KAWAM = kawa --r7rs --no-warn-unused -d out --main -C
CSC = csc -O3 -R r7rs
CSCSO = $(CSC) -sJ
CYCLONE = cyclone -O2

all: symlinks

.PHONY: symlinks kawa chicken cyclone clean stats stats-lisp

symlinks:
	$(SYMLINK) lib/util.sld lib/util.scm
	$(SYMLINK) lib/util.sld lib.util.scm
	$(SYMLINK) lib/types.sld lib/types.scm
	$(SYMLINK) lib/types.sld lib.types.scm
	$(SYMLINK) lib/reader.sld lib/reader.scm
	$(SYMLINK) lib/reader.sld lib.reader.scm
	$(SYMLINK) lib/printer.sld lib/printer.scm
	$(SYMLINK) lib/printer.sld lib.printer.scm
	$(SYMLINK) lib/env.sld lib/env.scm
	$(SYMLINK) lib/env.sld lib.env.scm
	$(SYMLINK) lib/core.sld lib/core.scm
	$(SYMLINK) lib/core.sld lib.core.scm

kawa:
	$(KAWA) lib/util.scm
	$(KAWA) lib/types.scm
	$(KAWA) lib/reader.scm
	$(KAWA) lib/printer.scm
	$(KAWA) lib/env.scm
	$(KAWA) lib/core.scm
	$(KAWAM) step0_repl.scm
	$(KAWAM) step1_read_print.scm
	$(KAWAM) step2_eval.scm
	$(KAWAM) step3_env.scm
	$(KAWAM) step4_if_fn_do.scm
	$(KAWAM) step5_tco.scm
	$(KAWAM) step6_file.scm
	$(KAWAM) step7_quote.scm
	$(KAWAM) step8_macros.scm
	$(KAWAM) step9_try.scm
	$(KAWAM) stepA_mal.scm

chicken:
	$(CSCSO) lib.util.scm
	$(CSCSO) lib.types.scm
	$(CSCSO) lib.reader.scm
	$(CSCSO) lib.printer.scm
	$(CSCSO) lib.env.scm
	$(CSCSO) lib.core.scm
	$(CSC) step0_repl.scm
	$(CSC) step1_read_print.scm
	$(CSC) step2_eval.scm
	$(CSC) step3_env.scm
	$(CSC) step4_if_fn_do.scm
	$(CSC) step5_tco.scm
	$(CSC) step6_file.scm
	$(CSC) step7_quote.scm
	$(CSC) step8_macros.scm
	$(CSC) step9_try.scm
	$(CSC) stepA_mal.scm

cyclone:
	$(CYCLONE) lib/util.sld
	$(CYCLONE) lib/types.sld
	$(CYCLONE) lib/reader.sld
	$(CYCLONE) lib/printer.sld
	$(CYCLONE) lib/env.sld
	$(CYCLONE) lib/core.sld
	$(CYCLONE) step0_repl.scm
	$(CYCLONE) step1_read_print.scm
	$(CYCLONE) step2_eval.scm
	$(CYCLONE) step3_env.scm
	$(CYCLONE) step4_if_fn_do.scm
	$(CYCLONE) step5_tco.scm
	$(CYCLONE) step6_file.scm
	$(CYCLONE) step7_quote.scm
	$(CYCLONE) step8_macros.scm
	$(CYCLONE) step9_try.scm
	$(CYCLONE) stepA_mal.scm

clean:
	$(RM) lib/*.scm lib/*.so lib/*.c lib/*.o lib/*.meta
	$(RM) lib.*.scm *.so *.c *.o $(BINS)
	$(RMR) out

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

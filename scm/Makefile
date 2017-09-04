SOURCES_BASE = lib/reader.sld lib/printer.sld lib/types.sld
SOURCES_LISP = lib/env.sld lib/func.sld lib/core.sld stepA_mal.scm
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)
BINS = step0_repl step1_read_print step2_eval step3_env

SYMLINK = ln -sfr
RM = rm -f

CSC = csc -O2 -R r7rs
CSCSO = csc -O2 -R r7rs -sJ
CYCLONE = cyclone -O2

all: symlinks

.PHONY: symlinks chicken cyclone clean stats stats-lisp

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

chicken:
	$(CSCSO) lib.util.scm
	$(CSCSO) lib.types.scm
	$(CSCSO) lib.reader.scm
	$(CSCSO) lib.printer.scm
	$(CSCSO) lib.env.scm
	$(CSC) step0_repl.scm
	$(CSC) step1_read_print.scm
	$(CSC) step2_eval.scm
	$(CSC) step3_env.scm

cyclone:
	$(CYCLONE) lib/util.sld
	$(CYCLONE) lib/types.sld
	$(CYCLONE) lib/reader.sld
	$(CYCLONE) lib/printer.sld
	$(CYCLONE) lib/env.sld
	$(CYCLONE) step0_repl.scm
	$(CYCLONE) step1_read_print.scm
	$(CYCLONE) step2_eval.scm
	$(CYCLONE) step3_env.scm

clean:
	$(RM) lib/*.scm lib/*.so lib/*.c lib/*.o lib/*.meta
	$(RM) lib.*.scm *.so *.c *.o $(BINS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

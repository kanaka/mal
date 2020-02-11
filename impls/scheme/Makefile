BINS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco
BINS += step6_file step7_quote step8_macros step9_try stepA_mal
scheme_MODE ?= chibi

CLASSSTEPS = out/step0_repl.class out/step1_read_print.class \
             out/step3_env.class out/step4_if_fn_do.class out/step5_tco.class \
             out/step6_file.class out/step7_quote.class out/step8_macros.class \
             out/step9_try.class out/stepA_mal.class
STEPS = $(if $(filter kawa,$(scheme_MODE)),$(CLASSSTEPS),\
        $(if $(filter chicken,$(scheme_MODE)),$(BINS),\
        $(if $(filter cyclone,$(scheme_MODE)),$(BINS))))

KAWA_STEP1_DEPS = out/lib/util.class out/lib/reader.class \
                  out/lib/printer.class out/lib/types.class
KAWA_STEP3_DEPS = $(KAWA_STEP1_DEPS) out/lib/env.class
KAWA_STEP4_DEPS = $(KAWA_STEP3_DEPS) out/lib/core.class
GAUCHE_STEP1_DEPS = lib/util.scm lib/reader.scm lib/printer.scm lib/types.scm
GAUCHE_STEP3_DEPS = $(GAUCHE_STEP1_DEPS) lib/env.scm
GAUCHE_STEP4_DEPS = $(GAUCHE_STEP3_DEPS) lib/core.scm
CHICKEN_STEP1_DEPS = eggs/lib.util.so eggs/lib.types.so \
                     eggs/lib.reader.so eggs/lib.printer.so
CHICKEN_STEP3_DEPS = $(CHICKEN_STEP1_DEPS) eggs/lib.env.so
CHICKEN_STEP4_DEPS = $(CHICKEN_STEP3_DEPS) eggs/lib.core.so
CYCLONE_STEP1_DEPS = lib/util.so lib/reader.so lib/printer.so lib/types.so
CYCLONE_STEP3_DEPS = $(CYCLONE_STEP1_DEPS) lib/env.so
CYCLONE_STEP4_DEPS = $(CYCLONE_STEP3_DEPS) lib/core.so

STEP1_DEPS = $(if $(filter kawa,$(scheme_MODE)),$(KAWA_STEP1_DEPS),\
             $(if $(filter gauche,$(scheme_MODE)),$(GAUCHE_STEP1_DEPS),\
             $(if $(filter chicken,$(scheme_MODE)),$(CHICKEN_STEP1_DEPS),\
             $(if $(filter cyclone,$(scheme_MODE)),$(CYCLONE_STEP1_DEPS)))))
STEP3_DEPS = $(if $(filter kawa,$(scheme_MODE)),$(KAWA_STEP3_DEPS),\
             $(if $(filter gauche,$(scheme_MODE)),$(GAUCHE_STEP3_DEPS),\
             $(if $(filter chicken,$(scheme_MODE)),$(CHICKEN_STEP3_DEPS),\
             $(if $(filter cyclone,$(scheme_MODE)),$(CYCLONE_STEP3_DEPS)))))
STEP4_DEPS = $(if $(filter kawa,$(scheme_MODE)),$(KAWA_STEP4_DEPS),\
             $(if $(filter gauche,$(scheme_MODE)),$(GAUCHE_STEP4_DEPS),\
             $(if $(filter chicken,$(scheme_MODE)),$(CHICKEN_STEP4_DEPS),\
             $(if $(filter cyclone,$(scheme_MODE)),$(CYCLONE_STEP4_DEPS)))))

KAWALIB = kawa --r7rs --no-warn-unused -d out -C
KAWA = kawa --r7rs --no-warn-unused -d out --main -C
CHICKEN = CHICKEN_REPOSITORY=$(CURDIR)/eggs csc -O3 -R r7rs
CHICKENLIB = $(CHICKEN) -sJ
CYCLONELIB = cyclone -O2
CYCLONE = $(CYCLONELIB)

SCMLIB = $(if $(filter kawa,$(scheme_MODE)),$(KAWALIB),\
         $(if $(filter chicken,$(scheme_MODE)),$(CHICKENLIB),\
         $(if $(filter cyclone,$(scheme_MODE)),$(CYCLONELIB))))
SCM = $(if $(filter kawa,$(scheme_MODE)),$(KAWA),\
      $(if $(filter chicken,$(scheme_MODE)),$(CHICKEN),\
      $(if $(filter cyclone,$(scheme_MODE)),$(CYCLONE))))

MKDIR = mkdir -p
SYMLINK = ln -sfr
RM = rm -f
RMR = rm -rf

all: $(STEPS)

.PHONY: clean
.PRECIOUS: lib/%.scm eggs/lib.%.scm

eggs/r7rs.so:
	chicken-install -init eggs
	CHICKEN_REPOSITORY=$(CURDIR)/eggs chicken-install r7rs

lib/%.scm: lib/%.sld
	$(SYMLINK) $< $@

eggs/lib.%.scm: lib/%.sld
	$(SYMLINK) $< $@

out/lib/%.class: lib/%.scm
	$(SCMLIB) $<

out/%.class: %.scm
	$(SCM) $<

eggs/lib.%.so: eggs/lib.%.scm
	$(SCMLIB) $<

lib/%.so: lib/%.sld
	$(SCMLIB) $<

%: %.scm
	$(SCM) $<

out/step1_read_print.class out/step2_eval.class: $(STEP1_DEPS)
out/step3_env.class: $(STEP3_DEPS)
out/step4_if_fn_do.class out/step5_tco.class out/step6_file.class out/step7_quote.class out/step8_macros.class out/step9_try.class out/stepA_mal.class: $(STEP4_DEPS)

step0_repl: $(if $(filter chicken,$(scheme_MODE)),eggs/r7rs.so,)

step1_read_print.scm step2_eval.scm: $(STEP1_DEPS)
step3_env.scm: $(STEP3_DEPS)
step4_if_fn_do.scm step5_tco.scm step6_file.scm step7_quote.scm step8_macros.scm step9_try.scm stepA_mal.scm: $(STEP4_DEPS)

clean:
	$(RM) lib/*.scm lib/*.so lib/*.c lib/*.o lib/*.meta
	$(RM) lib.*.scm *.so *.c *.o $(BINS)
	$(RM) eggs/*
	$(RMR) out

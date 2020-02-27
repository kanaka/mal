ifdef DEBUG
  ADAFLAGS := -Wall -Wextra -gnatw.eH.Y -gnatySdouxy -gnatVa -g -gnataEfoqQ \
    -fstack-check -pg
  LDFLAGS  := -pg
else
  # -O3 is not recommended as the default by the GCC documentation,
  # and -O2 seems to produce slightly better performances.
  ADAFLAGS := -O2 -gnatnp
endif

# Compiler arguments.
CARGS = $(ADAFLAGS)
# Linker arguments.
LARGS = $(LDFLAGS) -lreadline

step0  := step0_repl
step13 := step1_read_print \
          step2_eval \
          step3_env
step49 := step4_if_fn_do \
          step5_tco \
          step6_file \
          step7_quote \
          step8_macros \
          step9_try
stepa  := stepA_mal
steps  := $(step0) $(step13) $(step49) $(stepa)

.PHONY: all clean
all: $(steps)
clean:
	$(RM) *~ *.adt *.ali *.o b~*.ad[bs] gmon.out $(steps)

# Tell Make how to detect out-of-date executables, and let gnatmake do
# the rest when it must be executed.
sources = $(foreach unit,$1,$(unit).adb $(unit).ads)
TYPES := $(call sources,\
  envs \
  err \
  garbage_collected \
  printer \
  reader \
  readline \
  types \
  types-atoms \
  types-builtins \
  types-fns \
  types-maps \
  types-sequences \
  types-strings \
)
CORE := $(call sources,\
  core \
)

$(step0) :      %:      %.adb
$(step13):      %:      %.adb $(TYPES)
$(step49):      %:      %.adb $(TYPES) $(CORE)
$(stepa) : stepA%: stepa%.adb $(TYPES) $(CORE)
$(steps) :
	gnatmake $< -o $@ -cargs $(CARGS) -largs $(LARGS)

.PHONY: steps.diff
steps.diff:
	diff -u step0_*.adb step1_*.adb || true
	diff -u step1_*.adb step2_*.adb || true
	diff -u step2_*.adb step3_*.adb || true
	diff -u step3_*.adb step4_*.adb || true
	diff -u step4_*.adb step5_*.adb || true
	diff -u step5_*.adb step6_*.adb || true
	diff -u step6_*.adb step7_*.adb || true
	diff -u step7_*.adb step8_*.adb || true
	diff -u step8_*.adb step9_*.adb || true
	diff -u step9_*.adb stepa_*.adb || true

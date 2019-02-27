ifdef DEBUG
  ADAFLAGS := -Wall -Wextra -gnatw.eH.Y -gnatySdouxy -gnatVa -g -gnataEfoqQ \
    -fstack-check -pg
  LDFLAGS  := -pg
else
  # -O3 is not recommended as the default by the GCC documentation,
  # and -O2 seems to produce slightly better performances.
  # See README for a discussion of -gnatp.
  ADAFLAGS := -O2 -gnatnp
endif

# Compiler arguments.
CARGS = -gnat2012 $(OPT) $(ADAFLAGS)
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
	$(RM) *~ *.ali *.o b~*.ad[bs] gmon.out $(steps)

# Tell Make how to detect out-of-date executables, and let gnatmake do
# the rest when it must be executed.
TYPES := \
  environments.ads         environments.adb         \
  printer.ads              printer.adb              \
  reader.ads               reader.adb               \
  types-atoms.ads          types-atoms.adb          \
  types-builtins.ads       types-builtins.adb       \
  types-functions.ads      types-functions.adb      \
  types-lists.ads          types-lists.adb          \
  types-mal.ads            types-mal.adb            \
  types-maps.ads           types-maps.adb           \
  types-symbols-names.ads                           \
  types-symbols.ads        types-symbols.adb        \
  types.ads
CORE := \
  core.ads           core.adb

$(step0) :      %:      %.adb
$(step13):      %:      %.adb $(TYPES)
$(step49):      %:      %.adb $(TYPES) $(CORE)
$(stepa) : stepA%: stepa%.adb $(TYPES) $(CORE)
$(steps) :
	gnatmake $< -o $@ -cargs $(CARGS) -largs $(LARGS)

.PHONY: steps.diff
steps.diff:
	diff -u step1*.adb step2*.adb; \
	diff -u step2*.adb step3*.adb; \
	diff -u step3*.adb step4*.adb; \
	diff -u step4*.adb step5*.adb; \
	diff -u step5*.adb step6*.adb; \
	diff -u step6*.adb step7*.adb; \
	diff -u step7*.adb step8*.adb; \
	diff -u step8*.adb step9*.adb; \
	diff -u step9*.adb stepa*.adb || true

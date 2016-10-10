# Variables expected on the command line:
OPT      := -O2
GNATN    := -gnatn
GNATP    := -gnatp
ADAFLAGS :=
LDFLAGS  :=
DEBUG    :=

ifdef DEBUG
  # Some warnings require -O1.
  OPT      := -O1
  GNATN    :=
  GNATP    :=
  ADAFLAGS := -Wall -Wextra -gnatw.eH.Y -gnatySdouxy -gnatVa -g -gnataEfoqQ \
    -fstack-check -pg
  LDFLAGS  := -pg
endif

# Compiler arguments.
CARGS = -gnat2012 $(OPT) $(GNATN) $(GNATP) $(ADAFLAGS)
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
  atoms.ads          atoms.adb          \
  environments.ads   environments.adb   \
  lists.ads          lists.adb          \
  maps.ads           maps.adb           \
  names.ads                             \
  printer.ads        printer.adb        \
  reader.ads         reader.adb         \
  types.ads          types.adb          \
  strings.ads        strings.adb
CORE := \
  core.ads           core.adb

$(step0) :      %:      %.adb
$(step13):      %:      %.adb $(TYPES)
$(step49):      %:      %.adb $(TYPES) $(CORE)
$(stepa) : stepA%: stepa%.adb $(TYPES) $(CORE)
$(steps) :
	gnatmake $< -o $@ -cargs $(CARGS) -largs $(LARGS)

# Step 8 freezes during the "(or)" test with -gnatp.
step8%: GNATP :=

# The compiler crashes on types.adb with -gnatn.
$(step13) $(step49) $(stepa): types.o
types.o: GNATN :=
types.o: $(TYPES)
	gcc -c $(CARGS) types.adb

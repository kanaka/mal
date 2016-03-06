STEP0_DEPS = mal_readline.c mal_readline.h
STEP1_DEPS = $(STEP0_DEPS) types.h types.m reader.h reader.m printer.h printer.m
STEP2_DEPS = $(STEP1_DEPS)
STEP3_DEPS = $(STEP2_DEPS) env.m
STEP4_DEPS = $(STEP3_DEPS) malfunc.h malfunc.m core.h core.m

SOURCES = $(STEP4_DEPS) stepA_mal.m
SOURCES_LISP = env.h env.m core.h core.m stepA_mal.m

STEPS = step0_repl step1_read_print step2_eval step3_env \
	step4_if_fn_do step5_tco step6_file step7_quote \
	step8_macros step9_try stepA_mal

# From: https://blog.tlensing.org/2013/02/24/objective-c-on-linux-setting-up-gnustep-clang-llvm-objective-c-2-0-blocks-runtime-gcd-on-ubuntu-12-04/:
#   clang `gnustep-config --objc-flags` -o main -x objective-c main.m -fconstant-string-class=NSConstantString -fobjc-nonfragile-abi -fblocks -lgnustep-base -lgnustep-gui -ldispatch -I/usr/local/include/GNUstep -L/usr/local/lib/GNUstep

OS := $(shell uname)

## Bizzare gnustep-config/make interaction causes make to get run
## during gnustep-config so we need to remove make output
ifeq ($(OS),Darwin)
CC = clang -framework Foundation
OBJC_LIBS := -lobjc -lreadline
else
#CC = clang -fblocks -fobjc-nonfragile-abi -fobjc-arc
CC = clang -fblocks -fobjc-nonfragile-abi
OBJC_FLAGS := $(shell gnustep-config --objc-flags | egrep -v "Entering|Leaving")
OBJC_LIBS := $(filter-out -shared-libgcc,$(shell gnustep-config --base-libs | egrep -v "Entering|Leaving")) -ldispatch -lreadline
endif

all: $(STEPS)

dist: mal

mal: stepA_mal
	cp $< $@

step0_repl: $(STEP0_DEPS)
step1_read_print: $(STEP1_DEPS)
step2_eval: $(STEP2_DEPS)
step3_env: $(STEP3_DEPS)
step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal: $(STEP4_DEPS)

step%: step%.m
	$(CC) \
	    -xobjective-c $(filter-out %.h mal_readline%,$+) \
	    -xc mal_readline.c \
	    -o $@ \
	    $(OBJC_FLAGS) \
	    $(OBJC_LIBS)

clean:
	rm -f $(STEPS) *.o *.d mal

.PHONY: stats tests

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

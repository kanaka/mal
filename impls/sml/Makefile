STEP_BINS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step6_file step7_quote step8_macros step9_try stepA_mal

sml_MODE_DEFAULT = polyml
sml_MODE_CONFIG  = .smlmode

ifeq ($(sml_MODE),)
sml_MODE = $(sml_MODE_DEFAULT)
endif
# some hackery to let Make know if it needs to rebuild when sml_MODE changes
ifneq ($(sml_MODE),$(shell cat $(sml_MODE_CONFIG) 2> /dev/null))
$(shell rm $(sml_MODE_CONFIG) 2> /dev/null)
endif

ifeq ($(sml_MODE),mlton)
SMLC        = mlton
SMLCOUTFLAG = -output
BUILD_FILE  = %.mlb
build_args  = $1
endif
ifeq ($(sml_MODE),mosml)
SMLC        = mosmlc
SMLCOUTFLAG = -o
BUILD_FILE  = %.mlb
build_args  = LargeInt.sml -toplevel $(shell grep "\\.sml" $1)
endif
ifeq ($(sml_MODE),polyml)
SMLC        = polyc
SMLCOUTFLAG = -o
BUILD_FILE  = .%.poly.sml
build_args  = $1
endif

all: $(STEP_BINS)

dist: mal

mal: stepA_mal
	cp $< $@

.%.dep: %.mlb
	@echo sml-deps -o $@ $<
	$(eval DEPS := $(shell grep "\\.sml" $<))
	@echo "$(@:.%.dep=%) $@: $(DEPS)" > $@

include $(STEP_BINS:%=.%.dep)

.%.poly.sml: %.mlb
	@echo generate-sml -o $@ $<
	@grep "\\.sml" $< | grep -v main | xargs printf "use \"%s\";\n" > $@

# some hackery to let Make track changes in sml_MODE
$(sml_MODE_CONFIG):
	@echo $(sml_MODE) > $@

$(STEP_BINS): %: $(BUILD_FILE) $(sml_MODE_CONFIG)
	$(SMLC) $(SMLCOUTFLAG) $@ $(call build_args,$<)

clean:
	rm -f $(STEP_BINS) .*.dep *.ui *.uo .*.poly.sml $(sml_MODE_CONFIG)

.PHONY: all clean

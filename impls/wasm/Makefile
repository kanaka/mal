MODE ?= $(strip \
          $(if $(filter wace_libc,$(wasm_MODE)),\
	    libc,\
	    $(if $(filter direct node js wace_fooboot warpy,$(wasm_MODE)),\
	      direct,\
	      wasi)))

EXT = $(if $(filter lucet,$(wasm_MODE)),.so,.wasm)

WASM_AS ?= wasm-as
WAMP ?= node_modules/.bin/wamp
LUCETC ?= lucetc-wasi

STEP0_DEPS = $(WAMP) platform_$(MODE).wam string.wam printf.wam
STEP1_DEPS = $(STEP0_DEPS) types.wam mem.wam debug.wam reader.wam printer.wam
STEP3_DEPS = $(STEP1_DEPS) env.wam
STEP4_DEPS = $(STEP3_DEPS) core.wam

STEPS = step0_repl step1_read_print step2_eval step3_env \
        step4_if_fn_do step5_tco step6_file step7_quote \
        step8_macros step9_try stepA_mal

all: $(foreach s,$(STEPS),$(s)$(EXT))

node_modules/.bin/wamp:
	npm install

%.wat: %.wam
	$(WAMP) $(filter %.wam,$^) > $*.wat

%.wasm: %.wat
	$(WASM_AS) $< -o $@

# lucet object binaries
%.so: %.wasm
	$(LUCETC) $< -o $@

step0_repl.wat: $(STEP0_DEPS)
step1_read_print.wat step2_eval.wat: $(STEP1_DEPS)
step3_env.wat: $(STEP3_DEPS)
step4_if_fn_do.wat step5_tco.wat step6_file.wat: $(STEP4_DEPS)
step7_quote.wat step8_macros.wat step9_try.wat stepA_mal.wat: $(STEP4_DEPS)

.PHONY: clean

clean:
	rm -f *.wat *.wasm *.so

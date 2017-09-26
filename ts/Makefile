SOURCES_BASE = types.ts reader.ts printer.ts
SOURCES_LISP = env.ts core.ts stepA_mal.ts
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

STEPS = step0_repl step1_read_print step2_eval step3_env \
	step4_if_fn_do step5_tco step6_file step7_quote \
	step8_macros step9_try stepA_mal

all: ts

node_modules:
	npm install

step%.js: node_modules types.ts reader.ts printer.ts env.ts core.ts
	./node_modules/.bin/tsc -p ./


.PHONY: ts clean stats tests $(TESTS)

ts: $(foreach s,$(STEPS),$(s).js)

clean:
	rm -f *.js

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

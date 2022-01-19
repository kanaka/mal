STEPS = step0_repl

all: ts

node_modules:
	npm install

step%.js: node_modules step%.ts
	/usr/local/bin/tsc -p ./

# %.js: %.ts
# 	tsc $^

.PHONY: all ts clean

ts: $(foreach s,$(STEPS),$(s).js)

clean:
	rm -f *.js

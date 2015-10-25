SOURCES = reader.kt printer.kt types.kt env.kt core.kt readline.kt

SRCS = step0_repl.kt step1_read_print.kt step2_eval.kt step3_env.kt step4_if_fn_do.kt step5_tco.kt
JARS = $(SRCS:%.kt=%.jar)

all: $(JARS)

clean:
	rm -vf $(JARS)

$(JARS): %.jar: src/mal/%.kt $(SOURCES:%.kt=src/mal/%.kt)
	kotlinc src/mal/$(@:%.jar=%.kt) $(SOURCES:%.kt=src/mal/%.kt) -include-runtime -d $@

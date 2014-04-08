#####################

TESTS =

SOURCES = readline.cs types.cs reader.cs printer.cs env.cs core.cs \
	  step7_quote.cs

OTHER_SOURCES = getline.cs

#####################

SRCS = step0_repl.cs step1_read_print.cs step2_eval.cs step3_env.cs \
       step4_if_fn_do.cs step5_tco.cs step6_file.cs step7_quote.cs

LIB_SRCS = $(filter-out step%,$(OTHER_SOURCES) $(SOURCES))

FLAGS = -debug+

#####################

all: mal.exe $(patsubst %.cs,%.exe,$(SRCS))

mal.exe: $(patsubst %.cs,%.exe,$(word $(words $(SOURCES)),$(SOURCES)))
	cp $< $@

mal.dll: $(LIB_SRCS)
	mcs $(FLAGS) -target:library $+ -out:$@

%.exe: %.cs mal.dll
	mcs $(FLAGS) -r:mal.dll $<

clean:
	rm -f *.dll *.exe *.mbd

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \

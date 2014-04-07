#####################

TESTS =

SOURCES = readline.cs types.cs reader.cs printer.cs \
	  step0_repl.cs step1_read_print.cs step2_eval.cs
OTHER_SOURCES = getline.cs

#####################

FLAGS = -debug+

OBJS =
LIB_SRCS = $(filter-out step%,$(OTHER_SOURCES) $(SOURCES))

#####################

all: mal.exe $(patsubst %.cs,%.exe,$(filter step%,$(SOURCES)))

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

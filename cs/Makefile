#####################

TESTS =

SOURCES = readline.h readline.c types.h types.c \
          reader.h reader.c printer.h printer.c \
          env.c core.h core.c interop.h interop.c \
          stepA_more.c

#####################

SRCS = step0_repl.cs
OBJS = $(SRCS:%.cs=%.exe)
BINS = $(OBJS:%.o=%)
OTHER_OBJS = getline.dll

#####################

all: $(BINS) mal.exe

mal.exe: $(word $(words $(OBJS)),$(OBJS))
	cp $< $@

$(OTHER_OBJS): %.dll: %.cs
	mcs -target:library -out:$@ $+

$(OBJS): %.exe: %.cs  $(OTHER_OBJS)
	mcs $(foreach lib,$(OTHER_OBJS),-r:$(lib)) $(@:%.exe=%.cs)

#$(patsubst %.o,%,$(filter step%,$(OBJS))): $(OTHER_OBJS)
#$(BINS): %: %.o
#	gcc $+ -o $@ $(LDFLAGS)

clean:
	rm -f $(OBJS) $(OTHER_OBJS) mal.exe

.PHONY: stats tests $(TESTS)

stats: $(SOURCES)
	@wc $^

tests: $(TESTS)

$(TESTS):
	@echo "Running $@"; \
	./$@ || exit 1; \

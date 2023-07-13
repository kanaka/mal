LD=$(CXX)
DEBUG=-g
CXXFLAGS=-O2 -Wall $(DEBUG) $(INCPATHS) -std=c++17 #-fsanitize=address -fsanitize=leak
LDFLAGS=-O2 $(DEBUG) $(LIBPATHS) -L. -lreadline -lhistory #-fsanitize=address -fsanitize=leak

LIBSOURCES=reader.cc printer.cc core.cc
LIBOBJS=$(LIBSOURCES:%.cc=%.o)

MAINS=$(wildcard step*.cc)
TARGETS=$(MAINS:%.cc=%)

.PHONY:	all clean

.SUFFIXES: .cc .o

all: $(TARGETS)

dist: mal

mal: stepA_mal
	cp $< $@

.deps: *.cc *.hh
	$(CXX) $(CXXFLAGS) -MM *.cc > .deps

$(TARGETS): %: %.o libmal.a
	$(LD) $^ -o $@ $(LDFLAGS)

libmal.a: $(LIBOBJS)
	$(AR) rcs $@ $^

.cc.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf *.o $(TARGETS) libmal.a .deps mal

-include .deps

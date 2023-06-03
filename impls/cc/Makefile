LD=$(CXX)
DEBUG=-g
CXXFLAGS=-O0 -Wall $(DEBUG) $(INCPATHS) -std=c++17
LDFLAGS=-O0 $(DEBUG) $(LIBPATHS) -L. -lreadline -lhistory

#LIBSOURCES=Core.cpp Environment.cpp Reader.cpp ReadLine.cpp String.cpp \
			Types.cpp Validation.cpp
LIBSOURCES=reader.cc printer.cc
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

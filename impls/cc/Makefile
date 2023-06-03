LD=$(CXX)
DEBUG=-ggdb
CXXFLAGS=-O3 -Wall $(DEBUG) $(INCPATHS) -std=c++11
LDFLAGS=-O3 $(DEBUG) $(LIBPATHS) -L. -lreadline -lhistory

LIBSOURCES=Core.cpp Environment.cpp Reader.cpp ReadLine.cpp String.cpp \
			Types.cpp Validation.cpp
LIBOBJS=$(LIBSOURCES:%.cpp=%.o)

MAINS=$(wildcard step*.cc)
TARGETS=$(MAINS:%.cc=%)

.PHONY:	all clean

.SUFFIXES: .cc .o

all: $(TARGETS)

dist: mal

mal: stepA_mal
	cp $< $@

.deps: *.cc *.h
	$(CXX) $(CXXFLAGS) -MM *.cc > .deps

$(TARGETS): %: %.o #libmal.a
	$(LD) $^ -o $@ $(LDFLAGS)

libmal.a: $(LIBOBJS)
	$(AR) rcs $@ $^

.cc.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf *.o $(TARGETS) libmal.a .deps mal

-include .deps

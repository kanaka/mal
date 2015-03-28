uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Darwin)
	# Native build on yosemite. Requires: brew install readline
	CXX=g++
	READLINE=/usr/local/Cellar/readline/6.3.8
	INCPATHS=-I$(READLINE)/include
	LIBPATHS=-L$(READLINE)/lib
else
	# Ubuntu 14.10 / docker
	CXX=g++-4.9
endif

LD=$(CXX)
AR=ar

DEBUG=-ggdb
CXXFLAGS=-O3 -Wall $(DEBUG) $(INCPATHS) -std=c++11
LDFLAGS=-O3 $(DEBUG) $(LIBPATHS) -L. -lreadline -lhistory

LIBSOURCES=Core.cpp Environment.cpp Reader.cpp ReadLine.cpp String.cpp \
			Types.cpp Validation.cpp
LIBOBJS=$(LIBSOURCES:%.cpp=%.o)

MAINS=$(wildcard step*.cpp)
TARGETS=$(MAINS:%.cpp=%)

.PHONY:	all clean

.SUFFIXES: .cpp .o

all: $(TARGETS)

.deps: *.cpp *.h
	$(CXX) $(CXXFLAGS) -MM *.cpp > .deps

$(TARGETS): %: %.o libmal.a
	$(LD) $^ -o $@ $(LDFLAGS)

libmal.a: $(LIBOBJS)
	$(AR) rcs $@ $^

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf *.o $(TARGETS) libmal.a .deps

-include .deps


### Stats

.PHONY: stats stats-lisp

stats: $(LIBSOURCES) stepA_mal.cpp
	@wc $^

stats-lisp: Core.cpp Environment.cpp stepA_mal.cpp
	@wc $^

################################################################################
#
# Makefile for the Swift implementation of MAL.
#
# The MAL project consists of building up a dialect/subset of Clojure over a
# series of steps. Each step implements a new feature or concept in an easily
# understandable and approachable manner. Each step can be built on its own and
# tested. Each step is built from a step-specific "step.swift" file and a set
# of files common to all steps.
#
# The general approach in this file is to discover the set of "step" source
# files (step0_repl.swift, etc.), and build corresponding executable files
# (step0_repl, etc) from them and from the set of supporting Swift files.
# Since the set of "step" files is discovered on-the-fly, the rules to make
# those files are also generated on-the-fly using $(eval).
#
# The various "step0_repl.swift", etc., source files are actually generated
# from a file called "templates/step.swift". Since each "step" file
# incrementally builds towards the final, complete "step" file,
# "templates/step.swift" is -- for the most part -- a copy of this final "step"
# file with each line annotated with the step in which that line is introduced.
# Through the use of a simple filter program, the "templates/step.swift" file
# can then be processed to produce each intermediate "step" file. This Makefile
# takes care of performing that processing any time "templates/step.swift"
# changes.
#
# MAKE TARGETS:
#
# 	all:
# 		Make all step targets, (re)generating source files if needed.
# 	alls:
# 		(Re)generate source files, if needed.
# 	step0_repl, step1_read_print, etc.:
# 		Make the corresponding step target.
# 	s0...sN:
# 		Shortcuts for the previous targets.
# 	step0_repl.swift, step1_read_print.swift, etc.:
# 		(Re)generate source files for the corresponding step target, if
# 		needed.
# 	ss0...ssN:
# 		Shortcuts for the previous targets.
# 	clean:
# 		Delete all built executables. Generated source files are *not*
# 		deleted.
# 	dump:
# 		Print some Make variables for debugging.
#
# TODO:
#	* Compile each .swift file into an intermediate .o file and link the .o
#	  files, rather than performing a complete build of all files any time
#	  any one of them is out-of-date. Here are the commands generated when
#	  using `swiftc -v`:
#
#	  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/swift \
#	  	-frontend \
#	  	-c \
#	  	-primary-file stepA_mal.swift \
#	  	./core.swift \
#	  	./env.swift \
#	  	./main.swift \
#	  	./printer.swift \
#	  	./reader.swift \
#	  	./readline.swift \
#	  	./types.swift \
#	  	-target x86_64-apple-darwin14.1.0 \
#	  	-target-cpu core2 \
#	  	-sdk /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk \
#	  	-import-objc-header ./bridging-header.h \
#	  	-color-diagnostics \
#	  	-Onone \
#	  	-ledit \
#	  	-module-name stepA_mal \
#	  	-o /var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/stepA_mal-e0a836.o
#	  ... Similar for each source file...
#	  /usr/bin/ld \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/stepA_mal-e0a836.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/core-28b620.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/env-5d8422.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/main-e79633.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/printer-cdd3e5.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/reader-bb188a.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/readline-53df55.o \
#	  	/var/folders/dj/p3tx6v852sl88g79qvhhc2ch0000gp/T/types-7cb250.o \
#	  	-L /usr/lib \
#	  	-ledit \
#	  	-syslibroot \
#	  	/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk \
#	  	-lSystem \
#	  	-arch x86_64 \
#	  	-L /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/macosx \
#	  	-rpath /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/macosx \
#	  	-macosx_version_min 10.10.0 \
#	  	-no_objc_category_merging \
#	  	-o stepA_mal
#
#	* Consider adding a clean-dist (or similar) that deletes the generated
#	  "step" source files.
#
################################################################################

#
# Discover the set of "step" source files (those having the form
# "step<#>_foo.swift")
#
SRCS := $(wildcard ./step*.swift)

#
# From the set of "step" source files, generate the set of executable files
# (those having the form "step<#>_foo")
#
EXES := $(patsubst %.swift,%,$(SRCS))

#
# Also generate references to any debug-symbol directories we may make when -g
# is specified.
#
DSYMS := $(patsubst %.swift,%.dSYM,$(SRCS))

#
# Given a name like "./step<#>_foo", return <#>.
#
# (Is there a better way to do this? $(patsubst) seems to be the most
# appropriate built-in command, but it doesn't seem powerful enough.)
#
# (I've included a `sed` version in case relying on bash is contraindicated.)
#
get_step_number = $(shell echo $(1) | sed -e "s/.*step\(.\).*/\1/")
#get_step_number = $(shell [[ $(1) =~ step(.)_.* ]] ; echo $${BASH_REMATCH[1]})

#
# Working from the list of discovered "step<#>_foo.swift" files, generate the
# list of step numbers.
#
get_all_step_numbers = $(foreach SRC,$(SRCS),$(call get_step_number,$(SRC)))

#
# Generate the dependencies for the "all" target. This list has the form
# "s0 s1 ... sN" for all N returned by get_all_step_numbers. That is:
#
# 	all: s0 s1 ... sN
#
# Also create an "alls" target that just regenerates all the "step" files from
# the corresponding template file.
#
$(eval all: $(patsubst %,s%,$(call get_all_step_numbers)))
$(eval alls: $(patsubst %,ss%,$(call get_all_step_numbers)))

#
# Generate the dependencies for the ".PHONY" target. That is:
#
# 	.PHONY: all clean dump s0 s1 ... sN
#
$(eval .PHONY: all clean dump $(patsubst %,s%,$(call get_all_step_numbers)))

#
# Define the "EZ" targets, where "s0" builds "step0_repl", "s1" builds
# "step1_read_print", etc. That is:
#
# 	s0: step0_repl
# 	s1: step1_read_print
# 	...
# 	sN: stepN_foo
#
# Also create corresponding targets that rebuild the sources files:
#
# 	ss0: step0_repl.swift
# 	ss1: step1_read_print.swift
# 	...
# 	ssN: stepN_foo.swift
#
$(foreach EXE,$(EXES),$(eval s$(call get_step_number,$(EXE)): $(EXE)))
$(foreach SRC,$(SRCS),$(eval ss$(call get_step_number,$(SRC)): $(SRC)))

#
# Various helpful variables.
#
DEV_DIR		:= $(firstword $(wildcard /Applications/Xcode-beta.app /Applications/Xcode.app))
SWIFT		:= $(shell DEVELOPER_DIR="$(DEV_DIR)" xcrun --find swiftc 2>/dev/null)
SDKROOT		:= $(shell DEVELOPER_DIR="$(DEV_DIR)" xcrun --show-sdk-path 2>/dev/null)
STEP_TEMPLATE	:= ./templates/step.swift
FILTER	 	:= ./templates/filter_steps.sh
UTIL_SRC	:= $(filter-out $(STEP_TEMPLATE) $(SRCS),$(wildcard ./*.swift))
ifndef TYPES
TYPES		:= CLASS
endif
ifeq ($(TYPES), ENUM)
UTIL_SRC	:= $(filter-out ./types_class.swift,$(UTIL_SRC))
else
UTIL_SRC	:= $(filter-out ./types_enum.swift,$(UTIL_SRC))
endif
OPT		:= -Ounchecked -whole-module-optimization
DEBUG		:= #-g
EXTRA		:= #-v
COMMON		:= $(UTIL_SRC) $(OPT) $(DEBUG) $(EXTRA) -import-objc-header ./bridging-header.h -L /usr/lib -ledit -sdk $(SDKROOT)

#
# Build the executable from the input sources consisting of the appropriate
# "step" file and the supporting files in $(UTIL_SRC).
#
$(EXES) : % : %.swift $(UTIL_SRC) ./Makefile
	@echo "Making    : $@"
	@$(SWIFT) $< $(COMMON) -o $@

#
# Build the "step" source file ("step<#>_foo.swift") from the step template
# file that combines all the steps in one file.
#
$(SRCS) : % : $(STEP_TEMPLATE) ./Makefile
	@echo "Generating: $@"
	@$(FILTER) $(call get_step_number,$@) $< $@

#
# Delete all of the build output (other than generated "step" source files)
#
clean:
	@rm -rf $(EXES) $(DSYMS)

#
# Display some variables for debugging.
#
dump:
	@echo "   SRCS = $(SRCS)"
	@echo "   EXES = $(EXES)"
	@echo "  DSYMS = $(DSYMS)"
	@echo "   UTIL = $(UTIL_SRC)"
	@echo "  SWIFT = $(SWIFT)"
	@echo "SDKROOT = $(SDKROOT)"
	@echo "  STEPS = $(call get_all_step_numbers)"

#
# Display source stats
#
.PHONY: stats tests $(TESTS)

stats: bridging-header.h $(UTIL_SRC) ./stepA_mal.swift
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: ./env.swift ./core.swift ./stepA_mal.swift
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"


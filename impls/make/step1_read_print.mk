#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)readline.mk
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk

SHELL := /bin/bash

# READ: read and parse input
define READ
$(READ_STR)
endef

# EVAL: just return the input
define EVAL
$(if $(__ERROR)\
,,$1)
endef


# PRINT:
define PRINT
$(if $(__ERROR)\
  ,Error$(encoded_colon)$(_SP)$(call _pr_str,$(__ERROR),yes)$(rem \
  ),$(call _pr_str,$1,yes))
endef

# REPL: read, eval, print, loop

REP = $(call PRINT,$(call EVAL,$(READ)))

# The foreach does nothing when line is empty (EOF).
define REPL
$(foreach line,$(call READLINE,user>$(_SP))\
,$(eval __ERROR :=)$(rem \
)$(call print,$(call REP,$(line:ok=)))$(rem \
)$(call REPL))
endef

# repl loop
$(REPL)

# Do not complain that there is no target.
.PHONY: none
none:
	@true

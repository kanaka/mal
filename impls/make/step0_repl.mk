#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)readline.mk
include $(_TOP_DIR)util.mk

SHELL := /bin/bash

define READ
$1
endef

define EVAL
$1
endef

define PRINT
$1
endef

REP = $(call PRINT,$(call EVAL,$(READ)))

# The foreach does nothing when line is empty (EOF).
define REPL
$(foreach line,$(call READLINE,user>$(_SP))\
,$(eval __ERROR :=)$(rem \
)$(call print,$(call REP,$(line:ok=)))$(rem \
)$(call REPL))
endef

# Call the read-eval-print loop
$(REPL)

# Do not complain that there is no target.
.PHONY: none
none:
	@true

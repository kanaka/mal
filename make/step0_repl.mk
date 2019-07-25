#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)readline.mk

SHELL := /bin/bash

define READ
$(call READLINE)
endef

define EVAL
$(if $(READLINE_EOF),,$(1))
endef

define PRINT
$(1)
endef

REP = $(call PRINT,$(strip $(call EVAL,$(strip $(call READ)))))
REPL = $(info $(call REP))$(if $(READLINE_EOF),,$(call REPL))

# Call the read-eval-print loop
$(call REPL)

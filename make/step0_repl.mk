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
$(if $(READLINE_EOF),,\
  $(if $(findstring =,$(1)),$(eval $(1))$($(word 1,$(1))),$(eval __return := $(1))$(__return)))
endef

define PRINT
$(1)
endef

REP = $(call PRINT,$(strip $(call EVAL,$(strip $(call READ)))))
REPL = $(info $(call REP))$(if $(READLINE_EOF),,$(call REPL))

# Call the read-eval-print loop
$(call REPL)

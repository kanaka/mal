#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk

SHELL := /bin/bash
INTERACTIVE ?= yes

# READ: read and parse input
define READ
$(if $(READLINE_EOF)$(__ERROR),,$(call READ_STR,$(if $(1),$(1),$(call READLINE,"user> "))))
endef

# EVAL: just return the input
define EVAL
$(if $(READLINE_EOF)$(__ERROR),,$(1))
endef

# PRINT:
define PRINT
$(if $(__ERROR),Error: $(call _pr_str,$(__ERROR),yes),$(if $(1),$(call _pr_str,$(1),yes)))$(if $(__ERROR),$(eval __ERROR :=),)
endef

# REPL: read, eval, print, loop
REP = $(call PRINT,$(strip $(call EVAL,$(strip $(call READ,$(1))),$(REPL_ENV))))
REPL = $(info $(call REP,$(call READLINE,"user> ")))$(if $(READLINE_EOF),,$(call REPL))

# repl loop
$(if $(strip $(INTERACTIVE)),$(call REPL))

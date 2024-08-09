#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)readline.mk
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk
include $(_TOP_DIR)core.mk

SHELL := /bin/bash
EVAL_DEBUG ?=

# READ: read and parse input
define READ
$(READ_STR)
endef

# EVAL: evaluate the parameter

EVAL_nil     = $1
EVAL_true    = $1
EVAL_false   = $1
EVAL_string  = $1
EVAL_number  = $1
EVAL_keyword = $1

EVAL_symbol = $(or $(call _get,$2,$1),$(call _error,'$(_symbol_val)' not found))

EVAL_vector = $(call vector,$(foreach e,$(_seq_vals),$(call EVAL,$e,$2)))

# First foreach defines a constant, second one loops on keys.
define EVAL_map
$(foreach obj,$(call _map_new)\
,$(obj)$(rem $(foreach k,$(_keys)\
  ,$(call _assoc!,$(obj),$k,$(call EVAL,$(call _get,$1,$k),$2)))))
endef

define EVAL_list
$(if $(_seq_vals)\
  ,$(call EVAL_apply,$(_seq_vals),$2)$(rem \
  ),$1)
endef

define EVAL_apply
$(foreach f,$(call EVAL,$(firstword $1),$2)\
,$(if $(__ERROR)\
  ,,$(call _apply,$f,$(foreach a,$(_rest),$(call EVAL,$a,$2)))))
endef

define EVAL
$(if $(__ERROR)\
,,$(if $(EVAL_DEBUG),\
  $(call print,EVAL: $(call _pr_str,$1,yes)))$(rem \
)$(call EVAL_$(_obj_type),$1,$2))
endef


# PRINT:
define PRINT
$(if $(__ERROR)\
  ,Error$(encoded_colon)$(_SP)$(call _pr_str,$(__ERROR),yes)$(rem \
  ),$(call _pr_str,$1,yes))
endef

# REPL:
REPL_ENV := $(call hash-map,$(foreach f,+ - * /\
  ,$(call _symbol,$f) $(call _corefn,$f)))

REP = $(call PRINT,$(call EVAL,$(READ),$(REPL_ENV)))

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

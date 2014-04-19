#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk
include $(_TOP_DIR)core.mk

SHELL := /bin/bash
INTERACTIVE ?= yes
EVAL_DEBUG ?=

# READ: read and parse input
define READ
$(if $(READLINE_EOF)$(__ERROR),,$(call READ_STR,$(if $(1),$(1),$(call READLINE,"user> "))))
endef

# EVAL: evaluate the parameter
define EVAL_AST
$(strip \
  $(and $(EVAL_DEBUG),$(info EVAL_AST: $(call _pr_str,$(1))))\
  $(if $(call _symbol?,$(1)),\
    $(foreach key,$($(1)_value),\
      $(if $(call _contains?,$(2),$(key)),\
        $(call _get,$(2),$(key)),\
        $(call _error,'$(key)' not found in REPL_ENV ($(2))))),\
  $(if $(call _list?,$(1)),\
    $(call _smap,EVAL,$(1),$(2)),\
  $(if $(call _vector?,$(1)),\
    $(call _smap_vec,EVAL,$(1),$(2)),\
  $(if $(call _hash_map?,$(1)),\
    $(foreach new_hmap,$(call __new_obj,hmap),\
      $(foreach v,$(call __get_obj_values,$(1)),\
        $(eval $(v:$(1)_%=$(new_hmap)_%) := $(call EVAL,$($(v)),$(2))))\
      $(eval $(new_hmap)_size := $($(1)_size))\
      $(new_hmap)),\
  $(1))))))
endef

define EVAL_INVOKE
$(if $(__ERROR),,\
  $(and $(EVAL_DEBUG),$(info EVAL_INVOKE: $(call _pr_str,$(1))))\
  $(foreach el,$(call EVAL_AST,$(1),$(2)),\
    $(call _apply,$(call sfirst,$(el)),$(call srest,$(el)))))
endef

define EVAL
$(strip $(if $(__ERROR),,\
  $(and $(EVAL_DEBUG),$(info EVAL: $(call _pr_str,$(1))))\
  $(if $(call _list?,$(1)),\
    $(strip $(call EVAL_INVOKE,$(1),$(2))),\
    $(call EVAL_AST,$(1),$(2)))))
endef


# PRINT:
define PRINT
$(if $(__ERROR),Error: $(call _pr_str,$(__ERROR),yes),$(if $(1),$(call _pr_str,$(1),yes)))$(if $(__ERROR),$(eval __ERROR :=),)
endef

# REPL:
REPL_ENV := $(call _hash_map)
REP = $(call PRINT,$(strip $(call EVAL,$(strip $(call READ,$(1))),$(REPL_ENV))))
REPL = $(info $(call REP,$(call READLINE,"user> ")))$(if $(READLINE_EOF),,$(call REPL))

$(call do,$(call _assoc!,$(REPL_ENV),+,number_plus))
$(call do,$(call _assoc!,$(REPL_ENV),-,number_subtract))
$(call do,$(call _assoc!,$(REPL_ENV),*,number_multiply))
$(call do,$(call _assoc!,$(REPL_ENV),/,number_divide))

# repl loop
$(if $(strip $(INTERACTIVE)),$(call REPL))

#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk
include $(_TOP_DIR)env.mk
include $(_TOP_DIR)core.mk

SHELL := /bin/bash
INTERACTIVE ?= yes
EVAL_DEBUG ?=

# READ: read and parse input
define READ
$(if $(READLINE_EOF)$(__ERROR),,$(call READ_STR,$(if $(1),$(1),$(call READLINE,"user> "))))
endef

# EVAL: evaluate the parameter
define LET
$(strip \
  $(word 1,$(2) \
    $(foreach var,$(call _nth,$(1),0),\
      $(foreach val,$(call _nth,$(1),1),\
        $(call ENV_SET,$(2),$($(var)_value),$(call EVAL,$(val),$(2)))\
        $(foreach left,$(call srest,$(call srest,$(1))),
          $(if $(call _EQ,0,$(call _count,$(left))),\
            ,\
            $(call LET,$(left),$(2))))))))
endef

define EVAL_AST
$(strip \
  $(and $(EVAL_DEBUG),$(info EVAL_AST: $(call _pr_str,$(1))))\
  $(if $(call _symbol?,$(1)),\
    $(foreach key,$($(1)_value),\
      $(call ENV_GET,$(2),$(key))),\
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
  $(and $(EVAL_DEBUG),$(info EVAL_INVOKE: $(call _pr_str,$(1))))
  $(foreach a0,$(call _nth,$(1),0),\
    $(if $(call _EQ,def!,$($(a0)_value)),\
      $(foreach a1,$(call _nth,$(1),1),\
        $(foreach a2,$(call _nth,$(1),2),\
          $(foreach res,$(call EVAL,$(a2),$(2)),\
            $(if $(__ERROR),,\
              $(if $(call ENV_SET,$(2),$($(a1)_value),$(res)),$(res),))))),\
    $(if $(call _EQ,let*,$($(a0)_value)),\
      $(foreach a1,$(call _nth,$(1),1),\
        $(foreach a2,$(call _nth,$(1),2),\
          $(call EVAL,$(a2),$(call LET,$(a1),$(call ENV,$(2)))))),\
      $(foreach el,$(call EVAL_AST,$(1),$(2)),\
        $(call _apply,$(call sfirst,$(el)),$(call srest,$(el))))))))
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
REPL_ENV := $(call ENV)
REP = $(call PRINT,$(strip $(call EVAL,$(strip $(call READ,$(1))),$(REPL_ENV))))
REPL = $(info $(call REP,$(call READLINE,"user> ")))$(if $(READLINE_EOF),,$(call REPL))

# Setup the environment
REPL_ENV := $(call ENV_SET,$(REPL_ENV),+,number_plus)
REPL_ENV := $(call ENV_SET,$(REPL_ENV),-,number_subtract)
REPL_ENV := $(call ENV_SET,$(REPL_ENV),*,number_multiply)
REPL_ENV := $(call ENV_SET,$(REPL_ENV),/,number_divide)

# repl loop
$(if $(strip $(INTERACTIVE)),$(call REPL))

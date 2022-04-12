#
# mal (Make Lisp)
#
_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)readline.mk
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk
include $(_TOP_DIR)env.mk
include $(_TOP_DIR)core.mk

SHELL := /bin/bash

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

EVAL_symbol = $(or $(call ENV_GET,$2,$1),$(call _error,'$(_symbol_val)' not found))

EVAL_vector = $(call vector,$(foreach e,$(_seq_vals),$(call EVAL,$e,$2)))

# First foreach defines a constant, second one loops on keys.
define EVAL_map
$(foreach obj,$(call _map_new)\
,$(obj)$(rem $(foreach k,$(_keys)\
  ,$(call _assoc!,$(obj),$k,$(call EVAL,$(call _get,$1,$k),$2)))))
endef

define EVAL_list
$(if $(_seq_vals)\
  ,$(foreach a0,$(firstword $(_seq_vals))\
    ,$(if $(call _symbol?,$(a0))\
      ,$(foreach dispatch,EVAL_special_$(call _symbol_val,$(a0))\
        ,$(if $(filter undefined,$(flavor $(dispatch)))\
          ,$(call EVAL_apply,$(_seq_vals),$2)$(rem \
          ),$(call $(dispatch),$(call _rest,$(_seq_vals)),$2)))$(rem \
      ),$(call EVAL_apply,$(_seq_vals),$2)))$(rem \
  ),$1)
endef

define EVAL_apply
$(foreach f,$(call EVAL,$(firstword $1),$2)\
,$(if $(__ERROR)\
  ,,$(call _apply,$f,$(foreach a,$(_rest),$(call EVAL,$a,$2)))))
endef

define EVAL_special_def!
$(foreach res,$(call EVAL,$(lastword $1),$2)\
  ,$(if $(__ERROR)\
    ,,$(res)$(call ENV_SET,$2,$(firstword $1),$(res))))
endef

define EVAL_special_let*
$(foreach let_env,$(call ENV,$2)\
,$(call _foreach2,$(call _seq_vals,$(firstword $1))\
  ,$$(call ENV_SET,$(let_env),$$k,$$(call EVAL,$$v,$(let_env))))$(rem \
)$(call EVAL,$(lastword $1),$(let_env)))
endef

EVAL_special_do = $(lastword $(foreach x,$1,$(call EVAL,$x,$2)))

define EVAL_special_if
$(if $(call truthy?,$(call EVAL,$(firstword $1),$2))\
  ,$(call EVAL,$(word 2,$1),$2)$(rem \
),$(if $(word 3,$1)\
  ,$(call EVAL,$(lastword $1),$2)$(rem \
),$(__nil)))
endef

EVAL_special_fn* = $(call _function,$(call _seq_vals,$(firstword $1)),$(lastword $1),$2)

define EVAL
$(if $(__ERROR)\
,,$(if $(call truthy?,$(call ENV_GET,$(2),$(call _symbol,DEBUG-EVAL)))\
  ,$(call print,EVAL: $(call _pr_str,$1,yes)    env: $(call env_keys,$2)))$(rem \
)$(call EVAL_$(_obj_type),$1,$2))
endef


# PRINT:
define PRINT
$(if $(__ERROR)\
  ,Error$(encoded_colon)$(_SP)$(call _pr_str,$(__ERROR),yes)$(rem \
  ),$(call _pr_str,$1,yes))
endef

# REPL:
REPL_ENV := $(call ENV)
REP = $(call PRINT,$(call EVAL,$(READ),$(REPL_ENV)))

# The foreach does nothing when line is empty (EOF).
define REPL
$(foreach line,$(call READLINE,user>$(_SP))\
,$(eval __ERROR :=)$(rem \
)$(call print,$(call REP,$(line:ok=)))$(rem \
)$(call REPL))
endef

# Read and evaluate for side effects but ignore the result.
define RE
$(rem $(call EVAL,$(call READ,$(str_encode_nospace)),$(REPL_ENV)) \
)$(if $(__ERROR)\
  ,$(error during startup: $(call str_decode_nospace,$(call _pr_str,$(__ERROR),yes))))
endef

# core.mk: defined using Make
$(foreach f,$(core_ns)\
  ,$(call ENV_SET,$(REPL_ENV),$(call _symbol,$f),$(call _corefn,$f)))

# core.mal: defined in terms of the language itself
$(call RE, (def! not (fn* (a) (if a false true))) )

# repl loop
$(REPL)

# Do not complain that there is no target.
.PHONY: none
none:
	@true

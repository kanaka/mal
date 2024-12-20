#
# mal (Make a Lisp) object types
#

ifndef __mal_types_included
__mal_types_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)gmsl.mk
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)numbers.mk


# Low-level type implemenation

# magic is \u2344 \u204a
__obj_magic := ⍄⁊
# \u2256
__obj_hash_code := 0


# 1:type  2:optional content  ->  variable name
define __new_obj
$(eval __obj_hash_code := $(call int_add,1,$(__obj_hash_code)))$(rem \
)$(foreach obj,$(__obj_magic)_$(__obj_hash_code)_$1\
  ,$(obj)$(if $2,$(eval $(obj) := $2)))
endef


# Visualize Objects in memory
_visualize_memory = $(foreach v,$(sort $(filter $(__obj_magic)_%,$(.VARIABLES)))\
                      ,$(info $v  $($v)))


# Errors/Exceptions
__ERROR :=
throw = $(eval __ERROR := $1)
_error = $(call throw,$(call _string,$(str_encode_nospace)))


# Constant atomic values
__nil   := _nil
__true  := _true
__false := _false


# General functions

_obj_type = $(lastword $(subst _, ,$1))

_clone_obj = $(_clone_obj_$(_obj_type))
_clone_obj_list     = $(call list,$($1))
_clone_obj_vector   = $(call vector,$($1))
_clone_obj_map      = $(_map_new)
_clone_obj_function = $(call __new_obj,function,$($1))
_clone_obj_corefn   = $(call _corefn,$($1))

define _hash_equal?
$(if $3\
  ,$(and $(call _equal?,$($1_$(firstword $3)),$($2_$(firstword $3))),\
         $(call _hash_equal?,$1,$2,$(call _rest,$3)))$(rem \
  ),true)
endef

define _equal?_seq_loop
$(if $1\
  ,$(and $2,\
         $(call _equal?,$(firstword $1),$(firstword $2)),\
         $(call _equal?_seq_loop,$(_rest),$(call _rest,$2)))$(rem \
  ),$(if $2,,true))
endef

define _equal?
$(or $(filter $1,$2),\
     $(and $(filter %_list %_vector,$1),\
           $(filter %_list %_vector,$2),\
           $(call _equal?_seq_loop,$($1),$($2))),\
     $(and $(filter %_map,$1),\
           $(filter %_map,$2),\
           $(call _EQ,$(_keys),$(call _keys,$2)),\
           $(call _hash_equal?,$1,$2,$(_keys))))
endef

_nil? = $(filter $(__nil),$1)

_true? = $(filter $(__true),$1)

_false? = $(filter $(__false),$1)

# Conveniently for DEBUG-EVAL, returns false if $1 is empty.
truthy? = $(filter-out _nil _false,$1)


# Symbols
_symbol = $1_symbol
_symbol_val = $(1:_symbol=)
_symbol? = $(filter %_symbol,$1)


# Keywords
_keyword = $1_keyword
_keyword? = $(filter %_keyword,$1)
_keyword_val = $(1:_keyword=)


# Numbers
_number = $1_number
_number? = $(filter %_number,$1)
_number_val = $(1:_number=)


# Strings
_string = $1_string
_string? = $(filter %_string,$1)
_string_val = $(1:_string=)

# Functions

_corefn = $(call __new_obj,corefn,$1)
_function = $(call __new_obj,function,$2 $3 $1)
_as_macro = $(call __new_obj,macro,$($1))
_fn? = $(filter %_corefn %_function,$1)
_macro? = $(filter %_macro,$1)

# 1:env 2:formal parameters 3:actual parameters
define _function_set_env
$(if $2\
  ,$(if $(filter &_symbol,$(firstword $2))\
    ,$(call ENV_SET,$1,$(lastword $2),$(call list,$3)),$(rem \
  else \
    $(call ENV_SET,$1,$(firstword $2),$(firstword $3))
    $(call _function_set_env,$1,$(call _rest,$2),$(call _rest,$3)))))
endef

# Takes a function object and a list object of arguments and invokes
# the function with space separated arguments
define _apply
$(if $(filter %_corefn,$1)\
  ,$(call $($1),$2)$(rem \
),$(if $(filter %_function %_macro,$1)\
  ,$(foreach env,$(call ENV,$(word 2,$($1)))\
    ,$(call _function_set_env,$(env),$(call _rest2,$($1)),$2)$(rem \
    )$(call EVAL,$(firstword $($1)),$(env)))$(rem \
),$(call _error,cannot apply non-function)))
endef


# Lists
list = $(if $1,$(call __new_obj,list,$1),empty_list)
_list? = $(filter %_list,$1)

_seq_vals = $($1)


# Vectors (same as lists for now)
vector = $(if $1,$(call __new_obj,vector,$1),empty_vector)
_vector? = $(filter %_vector,$1)


# Hash maps (associative arrays)
# 1:optional source map  2:optional key/value pairs  3:optional removals
define _map_new
$(foreach obj,$(call __new_obj,map,$(filter-out $3,$(if $1,$($1))))\
,$(obj)$(rem \
$(foreach k,$($(obj))\
  ,$(eval $(obj)_$k := $($1_$k)))\
$(call _foreach2,$2\
  ,$$(call _assoc!,$(obj),$$k,$$v))))
endef

_hash_map? = $(filter %_map,$1)


# set a key/value in the hash map
# map key val
# sort removes duplicates.
_assoc! = $(eval $1_$2 := $3)$(eval $1 := $(sort $($1) $2))

_keys = $($1)

# retrieve the value of a plain string key from the hash map, or
# return the empty string if the key is missing
_get = $($1_$2)


# sequence operations

_sequential? = $(filter %_list %_vector,$1)


# Metadata functions

with-meta = $(foreach obj,$(call _clone_obj,$(firstword $1))\
  ,$(obj)$(eval $(obj)_meta := $(lastword $1)))

meta = $(or $($1_meta),$(__nil))


# atoms

atom = $(call __new_obj,atom,$1)
_atom? = $(filter %_atom,$1)
deref = $($1)
_reset = $(eval $1 = $2)


endif

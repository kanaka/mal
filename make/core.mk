#
# mal (Make a Lisp) Core functions
#

ifndef __mal_core_included
__mal_core_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)readline.mk
include $(_TOP_DIR)reader.mk
include $(_TOP_DIR)printer.mk


# Errors/Exceptions
throw = $(eval __ERROR := $(1))


# General functions

# Return the type of the object (or "make" if it's not a object
obj_type = $(call _string,$(call _obj_type,$(1)))

equal? = $(if $(call _equal?,$(word 1,$(1)),$(word 2,$(1))),$(__true),$(__false))


# Scalar functions
nil? = $(if $(call _nil?,$(1)),$(__true),$(__false))
true? = $(if $(call _true?,$(1)),$(__true),$(__false))
false? = $(if $(call _false?,$(1)),$(__true),$(__false))


# Symbol functions
symbol = $(call _symbol,$(call str_decode,$($(1)_value)))
symbol? = $(if $(call _symbol?,$(1)),$(__true),$(__false))

# Keyword functions
keyword = $(call _keyword,$(call str_decode,$($(1)_value)))
keyword? = $(if $(call _keyword?,$(1)),$(__true),$(__false))


# Number functions
number? = $(if $(call _number?,$(1)),$(__true),$(__false))

number_lt = $(if $(call int_lt_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_lte = $(if $(call int_lte_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_gt = $(if $(call int_gt_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_gte = $(if $(call int_gte_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))

number_plus = $(call _pnumber,$(call int_add_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_subtract = $(call _pnumber,$(call int_sub_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_multiply = $(call _pnumber,$(call int_mult_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_divide = $(call _pnumber,$(call int_div_encoded,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))

time_ms = $(call _number,$(shell echo $$(date +%s%3N)))

# String functions

string? = $(if $(call _string?,$(1)),$(__true),$(__false))

pr_str  = $(call _string,$(call _pr_str_mult,$(1),yes, ))
str     = $(call _string,$(call _pr_str_mult,$(1),,))
prn     = $(info $(call _pr_str_mult,$(1),yes, ))
println = $(info $(subst \n,$(NEWLINE),$(call _pr_str_mult,$(1),, )))

readline= $(foreach res,$(call _string,$(call READLINE,"$(call str_decode,$($(1)_value))")),$(if $(READLINE_EOF),$(eval READLINE_EOF :=)$(__nil),$(res)))
read_str= $(call READ_STR,$(1))
slurp   = $(call _string,$(call _read_file,$(call str_decode,$($(1)_value))))

subs = $(strip \
         $(foreach start,$(call int_add,1,$(call int_decode,$($(word 2,$(1))_value))),\
           $(foreach end,$(if $(3),$(call int_decode,$($(3)_value)),$(words $($(word 1,$(1))_value))),\
             $(call _string,$(wordlist $(start),$(end),$($(word 1,$(1))_value))))))



# Function functions
function? = $(if $(call _function?,$(1)),$(__true),$(__false))


# List functions
list? = $(if $(call _list?,$(1)),$(__true),$(__false))


# Vector functions
vector? = $(if $(call _vector?,$(1)),$(__true),$(__false))


# Hash map (associative array) functions
hash_map? = $(if $(call _hash_map?,$(1)),$(__true),$(__false))

# set a key/value in a copy of the hash map
assoc = $(word 1,\
          $(foreach hm,$(call _clone_obj,$(word 1,$(1))),\
            $(hm) \
            $(call _assoc_seq!,$(hm),$(wordlist 2,$(words $(1)),$(1)))))

# unset keys in a copy of the hash map
# TODO: this could be made more efficient by copying only the
#       keys that not being removed.
dissoc = $(word 1,\
           $(foreach hm,$(call _clone_obj,$(word 1,$(1))),\
             $(hm) \
             $(call _dissoc_seq!,$(hm),$(wordlist 2,$(words $(1)),$(1)))))

keys = $(foreach new_list,$(call _list),$(new_list)$(eval $(new_list)_value := $(foreach v,$(call __get_obj_values,$(1)),$(foreach vval,$(word 4,$(subst _, ,$(v))),$(if $(filter $(__keyword)%,$(vval)),$(call _keyword,$(patsubst $(__keyword)%,%,$(vval))),$(call _string,$(vval)))))))

vals = $(foreach new_list,$(call _list),$(new_list)$(eval $(new_list)_value := $(foreach v,$(call __get_obj_values,$(1)),$($(v)))))

# Hash map and vector functions

# retrieve the value of a string key object from the hash map, or
# retrive a vector by number object index
get = $(strip \
        $(if $(call _nil?,$(word 1,$(1))),\
          $(__nil),\
          $(if $(call _hash_map?,$(word 1,$(1))),\
            $(call _get,$(word 1,$(1)),$(call str_decode,$($(word 2,$(1))_value))),\
            $(call _get,$(word 1,$(1)),$(call int_decode,$($(word 2,$(1))_value))))))

contains? = $(if $(call _contains?,$(word 1,$(1)),$(call str_decode,$($(word 2,$(1))_value))),$(__true),$(__false))


# sequence operations

sequential? = $(if $(call _sequential?,$(1)),$(__true),$(__false))

cons = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $(strip $(word 1,$(1)) $(call __get_obj_values,$(word 2,$(1)))))))

concat = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $(strip $(foreach lst,$1,$(call __get_obj_values,$(lst)))))))

nth = $(strip \
        $(if $(call int_lt,$($(word 2,$(1))_value),$(call int_encode,$(call _count,$(word 1,$(1))))),\
          $(word $(call int_add,1,$(call int_decode,$($(word 2,$(1))_value))),$($(word 1,$(1))_value)),\
          $(call _error,nth: index out of range)))

sfirst = $(word 1,$($(1)_value))

slast = $(word $(words $($(1)_value)),$($(1)_value))

empty? = $(if $(call _EQ,0,$(if $(call _hash_map?,$(1)),$($(1)_size),$(words $($(1)_value)))),$(__true),$(__false))

count = $(call _number,$(call _count,$(1)))

conj = $(word 1,$(foreach new_list,$(call __new_obj_like,$(word 1,$(1))),\
                  $(new_list) \
                  $(eval $(new_list)_value := $(strip $($(word 1,$(1))_value))) \
                  $(if $(call _list?,$(new_list)),\
                    $(foreach elem,$(wordlist 2,$(words $(1)),$(1)),\
                      $(eval $(new_list)_value := $(strip $(elem) $($(new_list)_value)))),\
                    $(eval $(new_list)_value := $(strip $($(new_list)_value) $(wordlist 2,$(words $(1)),$(1)))))))

# Creates a new vector/list of the everything after but the first
# element
srest = $(word 1,$(foreach new_list,$(call _list),\
                   $(new_list) \
                   $(eval $(new_list)_value := $(wordlist 2,$(words $($(1)_value)),$($(1)_value)))))

# Takes a space separated arguments and invokes the first argument
# (function object) using the remaining arguments.
sapply = $(call $(word 1,$(1))_value,\
                $(strip \
                  $(wordlist 2,$(call int_sub,$(words $(1)),1),$(1)) \
                  $($(word $(words $(1)),$(1))_value)))

# Map a function object over a list object
smap = $(strip\
         $(foreach func,$(word 1,$(1)),\
           $(foreach lst,$(word 2,$(1)),\
             $(foreach type,list,\
               $(foreach new_hcode,$(call __new_obj_hash_code),\
                 $(foreach sz,$(words $(call __get_obj_values,$(lst))),\
                   $(eval $(__obj_magic)_$(type)_$(new_hcode)_value := $(strip \
                     $(foreach val,$(call __get_obj_values,$(lst)),\
                       $(call $(func)_value,$(val))))))\
                 $(__obj_magic)_$(type)_$(new_hcode))))))


# Metadata functions

with_meta = $(strip \
              $(foreach new_obj,$(call _clone_obj,$(word 1,$(1))),\
                $(eval $(new_obj)_meta := $(strip $(word 2,$(1))))\
                $(new_obj)))

meta = $(strip $($(1)_meta))


# Atom functions

atom = $(strip \
         $(foreach hcode,$(call __new_obj_hash_code),\
           $(foreach new_atom,$(__obj_magic)_atom_$(hcode),\
             $(new_atom)\
             $(eval $(new_atom)_value := $(1)))))
atom? = $(if $(call _atom?,$(1)),$(__true),$(__false))

deref = $($(1)_value)

reset! = $(eval $(word 1,$(1))_value := $(word 2,$(1)))$(word 2,$(1))

swap! = $(foreach resp,$(call $(word 2,$(1))_value,$($(word 1,$(1))_value) $(wordlist 3,$(words $(1)),$(1))),\
          $(eval $(word 1,$(1))_value := $(resp))\
          $(resp))




# Namespace of core functions

core_ns = type obj_type \
          = equal? \
          throw throw \
          nil? nil? \
          true? true? \
          false? false? \
          symbol symbol \
          symbol? symbol? \
          keyword keyword \
          keyword? keyword? \
          function? function? \
          string? string? \
          \
          pr-str pr_str \
          str str \
          prn prn \
          println println \
          readline readline \
          read-string read_str \
          slurp slurp \
          subs subs \
          number? number? \
          < number_lt \
          <= number_lte \
          > number_gt \
          >= number_gte \
          + number_plus \
          - number_subtract \
          * number_multiply \
          / number_divide \
          time-ms time_ms \
          \
          list _list \
          list? list? \
          vector _vector \
          vector? vector? \
          hash-map _hash_map \
          map? hash_map? \
          assoc assoc \
          dissoc dissoc \
          get get \
          contains? contains? \
          keys keys \
          vals vals \
          \
          sequential? sequential? \
          cons cons \
          concat concat \
          nth nth \
          first sfirst \
          rest srest \
          last slast \
          empty? empty? \
          count count \
          conj conj \
          apply sapply \
          map smap \
          \
          with-meta with_meta \
          meta meta \
          atom atom \
          atom? atom? \
          deref deref \
          reset! reset! \
          swap! swap!

endif

#
# mal (Make Lisp) Object Types and Functions
#

ifndef __mal_types_included
__mal_types_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk

# magic is \u2344 \u204a
__obj_magic = ⍄⁊
# \u2256
__equal = ≛
__obj_hash_code = 0

__new_obj_hash_code = $(eval __obj_hash_code := $(call gmsl_plus,1,$(__obj_hash_code)))$(__obj_hash_code)

__new_obj = $(__obj_magic)_$(1)_$(call __new_obj_hash_code)

__new_obj_like = $(foreach type,$(word 2,$(subst _, ,$(1))),$(__obj_magic)_$(type)_$(__new_obj_hash_code))

__get_obj_values = $(strip \
                     $(if $(filter $(__obj_magic)_hmap_%,$(1)),\
                       $(sort $(foreach v,$(filter %_value,$(filter $(1)_%,$(.VARIABLES))),$(if $(call _undefined?,$(v)),,$(v)))),\
                       $($(1)_value)))

__ERROR :=

#
# General functions
#

# Return the type of the object (or "make" if it's not a object
_obj_type = $(strip \
              $(if $(filter $(__obj_magic)_symb_%,$(1)),symbol,\
              $(if $(filter $(__obj_magic)_list_%,$(1)),list,\
              $(if $(filter $(__obj_magic)_numb_%,$(1)),number,\
              $(if $(filter $(__obj_magic)_func_%,$(1)),function,\
              $(if $(filter $(__obj_magic)_strn_%,$(1)),string,\
              $(if $(filter $(__obj_magic)__nil_%,$(1)),nil,\
              $(if $(filter $(__obj_magic)_true_%,$(1)),true,\
              $(if $(filter $(__obj_magic)_fals_%,$(1)),false,\
              $(if $(filter $(__obj_magic)_vect_%,$(1)),vector,\
              $(if $(filter $(__obj_magic)_atom_%,$(1)),atom,\
              $(if $(filter $(__obj_magic)_hmap_%,$(1)),hash_map,\
              $(if $(filter $(__obj_magic)_undf_%,$(1)),undefined,\
              make)))))))))))))
obj_type = $(call string,$(call _obj_type,$(1)))

# return a printable form of the argument, the second parameter is
# 'print_readably' which backslashes quotes in string values
_pr_str = $(if $(1),$(foreach ot,$(call _obj_type,$(1)),$(if $(call _EQ,make,$(ot)),$(call _error,_pr_str failed on $(1)),$(call $(ot)_pr_str,$(1),$(2)))),)

# Like _pr_str but takes multiple values in first argument, the second
# parameter is 'print_readably' which backslashes quotes in string
# values, the third parameter is the delimeter to use between each
# _pr_str'd value
_pr_str_mult = $(call _pr_str,$(word 1,$(1)),$(2))$(if $(word 2,$(1)),$(3)$(call _pr_str_mult,$(wordlist 2,$(words $(1)),$(1)),$(2),$(3)),)

pr_str  = $(call string,$(call _pr_str_mult,$(1),yes, ))
str     = $(call string,$(call _pr_str_mult,$(1),,))
prn     = $(info $(call _pr_str_mult,$(1),yes, ))
println = $(info $(subst \n,$(NEWLINE),$(call _pr_str_mult,$(1),, )))

_clone_obj = $(strip \
               $(foreach new_hcode,$(call __new_obj_hash_code),\
                 $(foreach new_obj,$(foreach type,$(word 2,$(subst _, ,$(1))),$(__obj_magic)_$(type)_$(new_hcode)),\
                   $(if $(filter $(__obj_magic)_hmap_%,$(1)),\
                     $(foreach v,$(call __get_obj_values,$(1)),\
                       $(eval $(v:$(1)_%=$(new_obj)_%) := $($(v))))\
                     $(eval $(new_obj)_size := $($(1)_size)),\
                   $(if $(filter $(__obj_magic)_func_%,$(1)),\
                     $(eval $(new_obj)_value = $(value $(1)_value)),\
                     $(eval $(new_obj)_value := $(strip $($(1)_value)))))\
                   $(new_obj))))

with_meta = $(strip \
              $(foreach new_obj,$(call _clone_obj,$(word 1,$(1))),\
                $(eval $(new_obj)_meta := $(strip $(word 2,$(1))))\
                $(new_obj)))

meta = $(strip $($(1)_meta))


#
# Special atomic values
#
__undefined = $(__obj_magic)_undf_0
__nil = $(__obj_magic)__nil_0
__true = $(__obj_magic)_true_0
__false = $(__obj_magic)_fals_0

_undefined? = $(or $(call _EQ,undefined,$(origin $(1))),$(filter $(__undefined),$($(1))))
undefined? = $(if $(call _undefined?,$(1)),$(__true),$(__false))

_nil? = $(if $(filter $(__obj_magic)__nil_%,$(1)),$(__true),)
nil? = $(if $(call _nil?,$(1)),$(__true),$(__false))
nil_pr_str = nil

_true? = $(if $(filter $(__obj_magic)_true_%,$(1)),$(__true),)
true? = $(if $(call _true?,$(1)),$(__true),$(__false))
true_pr_str = true

_false? = $(if $(filter $(__obj_magic)_fals_%,$(1)),$(__true),)
false? = $(if $(call _false?,$(1)),$(__true),$(__false))
false_pr_str = false


#
# Numbers
#

_pnumber = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_numb_$(hcode)$(eval $(__obj_magic)_numb_$(hcode)_value := $(1)))
number = $(call _pnumber,$(call int_encode,$(1)))

_number? = $(if $(filter $(__obj_magic)_numb_%,$(1)),$(__true),)
number? = $(if $(call _number?,$(1)),$(__true),$(__false))

number_pr_str = $(call int_decode,$($(1)_value))

number_plus = $(call _pnumber,$(call int_plus,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_subtract = $(call _pnumber,$(call int_subtract,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_multiply = $(call _pnumber,$(call int_multiply,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))
number_divide = $(call _pnumber,$(call int_divide,$($(word 1,$(1))_value),$($(word 2,$(1))_value)))

number_gt = $(if $(call int_gt,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_gte = $(if $(call int_gte,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_lt = $(if $(call int_lt,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))
number_lte = $(if $(call int_lte,$($(word 1,$(1))_value),$($(word 2,$(1))_value)),$(__true),$(__false))


#
# Symbols
#
symbol = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_symb_$(hcode)$(eval $(__obj_magic)_symb_$(hcode)_value := $(1)))

_symbol? = $(if $(filter $(__obj_magic)_symb_%,$(1)),$(__true),)
symbol? = $(if $(call _symbol?,$(1)),$(__true),$(__false))

symbol_pr_str = $($(1)_value)

#
# Strings
#
_string = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_strn_$(hcode)$(eval $(__obj_magic)_strn_$(hcode)_value := $(1)))
string = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_strn_$(hcode)$(eval $(__obj_magic)_strn_$(hcode)_value := $(call str_encode,$(1))))

_string? = $(if $(filter $(__obj_magic)_strn_%,$(1)),$(__true),)
string? = $(if $(call _string?,$(1)),$(__true),$(__false))

string_pr_str = $(if $(2),"$(subst $(DQUOTE),$(ESC_DQUOTE),$(subst $(SLASH),$(SLASH)$(SLASH),$(call str_decode,$($(1)_value))))",$(call str_decode,$($(1)_value)))

subs = $(strip \
         $(foreach start,$(call gmsl_plus,1,$(call int_decode,$($(word 2,$(1))_value))),\
           $(foreach end,$(if $(3),$(call int_decode,$($(3)_value)),$(words $($(word 1,$(1))_value))),\
             $(call _string,$(wordlist $(start),$(end),$($(word 1,$(1))_value))))))

#
# Function objects
#

# Return a function object. The first parameter is the
# function/macro 'source'. Note that any $ must be escaped as $$ to be
# preserved and become positional arguments for when the
# function/macro is later invoked.
function = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_func_$(hcode)$(eval $(__obj_magic)_func_$(hcode)_value = $(1)))

_function? = $(if $(filter $(__obj_magic)_func_%,$(1)),$(__true),)
function? = $(if $(call _function?,$(1)),$(__true),$(__false))

function_pr_str = <$(if $(word 6,$(value $(1)_value)),$(wordlist 1,5,$(value $(1)_value))...,$(value $(1)_value))>

# Takes a function name and a list object of arguments and invokes
# the function with space separated arguments
_apply = $(call $(1),$($(2)_value))

# Takes a function object and a list object of arguments and invokes
# the function with space separated arguments
apply = $(call $(1)_value,$($(2)_value))

# Takes a space separated arguments and invokes the first argument
# (function object) using the remaining arguments.
sapply = $(call $(word 1,$(1))_value,$($(word 2,$(1))_value))

#
# hash maps (associative arrays)
#

# create a new anonymous empty hash map
_hash_map = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_hmap_$(hcode)$(eval $(__obj_magic)_hmap_$(hcode)_size := 0))
hash_map = $(word 1,$(foreach new_hmap,$(call _hash_map),$(new_hmap) $(if $(1),$(call _assoc_seq!,$(new_hmap),$(1)))))

_hash_map? = $(if $(filter $(__obj_magic)_hmap_%,$(1)),$(__true),)
hash_map? = $(if $(call _hash_map?,$(1)),$(__true),$(__false))

hash_map_pr_str = {$(foreach v,$(call __get_obj_values,$(1)),"$(foreach hcode,$(word 3,$(subst _, ,$(1))),$(patsubst $(1)_%,%,$(v:%_value=%)))" $(call _pr_str,$($(v)),$(2)))}

# Set multiple key/values in a map
_assoc_seq! = $(call _assoc!,$(1),$(call str_decode,$($(word 1,$(2))_value)),$(word 2,$(2)))$(if $(word 3,$(2)),$(call _assoc_seq!,$(1),$(wordlist 3,$(words $(2)),$(2))),)

# set a key/value in the hash map
_assoc! = $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),$(eval $(1)_size := $(call gmsl_plus,$($(1)_size),1)),)$(eval $(1)_$(k)_value := $(3))$(1))

# set a key/value in a copy of the hash map
# TODO: multiple arguments
assoc = $(foreach hm,$(call _clone_obj,$(word 1,$(1))),$(call _assoc!,$(hm),$(call str_decode,$($(word 2,$(1))_value)),$(word 3,$(1))))

# unset a key in the hash map
_dissoc! = $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$(eval $(1)_$(k)_value := $(__undefined))$(eval $(1)_size := $(call gmsl_subtract,$($(1)_size),1))))$(1)

# unset a key in a copy of the hash map
# TODO: this could be made more efficient by not copying the key in
# the first place
# TODO: multiple arguments
dissoc = $(foreach hm,$(call _clone_obj,$(word 1,$(1))),$(call _dissoc!,$(hm),$(call str_decode,$($(word 2,$(1))_value))))

keys = $(foreach new_list,$(call _list),$(new_list)$(eval $(new_list)_value := $(foreach v,$(call __get_obj_values,$(1)),$(call string,$(word 4,$(subst _, ,$(v)))))))

vals = $(foreach new_list,$(call _list),$(new_list)$(eval $(new_list)_value := $(foreach v,$(call __get_obj_values,$(1)),$($(v)))))



# Hash map and vector functions

# retrieve the value of a plain string key from the hash map, or
# retrive a vector by plain index
_get = $(strip \
        $(if $(call _hash_map?,$(1)),\
          $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$($(1)_$(k)_value))),\
          $(if $(call _vector?,$(1)),\
            $(word $(call gmsl_plus,1,$(2)),$($(1)_value)),\
            ,)))

# retrieve the value of a string key object from the hash map, or
# retrive a vector by number object index
get = $(strip \
        $(if $(call _hash_map?,$(word 1,$(1))),\
          $(call _get,$(word 1,$(1)),$(call str_decode,$($(word 2,$(1))_value))),\
          $(call _get,$(word 1,$(1)),$(call number_pr_str,$(word 2,$(1))))))

_contains? = $(strip \
               $(if $(call _hash_map?,$(1)),\
                 $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$(__true))),\
                 $(if $(call _vector?,$(1)),\
                   $(if $(word $(call gmsl_plus,1,$(2)),$($(1)_value)),$(__true),),\
                   ,)))
contains? = $(if $(call _contains?,$(word 1,$(1)),$(call str_decode,$($(word 2,$(1))_value))),$(__true),$(__false))


#
# Errors/Exceptions
#
_error = $(eval __ERROR := $(call string,$(1)))
throw = $(eval __ERROR := $(1))


#
# vectors
#

_vector = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_vect_$(hcode))
vector = $(word 1,$(foreach new_vect,$(call _vector),$(new_vect) $(eval $(new_vect)_value := $1)))

_vector? = $(if $(filter $(__obj_magic)_vect_%,$(1)),$(__true),)
vector? = $(if $(call _vector?,$(1)),$(__true),$(__false))

vector_pr_str = [$(foreach v,$(call __get_obj_values,$(1)),$(call _pr_str,$(v),$(2)))]

#
# list (same as vectors for now)
#

_list = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_list_$(hcode))
list = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $1)))

_list? = $(if $(filter $(__obj_magic)_list_%,$(1)),$(__true),)
list? = $(if $(call _list?,$(1)),$(__true),$(__false))

list_pr_str = ($(foreach v,$(call __get_obj_values,$(1)),$(call _pr_str,$(v),$(2))))

cons = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $(strip $(word 1,$(1)) $(call __get_obj_values,$(word 2,$(1)))))))


#
# atoms
#
atom = $(strip \
         $(foreach hcode,$(call __new_obj_hash_code),\
           $(foreach new_atom,$(__obj_magic)_atom_$(hcode),\
             $(new_atom)\
             $(eval $(new_atom)_value := $(1)))))

_atom? = $(if $(filter $(__obj_magic)_atom_%,$(1)),$(__true),)
atom? = $(if $(call _atom?,$(1)),$(__true),$(__false))

atom_pr_str = (atom $(call _pr_str,$($(1)_value),$(2)))

deref = $($(1)_value)

reset! = $(eval $(word 1,$(1))_value := $(word 2,$(1)))$(word 2,$(1))

swap! = $(foreach resp,$(call $(word 2,$(1))_value,$($(word 1,$(1))_value) $(wordlist 3,$(words $(1)),$(1))),\
          $(eval $(word 1,$(1))_value := $(resp))\
          $(resp))


#
# sequence operations
#

_sequential? = $(if $(filter $(__obj_magic)_list_% $(__obj_magic)_vect_%,$(1)),$(__true),)
sequential? = $(if $(call _sequential?,$(1)),$(__true),$(__false))

raw_flat = $(foreach val,$(call __get_obj_values,$(1)),$($(val)))

_nth = $(word $(call gmsl_plus,1,$(2)),$($(1)_value))

nth = $(word $(call gmsl_plus,1,$(call int_decode,$($(word 2,$(1))_value))),$($(word 1,$(1))_value))

empty? = $(if $(call _EQ,0,$(if $(call _hash_map?,$(1)),$($(1)_size),$(words $($(1)_value)))),$(__true),$(__false))

concat = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $(strip $(foreach lst,$1,$(call __get_obj_values,$(lst)))))))

conj = $(word 1,$(foreach new_list,$(call __new_obj_like,$(word 1,$(1))),$(new_list) $(eval $(new_list)_value := $(strip $($(word 1,$(1))_value) $(wordlist 2,$(words $(1)),$(1))))))

# conj that mutates a sequence in-place to append the call arguments
_conj! = $(eval $(1)_value := $(strip $($(1)_value) $2 $3 $4 $5 $6 $7 $8 $9 $(10) $(11) $(12) $(13) $(14) $(15) $(16) $(17) $(18) $(19) $(20)))$(1)

_count = $(strip \
           $(if $(call _hash_map?,$(1)),\
             $($(1)_size),\
             $(words $($(1)_value))))
count = $(call number,$(call _count,$(1)))

sfirst = $(word 1,$($(1)_value))

slast = $(word $(words $($(1)_value)),$($(1)_value))

# Creates a new vector/list of the everything after but the first
# element
srest = $(word 1,$(foreach new_list,$(call _list),$(new_list) $(eval $(new_list)_value := $(wordlist 2,$(words $($(1)_value)),$($(1)_value)))))

# maps a make function over a list object, using mutating _conj!
_smap = $(word 1,\
         $(foreach new_list,$(call _list),\
           $(new_list)\
           $(foreach v,$(call __get_obj_values,$(2)),\
             $(call _conj!,$(new_list),$(call $(1),$(v),$(3),$(4))))))

# Same as _smap but returns a vector
_smap_vec = $(word 1,\
         $(foreach new_vector,$(call vector),\
           $(new_vector)\
           $(foreach v,$(call __get_obj_values,$(2)),\
             $(call _conj!,$(new_vector),$(call $(1),$(v),$(3),$(4))))))

# Map a function object over a list object
smap = $(strip\
         $(foreach func,$(word 1,$(1)),\
	   $(foreach lst,$(word 2,$(1)),\
             $(foreach type,$(word 2,$(subst _, ,$(lst))),\
               $(foreach new_hcode,$(call __new_obj_hash_code),\
                 $(foreach sz,$(words $(call __get_obj_values,$(lst))),\
                   $(eval $(__obj_magic)_$(type)_$(new_hcode)_value := $(strip \
                     $(foreach val,$(call __get_obj_values,$(lst)),\
                       $(call $(func)_value,$(val))))))\
                 $(__obj_magic)_$(type)_$(new_hcode))))))



_equal? = $(strip \
            $(foreach ot1,$(call _obj_type,$(1)),$(foreach ot2,$(call _obj_type,$(2)),\
              $(if $(or $(call _EQ,$(ot1),$(ot2)),\
		        $(and $(call _sequential?,$(1)),$(call _sequential?,$(2)))),\
                $(if $(or $(call _string?,$(1)),$(call _symbol?,$(1)),$(call _number?,$(1))),\
                  $(call _EQ,$($(1)_value),$($(2)_value)),\
                $(if $(or $(call _vector?,$(1)),$(call _list?,$(1)),$(call _hash_map?,$(1))),\
                  $(if $(and $(call _EQ,$(call _count,$(1)),$(call _count,$(2))),\
                             $(call _EQ,$(call _count,$(1)),$(words $(call gmsl_pairmap,_equal?,$(call __get_obj_values,$(1)),$(call __get_obj_values,$(2)))))),$(__true),),\
                $(call _EQ,$(1),$(2))))))))

equal? = $(if $(call _equal?,$(word 1,$(1)),$(word 2,$(1))),$(__true),$(__false))

#
# ENV
#

# An ENV environment is a hash-map with an __outer__ reference to an
# outer environment
define BIND_ARGS
$(strip \
  $(word 1,$(1) \
    $(foreach fparam,$(call _nth,$(2),0),\
      $(if $(call _EQ,&,$($(fparam)_value)),
        $(call ENV_SET,$(1),$($(call _nth,$(2),1)_value),$(strip \
          $(foreach new_list,$(call _list),
            $(word 1,$(new_list) \
              $(foreach val,$(3),$(call _conj!,$(new_list),$(val))))))),\
        $(foreach val,$(word 1,$(3)),\
          $(call ENV_SET,$(1),$($(fparam)_value),$(val))\
          $(foreach left,$(call srest,$(2)),\
            $(if $(call _EQ,0,$(call _count,$(left))),\
              ,\
              $(call BIND_ARGS,$(1),$(left),$(wordlist 2,$(words $(3)),$(3))))))))))
endef

# Create a new ENV and optional bind values in it
#   $(1): outer environment (set as a key named __outer__)
#   $(2): list/vector object of bind forms
#   $(3): space separated list of expressions to bind
ENV = $(strip $(foreach new_env,$(call _assoc!,$(call _hash_map),__outer__,$(if $(1),$(1),$(__nil))),$(if $(2),$(call BIND_ARGS,$(new_env),$(2),$(3)),$(new_env))))
ENV_FIND = $(strip \
             $(if $(call _contains?,$(1),$(subst =,$(__equal),$(2))),\
               $(1),\
               $(if $(call _EQ,$(__nil),$(call _get,$(1),__outer__)),\
                 ,\
                 $(call ENV_FIND,$(call _get,$(1),__outer__),$(2)))))

ENV_GET = $(foreach env,|$(call ENV_FIND,$(1),$(2))|,$(if $(call _EQ,||,$(env)),$(call _error,'$(2)' not found),$(call _get,$(strip $(subst |,,$(env))),$(subst =,$(__equal),$(2)))))

ENV_SET = $(if $(call _assoc!,$(1),$(subst =,$(__equal),$(2)),$(3)),$(1),)

#
# Visualize Objects in memory
#

__var_name = $(word 2,$(subst _, ,$(1)))_$(word 3,$(subst _, ,$(1)))
__var_idx := 0
__var_print = $(foreach v,$(1),\
                $(foreach var,$(call __var_name,$(v)),\
                  $(if $(or $(call _list?,$(v)),$(call _vector?,$(v))),\
                    $(info $(2)$(var):)\
                    $(eval __var_idx := $(call gmsl_plus,1,$(__var_idx)))\
                    $(foreach lidx,__lidx_$(__var_idx),\
                      $(eval $(lidx) := 0)\
                      $(foreach val,$($(v)_value),\
                        $(call __var_print,$(val),$(2)$(SPACE)$(SPACE)$($(lidx)): )\
                        $(eval $(lidx) := $(call gmsl_plus,1,$($(lidx)))))),\
                  $(if $(call _hash_map?,$(v)),\
                    $(info $(2)$(var):)\
                    $(foreach vkey,$(filter $(v)_%,$(.VARIABLES)),\
                      $(foreach key,$(word 4,$(subst _, ,$(vkey))),\
                        $(info $(2)$(SPACE)$(SPACE)$(subst $(__equal),=,$(key)): )\
                        $(call __var_print,$($(vkey)),$(2)$(SPACE)$(SPACE)$(SPACE)$(SPACE)))),\
                  $(if $(call _symbol?,$(v)),\
                    $(info $(2)$(var): $($(v)_value)),\
                  $(if $(call _number?,$(v)),\
                    $(info $(2)$(var): $(call int_decode,$($(v)_value))),\
                  $(if $(call _nil?,$(v)),\
                    $(info $(2)nil),\
                  $(if $(call _function?,$(v)),\
                    $(if $(word 6,$(value $(v)_value)),\
                      $(info $(2)$(var): $(wordlist 1,5,$(value $(v)_value))...),\
                      $(info $(2)$(var): $(value $(v)_value))),\
                  $(info $(2)$(var): ...)))))))))


visualize_memory = $(foreach var,$(sort $(foreach vv,$(filter $(__obj_magic)_%,$(.VARIABLES)),$(call __var_name,$(vv)))),$(call __var_print,$(__obj_magic)_$(var)))

#
# Namespace of type functions
#
types_ns = pr-str pr_str str str prn prn println println \
           with-meta with_meta meta meta \
           type obj_type = equal? \
           nil? nil? true? true? false? false? \
           number? number? \
           > number_gt >= number_gte < number_lt <= number_lte \
           + number_plus - number_subtract * number_multiply / number_divide \
           symbol? symbol? function? function? \
           string? string? subs subs \
           hash-map hash_map map? hash_map? assoc assoc dissoc dissoc \
           get get contains? contains? keys keys vals vals \
           throw throw \
           list list list? list? \
           vector vector vector? vector? \
           atom atom atom? atom? deref deref reset! reset! swap! swap! \
           sequential? sequential? \
           cons cons nth nth empty? empty? count count concat concat \
           conj conj first sfirst last slast rest srest \
           apply sapply map smap \

endif

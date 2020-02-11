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
__obj_magic = ⍄⁊
# \u2256
__equal = ≛
__keyword = ʞ
__obj_hash_code = 0

__new_obj_hash_code = $(eval __obj_hash_code := $(call int_add,1,$(__obj_hash_code)))$(__obj_hash_code)

__new_obj = $(__obj_magic)_$(1)_$(call __new_obj_hash_code)

__new_obj_like = $(foreach type,$(word 2,$(subst _, ,$(1))),$(__obj_magic)_$(type)_$(__new_obj_hash_code))

__get_obj_values = $(strip \
                     $(if $(filter $(__obj_magic)_hmap_%,$(1)),\
                       $(sort $(foreach v,$(filter %_value,$(filter $(1)_%,$(.VARIABLES))),$(if $(call _undefined?,$(v)),,$(v)))),\
                       $($(1)_value)))


# Visualize Objects in memory
__var_name = $(word 2,$(subst _, ,$(1)))_$(word 3,$(subst _, ,$(1)))
__var_idx := 0
__var_print = $(foreach v,$(1),\
                $(foreach var,$(call __var_name,$(v)),\
                  $(if $(or $(call _list?,$(v)),$(call _vector?,$(v))),\
                    $(info $(2)$(var):)\
                    $(eval __var_idx := $(call int_add,1,$(__var_idx)))\
                    $(foreach lidx,__lidx_$(__var_idx),\
                      $(eval $(lidx) := 0)\
                      $(foreach val,$($(v)_value),\
                        $(call __var_print,$(val),$(2)$(SPACE)$(SPACE)$($(lidx)): )\
                        $(eval $(lidx) := $(call int_add,1,$($(lidx)))))),\
                  $(if $(call _hash_map?,$(v)),\
                    $(info $(2)$(var):)\
                    $(foreach vkey,$(filter $(v)_%,$(.VARIABLES)),\
                      $(foreach key,$(word 4,$(subst _, ,$(vkey))),\
                        $(info $(2)$(SPACE)$(SPACE)$(subst $(__equal),=,$(key)): )\
                        $(call __var_print,$($(vkey)),$(2)$(SPACE)$(SPACE)$(SPACE)$(SPACE)))),\
                  $(if $(call _symbol?,$(v)),\
                    $(info $(2)$(var): $($(v)_value)),\
                  $(if $(call _keyword?,$(v)),\
                    $(info $(2)$(var): $($(v)_value)),\
                  $(if $(call _number?,$(v)),\
                    $(info $(2)$(var): $(call int_decode,$($(v)_value))),\
                  $(if $(call _nil?,$(v)),\
                    $(info $(2)nil),\
                  $(if $(call _function?,$(v)),\
                    $(if $(word 6,$(value $(v)_value)),\
                      $(info $(2)$(var): $(wordlist 1,5,$(value $(v)_value))...),\
                      $(info $(2)$(var): $(value $(v)_value))),\
                  $(info $(2)$(var): ...))))))))))

_visualize_memory = $(foreach var,$(sort $(foreach vv,$(filter $(__obj_magic)_%,$(.VARIABLES)),$(call __var_name,$(vv)))),$(call __var_print,$(__obj_magic)_$(var)))


# Errors/Exceptions
__ERROR :=
_error = $(strip $(eval __ERROR := $(call _string,$(1))))


# Constant atomic values
__undefined = $(__obj_magic)_undf_0
__nil = $(__obj_magic)__nil_0
__true = $(__obj_magic)_true_0
__false = $(__obj_magic)_fals_0


# General functions

# Return the type of the object (or "make" if it's not a object
_obj_type = $(strip \
              $(if $(filter $(__obj_magic)_symb_%,$(1)),symbol,\
              $(if $(filter $(__obj_magic)_list_%,$(1)),list,\
              $(if $(filter $(__obj_magic)_numb_%,$(1)),number,\
              $(if $(filter $(__obj_magic)_func_%,$(1)),function,\
              $(if $(filter $(__obj_magic)_strn_%,$(1)),\
                $(if $(filter $(__keyword)%,$($(1)_value)),keyword,string),\
              $(if $(filter $(__obj_magic)__nil_%,$(1)),nil,\
              $(if $(filter $(__obj_magic)_true_%,$(1)),true,\
              $(if $(filter $(__obj_magic)_fals_%,$(1)),false,\
              $(if $(filter $(__obj_magic)_vect_%,$(1)),vector,\
              $(if $(filter $(__obj_magic)_atom_%,$(1)),atom,\
              $(if $(filter $(__obj_magic)_hmap_%,$(1)),hash_map,\
              $(if $(filter $(__obj_magic)_undf_%,$(1)),undefined,\
              make)))))))))))))

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

_hash_equal? = $(strip \
                 $(if $(and $(call _EQ,$(foreach v,$(call __get_obj_values,$(1)),$(word 4,$(subst _, ,$(v)))),$(foreach v,$(call __get_obj_values,$(2)),$(word 4,$(subst _, ,$(v))))),\
                            $(call _EQ,$(call _count,$(1)),$(words $(call gmsl_pairmap,_equal?,$(foreach v,$(call __get_obj_values,$(1)),$($(v))),$(foreach v,$(call __get_obj_values,$(2)),$($(v))))))),\
                      $(__true),))

_equal? = $(strip \
            $(foreach ot1,$(call _obj_type,$(1)),$(foreach ot2,$(call _obj_type,$(2)),\
              $(if $(or $(call _EQ,$(ot1),$(ot2)),\
                        $(and $(call _sequential?,$(1)),$(call _sequential?,$(2)))),\
                $(if $(or $(call _string?,$(1)),$(call _symbol?,$(1)),$(call _keyword?,$(1)),$(call _number?,$(1))),\
                  $(call _EQ,$($(1)_value),$($(2)_value)),\
                $(if $(call _hash_map?,$(1)),\
                  $(call _hash_equal?,$(1),$(2)),\
                $(if $(or $(call _vector?,$(1)),$(call _list?,$(1))),\
		  $(if $(and $(call _EQ,$(call _count,$(1)),$(call _count,$(2))),\
                             $(call _EQ,$(call _count,$(1)),$(words $(call gmsl_pairmap,_equal?,$(call __get_obj_values,$(1)),$(call __get_obj_values,$(2)))))),\
                    $(__true),),\
                $(call _EQ,$(1),$(2)))))))))

_undefined? = $(or $(call _EQ,undefined,$(origin $(1))),$(filter $(__undefined),$($(1))))

_nil? = $(if $(filter $(__obj_magic)__nil_%,$(1)),$(__true),)

_true? = $(if $(filter $(__obj_magic)_true_%,$(1)),$(__true),)

_false? = $(if $(filter $(__obj_magic)_fals_%,$(1)),$(__true),)


# Symbols
_symbol = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_symb_$(hcode)$(eval $(__obj_magic)_symb_$(hcode)_value := $(1)))
_symbol? = $(if $(filter $(__obj_magic)_symb_%,$(1)),$(__true),)


# Keywords
_keyword = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_strn_$(hcode)$(eval $(__obj_magic)_strn_$(hcode)_value := $(__keyword)$(1)))
_keyword? = $(if $(filter $(__obj_magic)_strn_%,$(1)),$(if $(filter $(__keyword)%,$($(1)_value)),$(__true),))


# Numbers
_pnumber = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_numb_$(hcode)$(eval $(__obj_magic)_numb_$(hcode)_value := $(1)))
_number = $(call _pnumber,$(call int_encode,$(1)))
_number? = $(if $(filter $(__obj_magic)_numb_%,$(1)),$(__true),)


# Strings
__string = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_strn_$(hcode)$(eval $(__obj_magic)_strn_$(hcode)_value := $(1)))
_string = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_strn_$(hcode)$(eval $(__obj_magic)_strn_$(hcode)_value := $(call str_encode,$(1))))
_string? = $(if $(filter $(__obj_magic)_strn_%,$(1)),$(__true),)

# Functions

# Return a function object. The first parameter is the
# function/macro 'source'. Note that any $ must be escaped as $$ to be
# preserved and become positional arguments for when the
# function/macro is later invoked.
_function = $(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_func_$(hcode)$(eval $(__obj_magic)_func_$(hcode)_value = $(1)))
_function? = $(if $(filter $(__obj_magic)_func_%,$(1)),$(__true),)

# Takes a function name and a list object of arguments and invokes
# the function with space separated arguments
_apply = $(call $(1),$($(2)_value))

# Takes a function object and a list object of arguments and invokes
# the function with space separated arguments
apply = $(call $(1)_value,$($(2)_value))


# Lists
_list = $(word 1,$(foreach new_list,$(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_list_$(hcode)),$(new_list) $(eval $(new_list)_value := $1)))
_list? = $(if $(filter $(__obj_magic)_list_%,$(1)),$(__true),)


# Vectors (same as lists for now)
_vector = $(word 1,$(foreach new_vect,$(foreach hcode,$(call __new_obj_hash_code),$(__obj_magic)_vect_$(hcode)),$(new_vect) $(eval $(new_vect)_value := $1)))
_vector? = $(if $(filter $(__obj_magic)_vect_%,$(1)),$(__true),)


# Hash maps (associative arrays)
_hash_map = $(word 1,$(foreach hcode,$(call __new_obj_hash_code),$(foreach new_hmap,$(__obj_magic)_hmap_$(hcode),$(new_hmap) $(eval $(new_hmap)_size := 0) $(if $(1),$(call _assoc_seq!,$(new_hmap),$(1))))))
_hash_map? = $(if $(filter $(__obj_magic)_hmap_%,$(1)),$(__true),)

# Set multiple key/values in a map
_assoc_seq! = $(call _assoc!,$(1),$(call str_decode,$($(word 1,$(2))_value)),$(word 2,$(2)))$(if $(word 3,$(2)),$(call _assoc_seq!,$(1),$(wordlist 3,$(words $(2)),$(2))),)

_dissoc_seq! = $(foreach key,$(2),\
                   $(call _dissoc!,$(1),$(call str_decode,$($(key)_value))))

# set a key/value in the hash map
_assoc! = $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),$(eval $(1)_size := $(call int_add,$($(1)_size),1)),)$(eval $(1)_$(k)_value := $(3))$(1))

# unset a key in the hash map
_dissoc! = $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$(eval $(1)_$(k)_value := $(__undefined))$(eval $(1)_size := $(call int_sub,$($(1)_size),1))))$(1)

# Hash map and vector functions

# retrieve the value of a plain string key from the hash map, or
# retrive a vector by plain index
_get = $(strip \
        $(if $(call _hash_map?,$(1)),\
          $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$($(1)_$(k)_value))),\
          $(if $(call _vector?,$(1)),\
            $(word $(call int_add,1,$(2)),$($(1)_value)),\
            ,)))

_contains? = $(strip \
               $(if $(call _hash_map?,$(1)),\
                 $(foreach k,$(subst =,$(__equal),$(2)),$(if $(call _undefined?,$(1)_$(k)_value),,$(__true))),\
                 $(if $(call _vector?,$(1)),\
                   $(if $(word $(call int_add,1,$(2)),$($(1)_value)),$(__true),),\
                   ,)))


# sequence operations

_sequential? = $(if $(filter $(__obj_magic)_list_% $(__obj_magic)_vect_%,$(1)),$(__true),)

_nth = $(word $(call int_add,1,$(2)),$($(1)_value))

# conj that mutates a sequence in-place to append the call arguments
_conj! = $(eval $(1)_value := $(strip $($(1)_value) $2 $3 $4 $5 $6 $7 $8 $9 $(10) $(11) $(12) $(13) $(14) $(15) $(16) $(17) $(18) $(19) $(20)))$(1)

_count = $(strip \
           $(if $(call _hash_map?,$(1)),\
             $($(1)_size),\
             $(words $($(1)_value))))

# Creates a new vector/list of the everything after but the first
# element
srest = $(word 1,$(foreach new_list,$(call _list),\
                   $(new_list) \
                   $(eval $(new_list)_value := $(wordlist 2,$(words $($(1)_value)),$($(1)_value)))))

# maps a make function over a list object, using mutating _conj!
_smap = $(word 1,\
         $(foreach new_list,$(call _list),\
           $(new_list)\
           $(foreach v,$(call __get_obj_values,$(2)),\
             $(call _conj!,$(new_list),$(call $(1),$(v),$(3),$(4))))))

# Same as _smap but returns a vector
_smap_vec = $(word 1,\
         $(foreach new_vector,$(call _vector),\
           $(new_vector)\
           $(foreach v,$(call __get_obj_values,$(2)),\
             $(call _conj!,$(new_vector),$(call $(1),$(v),$(3),$(4))))))


# atoms

_atom? = $(if $(filter $(__obj_magic)_atom_%,$(1)),$(__true),)


endif

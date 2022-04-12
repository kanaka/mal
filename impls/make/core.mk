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


# General functions

$(encoded_equal) = $(if $(call _equal?,$(firstword $1),$(lastword $1)),$(__true),$(__false))


# Scalar functions
nil? = $(if $(_nil?),$(__true),$(__false))
true? = $(if $(_true?),$(__true),$(__false))
false? = $(if $(_false?),$(__true),$(__false))


# Symbol functions
symbol = $(call _symbol,$(_string_val))
symbol? = $(if $(_symbol?),$(__true),$(__false))

# Keyword functions
keyword = $(if $(_keyword?),$1,$(call _keyword,$(_string_val)))
keyword? = $(if $(_keyword?),$(__true),$(__false))


# Number functions
number? = $(if $(_number?),$(__true),$(__false))

define <
$(if $(call int_lt,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1)))\
  ,$(__true),$(__false))
endef
define <$(encoded_equal)
$(if $(call int_lte,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1)))\
  ,$(__true),$(__false))
endef
define >
$(if $(call int_gt,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1)))\
  ,$(__true),$(__false))
endef
define >$(encoded_equal)
$(if $(call int_gte,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1)))\
  ,$(__true),$(__false))
endef

+ = $(call _number,$(call int_add,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1))))
- = $(call _number,$(call int_sub,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1))))
* = $(call _number,$(call int_mult,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1))))
/ = $(call _number,$(call int_div,$(call _number_val,$(firstword $1)),$(call _number_val,$(lastword $1))))

time-ms = $(call _number,$(shell date +%s%3N))

# String functions

string? = $(if $(_string?),$(__true),$(__false))

pr-str  = $(call _string,$(call _pr_str_mult,$1,yes,$(_SP)))
str     = $(call _string,$(_pr_str_mult))
prn     = $(__nil)$(call print,$(call _pr_str_mult,$1,yes,$(_SP)))
println = $(__nil)$(call print,$(call _pr_str_mult,$1,,$(_SP)))

readline = $(or $(foreach res,$(call READLINE,$(_string_val))\
                  ,$(call _string,$(res:ok=)))\
                ,$(__nil))
read-string = $(call READ_STR,$(_string_val))
slurp = $(call _string,$(call _read_file,$(_string_val)))



# Function functions
fn? = $(if $(_fn?),$(__true),$(__false))
macro? = $(if $(_macro?),$(__true),$(__false))


# List functions
list? = $(if $(_list?),$(__true),$(__false))


# Vector functions
vector? = $(if $(_vector?),$(__true),$(__false))

vec = $(if $(_list?)\
  ,$(call vector,$(_seq_vals))$(rem \
),$(if $(_vector?)\
  ,$1$(rem \
),$(call _error,vec$(encoded_colon)$(_SP)called$(_SP)on$(_SP)non-sequence)))


# Hash map (associative array) functions
hash-map = $(call _map_new,,$1)
map? = $(if $(_hash_map?),$(__true),$(__false))

# set a key/value in a copy of the hash map
assoc = $(call _map_new,$(firstword $1),$(_rest))

# unset keys in a copy of the hash map
dissoc = $(call _map_new,$(firstword $1),,$(_rest))

keys = $(call list,$(_keys))

vals = $(call list,$(foreach k,$(_keys),$(call _get,$1,$k)))

# retrieve the value of a string key object from the hash map, or
# return nil if the key is not found.
get = $(or $(call _get,$(firstword $1),$(lastword $1)),$(__nil))

contains? = $(if $(call _get,$(firstword $1),$(lastword $1)),$(__true),$(__false))


# sequence operations

sequential? = $(if $(_sequential?),$(__true),$(__false))

# Strip in case seq_vals is empty.
cons = $(call list,$(strip $(firstword $1) $(call _seq_vals,$(lastword $1))))

# Strip in case foreach introduces a space after an empty argument.
concat = $(call list,$(strip $(foreach l,$1,$(call _seq_vals,$l))))

nth = $(or $(word $(call int_add,1,$(call _number_val,$(lastword $1))),\
                  $(call _seq_vals,$(firstword $1)))\
          ,$(call _error,nth: index out of range))

first = $(or $(if $(_sequential?),$(firstword $(_seq_vals))),$(__nil))

empty? = $(if $(_seq_vals),$(__false),$(__true))

count = $(call _number,$(words $(if $(_sequential?),$(_seq_vals))))

# Creates a new vector/list of the everything after but the first
# element
rest = $(call list,$(if $(_sequential?),$(call _rest,$(_seq_vals))))

# Takes a space separated arguments and invokes the first argument
# (function object) using the remaining arguments.
# Strip in case wordlist or _seq_vals is empty.
apply = $(call _apply,$(firstword $1),$(strip \
  $(wordlist 2,$(call int_sub,$(words $1),1),$1) \
  $(call _seq_vals,$(lastword $1))))

# Map a function object over a list object
map = $(call list,$(foreach e,$(call _seq_vals,$(lastword $1))\
  ,$(call _apply,$(firstword $1),$e)))

conj = $(foreach seq,$(firstword $1)\
  ,$(call conj_$(call _obj_type,$(seq)),$(call _seq_vals,$(seq)),$(_rest)))
# Strip in case $1 or $2 is empty.
# Also, _reverse introduces blanks.
conj_vector = $(call vector,$(strip $1 $2))
conj_list   = $(call list,$(strip $(call _reverse,$2) $1))

seq = $(or $(seq_$(_obj_type))\
          ,$(call _error,seq: called on non-sequence))
seq_list = $(if $(_seq_vals),$1,$(__nil))
seq_vector = $(if $(_seq_vals),$(call list,$(_seq_vals)),$(__nil))
seq_nil = $1
seq_string = $(if $(_string_val)\
               ,$(call list,$(foreach c,$(call str_encode,$(_string_val))\
                 ,$(call _string,$(call str_decode,$c))))$(rem \
               ),$(__nil))

# Metadata functions

# are implemented in types.mk.


# Atom functions

atom? = $(if $(_atom?),$(__true),$(__false))

reset! = $(foreach v,$(lastword $1),$(call _reset,$(firstword $1),$v)$v)

swap! = $(foreach a,$(firstword $1)\
  ,$(call reset!,$a $(call _apply,$(word 2,$1),$(call deref,$a) $(_rest2))))




# Namespace of core functions

core_ns := $(encoded_equal) throw nil?  true?  false?  string?  symbol \
  symbol?  keyword keyword?  number?  fn?  macro? \
  pr-str str prn println readline read-string slurp \ < \
  <$(encoded_equal) > >$(encoded_equal) + - * / time-ms \
  list list?  vector vector?  hash-map map?  assoc dissoc get \
  contains? keys vals \
  sequential? cons concat vec nth first rest empty? count apply map \
  conj seq \
  with-meta meta atom atom? deref reset! swap!

endif

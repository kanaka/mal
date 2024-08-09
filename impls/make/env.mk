#
# mal (Make Lisp) Object Types and Functions
#

ifndef __mal_env_included
__mal_env_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)types.mk

#
# ENV
#

# An ENV environment is a hash-map with an __outer__ reference to an
# outer environment

# Keys are stored as Make variables named $(env)_$(key).  The outer
# environment is the content of the variable itself.

# 1: outer environment, or ""  ->    new environment
ENV = $(call __new_obj,env,$1)

# 1:env  2:key  ->  value or ""
ENV_GET = $(if $1,$(or $($1_$2),$(call ENV_GET,$($1),$2)))

# 1:env  2:key  3:value
ENV_SET = $(eval $1_$2 := $3)

# 1:env  ->  (encoded) keys
env_keys = $(foreach k,$(patsubst $1_%,%,$(filter $1_%,$(.VARIABLES)))\
             ,$(call _symbol_val,$k))

endif

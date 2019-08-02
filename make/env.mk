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

ENV_GET = $(foreach env,|$(call ENV_FIND,$(1),$(2))|,$(if $(call _EQ,||,$(env)),$(call _error,'$(2)' not found)$(__nil),$(call _get,$(strip $(subst |,,$(env))),$(subst =,$(__equal),$(2)))))

ENV_SET = $(if $(call _assoc!,$(1),$(subst =,$(__equal),$(2)),$(3)),$(1),)

endif

#
# mal (Make a Lisp) printer
#

ifndef __mal_printer_included
__mal_printer_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk

# return a printable form of the argument, the second parameter is
# 'print_readably' which backslashes quotes in string values
_pr_str = $(if $(1),$(foreach ot,$(call _obj_type,$(1)),$(if $(call _EQ,make,$(ot)),$(call _error,_pr_str failed on $(1)),$(call $(ot)_pr_str,$(1),$(2)))),)

# Like _pr_str but takes multiple values in first argument, the second
# parameter is 'print_readably' which backslashes quotes in string
# values, the third parameter is the delimeter to use between each
# _pr_str'd value
_pr_str_mult = $(call _pr_str,$(word 1,$(1)),$(2))$(if $(word 2,$(1)),$(3)$(call _pr_str_mult,$(wordlist 2,$(words $(1)),$(1)),$(2),$(3)),)


# Type specific printing

nil_pr_str = nil
true_pr_str = true
false_pr_str = false

number_pr_str = $(call int_decode,$($(1)_value))

symbol_pr_str = $($(1)_value)

keyword_pr_str = $(COLON)$(patsubst $(__keyword)%,%,$(call str_decode,$($(1)_value)))

string_pr_str = $(if $(filter $(__keyword)%,$(call str_decode,$($(1)_value))),$(COLON)$(patsubst $(__keyword)%,%,$(call str_decode,$($(1)_value))),$(if $(2),"$(subst $(DQUOTE),$(ESC_DQUOTE),$(subst $(SLASH),$(SLASH)$(SLASH),$(call str_decode,$($(1)_value))))",$(call str_decode,$($(1)_value))))

function_pr_str = <$(if $(word 6,$(value $(1)_value)),$(wordlist 1,5,$(value $(1)_value))...,$(value $(1)_value))>

list_pr_str = ($(foreach v,$(call __get_obj_values,$(1)),$(call _pr_str,$(v),$(2))))

vector_pr_str = [$(foreach v,$(call __get_obj_values,$(1)),$(call _pr_str,$(v),$(2)))]

hash_map_pr_str = {$(foreach v,$(call __get_obj_values,$(1)),$(foreach vval,$(foreach hcode,$(word 3,$(subst _, ,$(1))),$(patsubst $(1)_%,%,$(v:%_value=%))),$(if $(filter $(__keyword)%,$(vval)),$(patsubst $(__keyword)%,$(COLON)%,$(vval)),"$(vval)")) $(call _pr_str,$($(v)),$(2)))}

atom_pr_str = (atom $(call _pr_str,$($(1)_value),$(2)))

endif

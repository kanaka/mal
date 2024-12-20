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
_pr_str = $(call $(_obj_type)_pr_str,$1,$2)

# Like _pr_str but takes multiple values in first argument, the second
# parameter is 'print_readably' which backslashes quotes in string
# values, the third parameter is the delimeter to use between each
# _pr_str'd value
_pr_str_mult = $(subst $(SPACE),$3,$(foreach f,$1,$(call _pr_str,$f,$2)))


# Type specific printing

nil_pr_str := nil
true_pr_str := true
false_pr_str := false

number_pr_str = $(_number_val)

symbol_pr_str = $(_symbol_val)

keyword_pr_str = $(encoded_colon)$(_keyword_val)

string_pr_str = $(if $2\
  ,"$(subst $(_NL),$(encoded_slash)n,$(rem \
   )$(subst ",$(encoded_slash)",$(rem \
   )$(subst $(encoded_slash),$(encoded_slash)$(encoded_slash),$(rem \
   )$(_string_val))))"$(rem \
else \
  ),$(_string_val))

corefn_pr_str := <Core>
function_pr_str := <Function>
macro_pr_str := <Macro>

list_pr_str = $(_LP)$(call _pr_str_mult,$(_seq_vals),$2,$(_SP))$(_RP)

vector_pr_str = [$(call _pr_str_mult,$(_seq_vals),$2,$(_SP))]

map_pr_str = {$(call _pr_str_mult,$(foreach k,$(_keys),$k $(call _get,$1,$k)),$2,$(_SP))}

atom_pr_str = $(_LP)atom$(_SP)$(call _pr_str,$(deref),$2)$(_RP)

endif

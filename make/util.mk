#
# mal (Make Lisp) utility functions/definitions
#

ifndef __mal_util_included
__mal_util_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)gmsl.mk

SEMI := ;
COMMA := ,
COLON := :
LCURLY := {
RCURLY := }
LPAREN := (
RPAREN := )
LBRACKET := [
RBRACKET := ]
DQUOTE := "# "
SLASH := $(strip \ )
ESC_DQUOTE := $(SLASH)$(DQUOTE)
ESC_N := $(SLASH)n
SQUOTE := '# '
QQUOTE := `# `
SPACE := 
SPACE += 
NUMBERS := 0 1 2 3 4 5 6 7 8 9
UNQUOTE := ~
SPLICE_UNQUOTE := ~@
define NEWLINE


endef
CARET := ^
ATSIGN := @

# \u00ab
_LP := «
# \u00bb
_RP := »
# \u00ed
_LC := í
# \u00ec
_RC := ì
## \u00a7
_SP := §
## \u00ae
_SUQ := ®
## \u015e
_DOL := Ş
## \u00b6
_NL := ¶
## \u00a8
###_EDQ := ¨


#
# Utility functions
#

_EQ = $(if $(subst x$1,,x$2)$(subst x$2,,x$1),,true)

_NOT = $(if $1,,true)

# take a list of words and join them with a separator
# params: words, seperator, result
_join = $(strip \
          $(if $(strip $(1)),\
            $(if $(strip $(3)),\
              $(call _join,$(wordlist 2,$(words $(1)),$(1)),$(2),$(3)$(2)$(word 1,$(1))),\
              $(call _join,$(wordlist 2,$(words $(1)),$(1)),$(2),$(word 1,$(1)))),\
            $(3)))

#$(info _join(1 2 3 4): [$(call _join,1 2 3 4)])
#$(info _join(1 2 3 4,X): [$(call _join,1 2 3 4, )])
#$(info _join(1): [$(call _join,1)])
#$(info _join(): [$(call _join,)])

# reverse list of words
_reverse = $(if $(1),$(call _reverse,$(wordlist 2,$(words $(1)),$(1)))) $(firstword $(1))

#$(info reverse(1 2 3 4 5): $(call reverse,1 2 3 4 5))

# str_encode: take a string and return an encoded version of it with
# every character separated by a space and special characters replaced
# with special Unicode characters
str_encode = $(strip $(eval __temp := $$(subst $$$$,$(_DOL) ,$$(subst $(SPLICE_UNQUOTE),$(_SUQ) ,$$(subst $$(LPAREN),$$(_LP) ,$$(subst $$(RPAREN),$$(_RP) ,$$(subst $$(LCURLY),$$(_LC) ,$$(subst $$(RCURLY),$$(_RC) ,$$(subst $$(NEWLINE),$$(_NL) ,$$(subst $$(SPACE),$(_SP) ,$$1)))))))))$(foreach a,$(gmsl_characters),$(eval __temp := $$(subst $$a,$$a$$(SPACE),$(__temp))))$(__temp))

# str_decode: take an encoded string an return an unencoded version of
# it by replacing the special Unicode charactes with the real
# characters and with all characters joined into a regular string
str_decode = $(subst $(_SP),$(SPACE),$(subst $(_NL),$(NEWLINE),$(subst $(_LC),$(LCURLY),$(subst $(_RC),$(RCURLY),$(subst $(_LP),$(LPAREN),$(subst $(_RP),$(RPAREN),$(subst $(_SUQ),$(SPLICE_UNQUOTE),$(subst $(_DOL),$$,$(strip $(call _join,$(1)))))))))))

# Read a whole file substituting newlines with $(_NL)
_read_file = $(subst $(_NL),$(NEWLINE),$(shell out=""; while read -r l; do out="$${out}$${l}$(_NL)"; done < $(1); echo "$$out"))

endif

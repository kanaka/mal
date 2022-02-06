#
# mal (Make Lisp) utility functions/definitions
#

ifndef __mal_util_included
__mal_util_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)gmsl.mk

encoded_equal := Ξ
encoded_colon := κ
encoded_slash := λ
raw_hash      := \#
encoded_hash  := η

COMMA := ,
COLON := :
LPAREN := (
RPAREN := )
SLASH := $(strip \ )
SPACE :=
SPACE := $(SPACE) $(SPACE)
define NEWLINE


endef

# \u00ab
_LP := «
# \u00bb
_RP := »
## \u00a7
_SP := §
## \u00ae
_DOL := Ş
## \u00b6
_NL := ¶


#
# Utility functions
#

_EQ = $(if $(subst x$1,,x$2)$(subst x$2,,x$1),,true)

# reverse list of words
_reverse = $(if $1,$(call _reverse,$(_rest)) $(firstword $1))


#$(info reverse(1 2 3 4 5): $(call reverse,1 2 3 4 5))

# str_encode: take a string and return an encoded version of it with
# every character separated by a space and special characters replaced
# with special Unicode characters
define str_encode
$(eval __temp := $1)$(rem \
)$(foreach a,$(encoded_slash) $(_DOL) $(_LP) $(_RP) $(_NL) \
  $(encoded_hash) $(encoded_colon) $(_SP) $(encoded_equal) $(gmsl_characters)\
  ,$(eval __temp := $$(subst $$a,$$a$$(SPACE),$(__temp))))$(rem \
)$(__temp)
endef

# str_decode: take an encoded string an return an unencoded version of
# it by replacing the special Unicode charactes with the real
# characters and with all characters joined into a regular string
str_decode = $(subst $(SPACE),,$1)

define str_encode_nospace
$(subst $(SLASH),$(encoded_slash),$(rem \
)$(subst $$,$(_DOL),$(rem \
)$(subst $(LPAREN),$(_LP),$(rem \
)$(subst $(RPAREN),$(_RP),$(rem \
)$(subst $(NEWLINE),$(_NL),$(rem \
)$(subst $(raw_hash),$(encoded_hash),$(rem \
)$(subst $(COLON),$(encoded_colon),$(rem \
)$(subst $(SPACE),$(_SP),$(rem \
)$(subst =,$(encoded_equal),$(rem \
)$1)))))))))
endef

define str_decode_nospace
$(subst $(encoded_slash),$(SLASH),$(rem \
)$(subst $(_DOL),$$,$(rem \
)$(subst $(_LP),$(LPAREN),$(rem \
)$(subst $(_RP),$(RPAREN),$(rem \
)$(subst $(_NL),$(NEWLINE),$(rem \
)$(subst $(encoded_hash),$(raw_hash),$(rem \
)$(subst $(encoded_colon),$(COLON),$(rem \
)$(subst $(_SP),$(SPACE),$(rem \
)$(subst $(encoded_equal),=,$1)))))))))
endef

# Read a whole file substituting newlines with $(_NL)
_read_file = $(call str_encode_nospace,$(shell \
  sed -z 's/\n/$(_NL)/g' '$(str_decode_nospace)'))

print = $(info $(str_decode_nospace))

_rest = $(wordlist 2,$(words $1),$1)
_rest2 = $(wordlist 3,$(words $1),$1)

# Evaluate $2 repeatedly with $k and $v set to key/value pairs from $1.
define _foreach2
$(foreach k,$(firstword $1)\
  ,$(foreach v,$(word 2,$1)\
    ,$(eval $2)$(call _foreach2,$(_rest2),$2)))
endef

endif

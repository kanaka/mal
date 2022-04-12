#
# mal (Make Lisp) Parser/Reader
#

ifndef __mal_reader_included
__mal_reader_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk

READER_DEBUG ?=

_TOKEN_DELIMS := ; , " ` $(_SP) $(_NL) { } $(_LP) $(_RP) [ ] #`"

reader_init = $(eval __reader_temp := $(str_encode))
reader_next = $(firstword $(__reader_temp))
reader_drop = $(eval __reader_temp := $(call _rest,$(__reader_temp)))
reader_log = $(if $(READER_DEBUG),$(info READER: $1 from $(__reader_temp)))

define READ_NUMBER
$(call reader_log,number)$(rem \
)$(if $(filter 0 1 2 3 4 5 6 7 8 9,$(reader_next))\
  ,$(reader_next)$(reader_drop)$(call READ_NUMBER))
endef

define READ_STRING
$(call reader_log,string)$(rem \
)$(if $(filter ",$(reader_next))\
  ,$(reader_drop)$(rem "\
),$(if $(filter $(encoded_slash),$(reader_next))\
  ,$(reader_drop)$(rem \
  )$(if $(filter n,$(reader_next)),$(_NL),$(reader_next))$(rem \
  )$(reader_drop)$(call READ_STRING)$(rem \
),$(if $(reader_next)\
  ,$(reader_next)$(reader_drop)$(call READ_STRING)$(rem \
),$(call _error,Expected '"'$(COMMA) got EOF))))
endef

define READ_SYMBOL
$(call reader_log,symbol or keyword)$(rem \
)$(if $(filter-out $(_TOKEN_DELIMS),$(reader_next))\
  ,$(reader_next)$(reader_drop)$(call READ_SYMBOL))
endef

# read and return tokens until $1 found
# The last element if any is followed by a space.
define READ_UNTIL
$(call reader_log,until $1)$(rem \
)$(READ_SPACES)$(rem \
)$(if $(filter $1,$(reader_next))\
  ,$(reader_drop)$(rem \
),$(if $(reader_next)\
  ,$(call READ_FORM) $(call READ_UNTIL,$1)$(rem \
),$(call _error,Expected '$1'$(COMMA) got EOF)))
endef

define READ_COMMENT
$(call reader_log,comment)$(rem \
)$(if $(filter-out $(_NL),$(reader_next))\
  ,$(reader_drop)$(call READ_COMMENT))
endef

define READ_SPACES
$(call reader_log,spaces)$(rem \
)$(if $(filter $(_SP) $(_NL) $(COMMA),$(reader_next))\
  ,$(reader_drop)$(call READ_SPACES)$(rem \
),$(if $(filter ;,$(reader_next))\
  ,$(READ_COMMENT)))
endef

define READ_FORM
$(call reader_log,form)$(rem \
)$(READ_SPACES)$(rem \
)$(if $(filter-out undefined,$(flavor READ_FORM_$(reader_next)))\
  ,$(call READ_FORM_$(reader_next)$(reader_drop))$(rem \
),$(foreach sym,$(READ_SYMBOL)\
  ,$(if $(filter false nil true,$(sym))\
    ,$(__$(sym))$(rem \
    ),$(call _symbol,$(sym)))))
endef

READ_FORM_ = $(call _error,expected a form$(COMMA) found EOF)

# Reader macros
READ_FORM_@ = $(call list,$(call _symbol,deref) $(call READ_FORM))
READ_FORM_' = $(call list,$(call _symbol,quote) $(call READ_FORM))#'
READ_FORM_` = $(call list,$(call _symbol,quasiquote) $(call READ_FORM))#`
READ_FORM_^ = $(call list,$(call _symbol,with-meta) $(foreach m,\
 $(call READ_FORM),$(call READ_FORM) $m))

READ_FORM_~ = $(call list,$(if $(filter @,$(reader_next))\
  ,$(reader_drop)$(call _symbol,splice-unquote)$(rem \
  ),$(call _symbol,unquote)) $(call READ_FORM))

# Lists, vectors and maps
# _map_new accepts a leading space, list and vector require )strip.
READ_FORM_{      = $(call _map_new,,$(strip $(call READ_UNTIL,})))
READ_FORM_$(_LP) = $(call list,$(strip $(call READ_UNTIL,$(_RP))))
READ_FORM_[      = $(call vector,$(strip $(call READ_UNTIL,])))
READ_FORM_}      = $(call _error,Unexpected '}')
READ_FORM_$(_RP) = $(call _error,Unexpected '$(_RP)')
READ_FORM_]      = $(call _error,Unexpected ']')

# Numbers
define READ_FORM_-
$(if $(filter 0 1 2 3 4 5 6 7 8 9,$(reader_next))\
  ,$(call _number,-$(READ_NUMBER))$(rem \
  ),$(call _symbol,-$(READ_SYMBOL)))
endef
READ_FORM_0 = $(call _number,0$(READ_NUMBER))
READ_FORM_1 = $(call _number,1$(READ_NUMBER))
READ_FORM_2 = $(call _number,2$(READ_NUMBER))
READ_FORM_3 = $(call _number,3$(READ_NUMBER))
READ_FORM_4 = $(call _number,4$(READ_NUMBER))
READ_FORM_5 = $(call _number,5$(READ_NUMBER))
READ_FORM_6 = $(call _number,6$(READ_NUMBER))
READ_FORM_7 = $(call _number,7$(READ_NUMBER))
READ_FORM_8 = $(call _number,8$(READ_NUMBER))
READ_FORM_9 = $(call _number,9$(READ_NUMBER))

# Strings
READ_FORM_" = $(call _string,$(call str_decode,$(READ_STRING)))#"

# Keywords
READ_FORM_$(encoded_colon) = $(call _keyword,$(READ_SYMBOL))

READ_STR = $(reader_init)$(or $(READ_FORM),$(__nil))

endif

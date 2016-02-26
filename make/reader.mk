#
# mal (Make Lisp) Parser/Reader
#

ifndef __mal_reader_included
__mal_reader_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk
include $(_TOP_DIR)types.mk
include $(_TOP_DIR)readline.mk

READER_DEBUG ?=

_TOKEN_DELIMS := $(SEMI) $(COMMA) $(DQUOTE) $(QQUOTE) $(_SP) $(_NL) $(_LC) $(_RC) $(_LP) $(_RP) $(LBRACKET) $(RBRACKET)

define READ_NUMBER
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(filter $(_TOKEN_DELIMS),$(ch)),\
      ,\
      $(if $(filter-out $(MINUS) $(NUMBERS),$(ch)),\
        $(call _error,Invalid number character '$(ch)'),\
        $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
        $(and $(READER_DEBUG),$(info READ_NUMBER ch: $(ch) | $($(1))))\
        $(ch)$(strip $(call READ_NUMBER,$(1))))),\
    ))
endef

# $(_NL) is used here instead of $(NEWLINE) because $(strip) removes
# $(NEWLINE). str_encode will just pass through $(_NL) so str_decode
# later will restore a correct newline
define READ_STRING
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(and $(filter \,$(ch)),$(filter $(DQUOTE),$(word 2,$($(1))))),\
      $(eval $(1) := $(wordlist 3,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_STRING ch: \$(word 1,$($(1))) | $($(1))))\
      $(DQUOTE) $(strip $(call READ_STRING,$(1))),\
    $(if $(and $(filter \,$(ch)),$(filter n,$(word 2,$($(1))))),\
      $(eval $(1) := $(wordlist 3,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_STRING ch: \$(word 1,$($(1))) | $($(1))))\
      $(_NL) $(strip $(call READ_STRING,$(1))),\
    $(if $(and $(filter \,$(ch)),$(filter \,$(word 2,$($(1))))),\
      $(eval $(1) := $(wordlist 3,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_STRING ch: \$(word 1,$($(1))) | $($(1))))\
      \ $(strip $(call READ_STRING,$(1))),\
    $(if $(filter $(DQUOTE),$(ch)),\
      ,\
      $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_STRING ch: $(ch) | $($(1))))\
      $(ch) $(strip $(call READ_STRING,$(1))))))),))
endef

define READ_SYMBOL
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(filter $(_TOKEN_DELIMS),$(ch)),\
      ,\
      $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_SYMBOL ch: $(ch) | $($(1))))\
      $(ch)$(strip $(call READ_SYMBOL,$(1)))),\
    ))
endef

define READ_KEYWORD
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(filter $(_TOKEN_DELIMS),$(ch)),\
      ,\
      $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
      $(and $(READER_DEBUG),$(info READ_KEYWORD ch: $(ch) | $($(1))))\
      $(ch)$(strip $(call READ_KEYWORD,$(1)))),\
    ))
endef

define READ_ATOM
$(foreach ch,$(word 1,$($(1))),\
  $(if $(and $(filter $(MINUS),$(ch)),$(filter $(NUMBERS),$(word 2,$($(1))))),\
    $(call _number,$(call READ_NUMBER,$(1))),\
  $(if $(filter $(NUMBERS),$(ch)),\
    $(call _number,$(call READ_NUMBER,$(1))),\
  $(if $(filter $(DQUOTE),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call __string,$(strip $(call READ_STRING,$(1))))\
    $(eval $(if $(filter $(DQUOTE),$(word 1,$($(1)))),\
           $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1)))),\
           $(call _error,Expected '$(DQUOTE)' in; $($(1))))),\
  $(if $(filter $(COLON),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _keyword,$(call READ_KEYWORD,$(1))),\
  $(foreach sym,$(call READ_SYMBOL,$(1)),\
    $(if $(call _EQ,nil,$(sym)),\
      $(__nil),\
    $(if $(call _EQ,true,$(sym)),\
      $(__true),\
    $(if $(call _EQ,false,$(sym)),\
      $(__false),\
      $(call _symbol,$(sym)))))))))))
endef

# read and return tokens until $(2) found
define READ_UNTIL
$(and $(READER_DEBUG),$(info READ_UNTIL: $($(1)) [$(2) $(3)]))
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(filter $(2),$(ch)),\
      ,\
      $(call READ_FORM,$(1))\
      $(call READ_UNTIL,$(1),$(2),$(3))),\
    $(call _error,Expected '$(3)')))
endef

define DROP_UNTIL
$(and $(READER_DEBUG),$(info DROP_UNTIL: $($(1)) [$(2)]))
$(foreach ch,$(word 1,$($(1))),\
  $(if $(ch),\
    $(if $(filter $(2),$(ch)),\
      ,\
      $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
      $(call DROP_UNTIL,$(1),$(2),$(3))),\
    ))
endef

define READ_SPACES
$(and $(READER_DEBUG),$(info READ_SPACES: $($(1))))
$(foreach ch,$(word 1,$($(1))),\
  $(if $(filter $(_SP) $(_NL) $(COMMA),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call READ_SPACES,$(1)),))
endef

define READ_FORM
$(and $(READER_DEBUG),$(info READ_FORM: $($(1))))
$(call READ_SPACES,$(1))
$(foreach ch,$(word 1,$($(1))),\
  $(if $(filter $(SEMI),$(ch)),\
    $(call DROP_UNTIL,$(1),$(_NL)),\
  $(if $(filter $(SQUOTE),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _list,$(call _symbol,quote) $(strip $(call READ_FORM,$(1)))),\
  $(if $(filter $(QQUOTE),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _list,$(call _symbol,quasiquote) $(strip $(call READ_FORM,$(1)))),\
  $(if $(filter $(UNQUOTE),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _list,$(call _symbol,unquote) $(strip $(call READ_FORM,$(1)))),\
  $(if $(filter $(_SUQ),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _list,$(call _symbol,splice-unquote) $(strip $(call READ_FORM,$(1)))),\
  $(if $(filter $(CARET),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(foreach meta,$(strip $(call READ_FORM,$(1))),\
      $(call _list,$(call _symbol,with-meta) $(strip $(call READ_FORM,$(1))) $(meta))),\
  $(if $(filter $(ATSIGN),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(call _list,$(call _symbol,deref) $(strip $(call READ_FORM,$(1)))),\
  $(if $(filter $(_RC),$(ch)),\
    $(call _error,Unexpected '$(RCURLY)'),\
  $(if $(filter $(_LC),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(foreach thm,$(call _hash_map),\
      $(call do,$(call _assoc_seq!,$(thm),$(strip $(call READ_UNTIL,$(1),$(_RC),$(RCURLY)))))\
      $(eval $(if $(filter $(_RC),$(word 1,$($(1)))),\
               $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1)))),\
               $(call _error,Expected '$(RCURLY)')))\
      $(thm)),\
  $(if $(filter $(_RP),$(ch)),\
    $(call _error,Unexpected '$(RPAREN)'),\
  $(if $(filter $(_LP),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(foreach tlist,$(call _list),\
      $(eval $(foreach item,$(strip $(call READ_UNTIL,$(1),$(_RP),$(RPAREN))),\
               $(call do,$(call _conj!,$(tlist),$(item)))))\
      $(eval $(if $(filter $(_RP),$(word 1,$($(1)))),\
               $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1)))),\
               $(call _error,Expected '$(RPAREN)')))\
      $(tlist)),\
  $(if $(filter $(RBRACKET),$(ch)),\
    $(call _error,Unexpected '$(RBRACKET)'),\
  $(if $(filter $(LBRACKET),$(ch)),\
    $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1))))\
    $(foreach tvec,$(call _vector),\
      $(eval $(foreach item,$(strip $(call READ_UNTIL,$(1),$(RBRACKET),$(RBRACKET))),\
               $(call do,$(call _conj!,$(tvec),$(item)))))\
      $(eval $(if $(filter $(RBRACKET),$(word 1,$($(1)))),\
               $(eval $(1) := $(wordlist 2,$(words $($(1))),$($(1)))),\
               $(call _error,Expected '$(RBRACKET)')))\
      $(tvec)),\
  $(call READ_ATOM,$(1))))))))))))))))
$(call READ_SPACES,$(1))
endef

# read-str from a raw "string" or from a string object
READ_STR = $(strip $(eval __reader_temp := $(call str_encode,$(if $(call _string?,$(1)),$(call str_decode,$($(1)_value)),$(1))))$(call READ_FORM,__reader_temp))

endif

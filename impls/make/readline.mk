#
# mal (Make Lisp) shell readline wrapper
#

ifndef __mal_readline_included
__mal_readline_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk

# Call bash read/readline. Since each call is in a separate shell
# instance we need to restore and save after each call in order to
# have readline history.
READLINE_HISTORY_FILE := $${HOME}/.mal-history

# Either empty (if EOF) or an encoded string with the 'ok' suffix.
READLINE = $(call str_encode_nospace,$(shell \
 history -r $(READLINE_HISTORY_FILE); \
 read -u 0 -r -e -p '$(str_decode_nospace)' line && \
 history -s -- "$${line}" && \
 echo "$${line}ok" ; \
 history -a $(READLINE_HISTORY_FILE) 2>/dev/null || \
 true \
))

endif

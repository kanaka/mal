#
# mal (Make Lisp) shell readline wrapper
#

ifndef __mal_readline_included
__mal_readline_included := true

# Call bash read/readline. Since each call is in a separate shell
# instance we need to restore and save after each call in order to
# have readline history.
READLINE_EOF :=
READLINE_HISTORY_FILE := $${HOME}/.mal-history
READLINE = $(eval __readline_temp := $(shell history -r $(READLINE_HISTORY_FILE); read -u 0 -r -e -p $(if $(1),$(1),"user> ") line && history -s -- "$${line}" && echo "$${line}" || echo "__||EOF||__"; history -a $(READLINE_HISTORY_FILE) 2>/dev/null || true))$(if $(filter __||EOF||__,$(__readline_temp)),$(eval READLINE_EOF := yes),$(__readline_temp))

endif

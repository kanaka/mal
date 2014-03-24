
# assert macros
assert = $(if $1,,$(error assert failure: $2))
assert_not = $(if $1,$(error assert_not: $2),)
assert_eq = $(if $(call _EQ,$(1),$(2)),,$(error assert_eq failure: $(1) != $(2): $(3)))
# With debug:
#assert_eq = $(info 1: $(1))$(info 2: $(2))$(info 3: $(3))$(if $(call _EQ,$(1),$(2)),,$(error assert_eq failure: $(3)))


# REPL related wrappers
test_read = $(call READ_STR,$(1))
ifndef MACROEXPAND
define MACROEXPAND
$(1)
endef
endif
test_re = $(strip $(call EVAL,$(call MACROEXPAND,$(strip $(call test_read,$(1))),$(REPL_ENV)),$(REPL_ENV)))
test_rep = $(call PRINT,$(strip $(call EVAL,$(call MACROEXPAND,$(strip $(call test_read,$(1))),$(REPL_ENV)),$(REPL_ENV))))

# To load this file:
#   $(eval include rules.mk)

# Usage:
#   (make* "$(eval $(call PRINT_RULE,abc,,@echo \"building $$@\"))")
define PRINT_RULE
$(1): $(2)
	$(3)
endef

# Usage:
#   (make* "$(eval $(call PRINT_LINES,abc:,	@echo \"shell command\"))")
define PRINT_LINES
$(1)
$(2)
$(3)
$(4)
$(5)
$(6)
$(7)
$(8)
$(9)
$(10)
$(11)
$(12)
$(13)
$(14)
$(15)
$(16)
$(17)
$(18)
$(19)
$(20)
endef

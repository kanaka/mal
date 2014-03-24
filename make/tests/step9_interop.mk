INTERACTIVE =

include tests/common.mk
include step9_interop.mk

$(info Testing trivial macros)
$(call assert_eq,7,$(call test_rep, (make* "7") ))
$(call assert_eq,"XaY XbY XcY",$(call test_rep, (make* "\"$(foreach v,a b c,X$(v)Y)\"") ))
$(call assert_eq,(2 3 4),$(call test_rep, (make* "($(foreach v,1 2 3,$(call gmsl_plus,1,$(v))))") ))


.PHONY: all
all:
	@echo "All tests completed"

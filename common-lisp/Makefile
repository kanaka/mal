# Helper functions
define record_lisp
	$(shell (test -f "hist/$(1)_impl" && grep -q $(2) "hist/$(1)_impl") || echo $(2) > "hist/$(1)_impl")
endef

define steps
	$(if $(MAKECMDGOALS),\
		$(if $(findstring all,$(MAKECMDGOALS)),\
			stepA_mal,\
			$(filter step%, $(MAKECMDGOALS))),\
		stepA_mal)
endef

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SOURCES_LISP := src/env.lisp src/core.lisp src/stepA_mal.lisp
SOURCES := src/utils.lisp src/types.lisp src/reader.lisp src/printer.lisp $(SOURCES_LISP)
LISP ?= sbcl
ABCL ?= abcl

# Record the Common Lisp implementation used for all steps built in this
# invocation This is used in the targets to rebuild the step if the
# implementation changes
$(foreach step, $(call steps), $(call record_lisp,$(patsubst step%,%,$(step)),$(LISP)))

.PRECIOUS: hist/%_impl
.PHONY: stats

all : stepA_mal

hist/%_impl: ;

# CL_LAUNCH_VERSION is only defined while building it. We change to the
# directory of the CL_LAUNCH_FILE in --wrap script so that the script can find the dumped
# image even if invoked from some directory different from where it
# currently resides
step% : src/step%.lisp src/utils.lisp src/types.lisp src/env.lisp src/printer.lisp src/reader.lisp src/core.lisp hist/%_impl
ifeq ($(LISP),abcl)
	echo -n '#!/bin/sh\ncd `dirname $$0` ; $(ABCL) --noinform --noinit --nosystem --load run-abcl.lisp -- $@ $$@' > $@
	chmod +x $@
else
	cl-launch --wrap 'if [ -z "$$CL_LAUNCH_VERSION" ] ; then cd "$$(dirname $$CL_LAUNCH_FILE)" ; fi' --verbose --lisp $(LISP) --source-registry $(ROOT_DIR) --system $@ --dump images/$@.$(LISP).image -o $@ --entry 'mal:main'
endif

clean:
	find . -name 'step*' -executable -exec git check-ignore \{\} \; -delete
	rm -f *.lib *.fas[l] images/* hist/*_impl

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*;|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

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

LISP ?= sbcl
ABCL ?= abcl
MKCL ?= mkcl

# TODO: In theory cl-launch should be able to build standalone executable using
# MKCL unfortunately the executable crashes on startup
STANDALONE_EXE = sbcl clisp ccl ecl cmucl

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Record the Common Lisp implementation used for all steps built in this
# invocation This is used in the targets to rebuild the step if the
# implementation changes
$(foreach step, $(call steps), $(call record_lisp,$(patsubst step%,%,$(step)),$(LISP)))

.PRECIOUS: hist/%_impl

all : stepA_mal

hist/%_impl: ;

# CL_LAUNCH_VERSION is only defined while building it. We change to the
# directory of the CL_LAUNCH_FILE in --wrap script so that the script can find the dumped
# image even if invoked from some directory different from where it
# currently resides
step% : src/step%.lisp src/utils.lisp src/types.lisp src/env.lisp src/printer.lisp src/reader.lisp src/core.lisp hist/%_impl

ifeq ($(LISP),clisp)
	@echo "=============================================================="
	@echo "WARNING: This build might fail since GNU Clisp does not have bundled version of asdf (yet)"
	@echo "Please do something like below to make it work"
	@echo "(mkdir -p ~/common-lisp/ && cd ~/common-lisp && git clone -b release https://gitlab.common-lisp.net/asdf/asdf.git && cd asdf && make)"
	@echo "=============================================================="
endif

ifneq ($(filter $(LISP),$(STANDALONE_EXE)),)
	cl-launch --wrap 'if [ -z "$$CL_LAUNCH_VERSION" ] ; then cd "$$(dirname $$CL_LAUNCH_FILE)" ; fi' --verbose --lisp $(LISP) --source-registry $(ROOT_DIR) --system $@ --dump '!' -o $@ --entry 'mal:main'
else ifeq ($(LISP),abcl)
	echo -n '#!/bin/sh\ncd `dirname $$0` ; $(ABCL) --noinform --noinit --nosystem --load run-abcl.lisp -- $@ $$@' > $@
	chmod +x $@
else ifeq ($(LISP),mkcl)
	$(MKCL) -eval '(progn (require "asdf") (push *default-pathname-defaults* asdf:*central-registry*) (asdf:load-system "$@") (quit))'
	echo -n '#!/bin/sh\ncd `dirname $$0` ; $(MKCL) -q -load run-mkcl.lisp -- $@ $$@' > $@
	chmod +x $@
else ifeq ($(LISP),allegro)
	cl-launch --wrap 'if [ -z "$$CL_LAUNCH_VERSION" ] ; then cd "$$(dirname $$CL_LAUNCH_FILE)" ; fi' --verbose --lisp $(LISP) --source-registry $(ROOT_DIR) --system $@ --dump images/$@.$(LISP).image -o $@ --entry 'mal:main'
else
	@echo "Unsupported Lisp implementation $(LISP)"
	@exit 1
endif

clean:
	find . -maxdepth 1 -name 'step*' -executable -delete
	rm -f *.lib *.fas[l] images/* hist/*_impl

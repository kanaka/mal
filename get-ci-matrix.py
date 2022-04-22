#!/usr/bin/env python3

import json
import os
import re
import sys
import yaml

IMPLS_FILE = "IMPLS.yml"
RE_IGNORE = re.compile(r'(^LICENSE$|^README.md$|^docs/|^process/)')
RE_IMPL = re.compile(r'^impls/(?!lib|tests)([^/]*)/')

OVERRIDE_IMPLS = os.environ.get('OVERRIDE_IMPLS', '').split()

def impl_text(impl):
    s = "IMPL=%s" % impl['IMPL']
    for k, v in impl.items():
        if k == 'IMPL': continue
        s += " %s=%s" % (k, v)
    return s

all_changes = sys.argv[1:]
# code changes that are not just to docs
code_changes = set([c for c in all_changes if not RE_IGNORE.search(c)])
# actual changes to implementations
impl_changes = set([c for c in all_changes if RE_IMPL.search(c)])
# names of changed implementations
run_impls = set([RE_IMPL.search(c).groups()[0] for c in impl_changes])

do_full = (len(code_changes) != len(impl_changes))

# If we have non-implementation code changes then we will add all
# implementations to the test matrix
if OVERRIDE_IMPLS:
    run_impls = OVERRIDE_IMPLS
    if 'all' in OVERRIDE_IMPLS:
        do_full = True


print("OVERRIDE_IMPLS: %s" % OVERRIDE_IMPLS)
print("code_changes: %s (%d)" % (code_changes, len(code_changes)))
print("impl_changes: %s (%d)" % (impl_changes, len(impl_changes)))
print("run_impls: %s (%d)" % (run_impls, len(run_impls)))
print("do_full: %s" % do_full)

# Load the full implementation description file
all_impls = yaml.safe_load(open(IMPLS_FILE))

# Accumulate and output linux and macos implementations separately
linux_impls = []
macos_impls = []
for impl in all_impls['IMPL']:
    targ = linux_impls
    if 'OS' in impl and impl['OS'] == 'macos':
        targ = macos_impls
    # Run implementations with actual changes first before running
    # other impls triggered by non-impl code changes
    if impl['IMPL'] in run_impls:
        targ.insert(0, impl_text(impl))
    elif do_full:
        targ.append(impl_text(impl))

print("::set-output name=do-linux::%s" % json.dumps(len(linux_impls)>0))
print("::set-output name=do-macos::%s" % json.dumps(len(macos_impls)>0))
print("::set-output name=linux::{\"IMPL\":%s}" % json.dumps(linux_impls))
print("::set-output name=macos::{\"IMPL\":%s}" % json.dumps(macos_impls))

#!/bin/bash

# add_steps.sh input-file output-file
#
# Adds placeholder annotations to each line of a file. These annotations
# indicate which version(s) of the main (step*.swift) file the line should be
# included in. The annotations are just placeholders, and need to be edited to
# identify the right file versions.
#
# e.g.:
#
#	$ ./add_steps.sh stepA_mal.swift main_template.swift

SPC10="          "
SPC20="${SPC10}${SPC10}"
SPC40="${SPC20}${SPC20}"
SPC80="${SPC40}${SPC40}"
SPC160="${SPC80}${SPC80}"
sed < $1 > $2 -e "s/\(.*\)/\1${SPC160}/" -e "/^\(.\)\{156\}    .*$/s/\(.\{160\}\).*/\1\/\/ malstep(A)/"

# TBD: try the following, subsequently found on stackoverflow:
#
#	sed -i ':a;/.\{63\}/!{s/$/ /;ba}' file

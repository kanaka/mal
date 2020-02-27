#!/bin/bash

# filter_steps.sh step input-file output-file
#
# Filter the template file to produce a specific version of the file. E.g.:
#
#	$ ./filter_steps 4 main_template.swift step4_if_fn_do.swift

grep "malstep.*\<$1\>" $2 | sed -e 's/\(.*\)\/\/ malstep(.*)$/\1/' -e 's/ *$//' > $3

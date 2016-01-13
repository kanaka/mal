#!/bin/sh

# Run Vim in ex mode (-e) and run the given script ($1) on startup. Our scripts
# end with 'qall!' which causes actual Vim UI to never start up.
#
# Set environment variable DEBUG=1 to allow more verbose error output from Vim.
#
# See: http://vim.wikia.com/wiki/Vim_as_a_system_interpreter_for_vimscript

rundir=`dirname $0`
export LD_LIBRARY_PATH=`readlink -f $rundir`
vimscriptfile="$1"
shift
if [ x$DEBUG = x ] ; then
  exec 2> /dev/null
fi
exec vim -i NONE -V1 -nNesS $vimscriptfile -- "$@" | cat

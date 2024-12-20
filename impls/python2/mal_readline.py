# Importing this module is sufficient for the 'input' builtin command
# to support readline.

import atexit
import os.path
from readline import read_history_file, set_history_length, write_history_file
import sys

if sys.version_info[0] < 3:
    _exc = Exception
    readline = raw_input
else:
    _exc = FileNotFoundError
    readline = input

histfile = os.path.join(os.path.expanduser("~"), ".mal-history")
try:
    read_history_file(histfile)
except _exc:
    pass
set_history_length(1000)

atexit.register(write_history_file, histfile)

# Importing this module is sufficient for the 'input' builtin command
# to support readline.

import atexit
import os.path
import readline
import sys


histfile = os.path.join(os.path.expanduser("~"), ".mal-history")
try:
    readline.read_history_file(histfile)
except (Exception if sys.version_info[0] < 3 else FileNotFoundError):
    pass
readline.set_history_length(1000)

atexit.register(readline.write_history_file, histfile)

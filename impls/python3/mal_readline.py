# Importing this module is sufficient for the 'input' builtin command
# to support readline.

import atexit
import os.path
import readline

histfile = os.path.join(os.path.expanduser('~'), '.mal-history')
try:
    readline.read_history_file(histfile)
except FileNotFoundError:
    pass
readline.set_history_length(1000)
atexit.register(readline.write_history_file, histfile)


def input_(prompt: str) -> str:
    line = input(prompt)
    if line:
        readline.add_history(line)
    return line

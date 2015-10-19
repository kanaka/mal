#import os, readline as pyreadline
#
#histfile = os.path.expanduser("~/.mal-history")
#
#def init():
#    try:
#        with open(histfile, "r") as hf:
#            for line in hf.readlines():
#                pyreadline.add_history(line.rstrip("\r\n"))
#                pass
#    except IOError:
#        #print("Could not open %s" % histfile)
#        pass
#
#def readline(prompt="user> "):
#    try:
#        line = raw_input(prompt)
#        pyreadline.add_history(line)
#        with open(histfile, "a") as hf:
#            hf.write(line + "\n")
#    except IOError:
#        pass
#    except EOFError:
#        return None
#    return line

import os
def readline(prompt):
    res = ''
    os.write(1, prompt)
    while True:
        buf = os.read(0, 255)
        if not buf: raise EOFError()
        res += buf
        if res[-1] == '\n': return res[:-1]


import os, readline as pyreadline

history_loaded = False
histfile = os.path.expanduser("~/.mal-history")

def readline(prompt="user> "):
    if not history_loaded:
        try:
            with open(histfile, "r") as hf:
                for line in hf.readlines():
                    pyreadline.add_history(line.rstrip("\r\n"))
                    pass
        except IOError:
            print("Could not open %s" % histfile)
            pass

    try:
        line = raw_input(prompt)
        pyreadline.add_history(line)
        with open(histfile, "a") as hf:
            hf.write(line + "\n")
        return line
    except EOFError:
        return None

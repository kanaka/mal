import os
from os import fork, execv, pipe, close, dup2, kill, read, write
from select import select
import json
from os.path import dirname, realpath
from os import environ
import signal
from sys import argv
import fcntl

DEBUG = False
HALT = False

# Bestow IO upon jq

def _read(fname, out=None):
    with open(fname, "r") as f:
        data = json.dumps(f.read()) + "\n"
        # print("data =", data)
        write(out, bytes(data, 'utf-8'))

def _readline(prompt="", out=None):
    data = json.dumps(input(prompt)) + "\n"
    # print("data =", data)
    write(out, bytes(data, 'utf-8'))

def _fwrite(fname, data, out=None):
    return

def _halt(out=None):
    global HALT
    HALT = True

def stub(*args, out=None):
    raise Exception("command not understood")

rts = {
    "read": _read,
    "readline": _readline,
    "fwrite": _fwrite,
    "halt": _halt,
}

def process(cmd, fout):
    if type(cmd) == str:
        print(cmd, end="")
    elif type(cmd) == dict:
        cmd = cmd['command']
        command = cmd['cmd']
        args = cmd['args']
        fn = rts.get(command, stub)
        fn(*args, out=fout)

def get_one(fd):
    s = b""
    while True:
        x = read(fd, 1)
        if x == b'\n':
            break
        if x == b'':
            break
        s += x
    if s == "":
        return None
    return s.decode('utf-8')


def main(args):
    args = [
        "jq", "--argjson", "DEBUG", json.dumps(DEBUG), "-nrRM",
        "-f",
        dirname(realpath(__file__)) + "/" + environ.get("STEP", "stepA_mal") + ".jq",
        "--args",
        *args
    ]
    # print(args)
    sin_pipe  = pipe()
    sout_pipe = pipe()

    pid = fork()
    if pid == 0:
        # jq
        close(sin_pipe[1])
        close(sout_pipe[0])

        dup2(sin_pipe[0], 0)
        dup2(sout_pipe[1], 2) # bind to stderr, as we write there
        dup2(sout_pipe[1], 1)

        execv("/usr/bin/jq", args)
    else:
        close(sin_pipe[0])
        close(sout_pipe[1])

        msout = sin_pipe[1]
        msin = sout_pipe[0]

        while True:
            try:
                if HALT:
                    break
                cmd = get_one(msin)
                # print(cmd)
                if cmd:
                    process(json.loads(cmd)[1], msout)
            except KeyboardInterrupt:
                exit()
            except Exception as e:
                print("RTS Error:", e)


main(argv[1:])

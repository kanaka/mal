include "utils";

def READ:
    .;

def EVAL:
    .;

def PRINT:
    .;

def repl:
    # Infinite generator, interrupted by ./run.
   "user> " | __readline |
   READ | EVAL |
   PRINT, repl;

repl

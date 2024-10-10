import LeanMal.reader
import LeanMal.printer

universe u

def READ (input : String) :=
  read_str input

def EVAL (ast : Types) (_: String) := ast

def PRINT (ast : Types): String :=
  pr_str true ast

def rep (input : String): String :=
  match READ input with
  | Except.ok result =>
    PRINT (EVAL result "")
  | Except.error err =>
    s!"Parsing failed: {err}"

def main : IO Unit := do
  let mut donext := true
  while donext do
    IO.print "user> "
    let stdin ← IO.getStdin
    let input ← stdin.getLine
    let value := input.trim
    if value = "exit" then
      donext := false
      IO.println "Exiting REPL."
    if value.isEmpty then
      donext := false
    else
      IO.println (rep value)

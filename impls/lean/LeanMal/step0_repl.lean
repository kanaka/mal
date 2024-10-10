universe u

def READ (input : String) := input

def EVAL (input : String) (_: String) := input

def PRINT (input : String) := input

def rep (input : String): String :=
  PRINT (EVAL (READ input) "")

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

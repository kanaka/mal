import LeanMal.types

universe u

def pr_str (readably: Bool) (input : Types)  : String :=
  input.toString readably

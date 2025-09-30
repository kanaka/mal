⟨ReadStr⟩ ← •Import "reader.bqn"
⟨PrStrR⟩ ← •Import "printer.bqn"

READ ← ReadStr
EVAL ← ⊢
PRINT ← PrStrR
Rep ← PRINT∘EVAL∘READ

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
}@

⟨ReadStr⟩ ← •Import "reader.bqn"
⟨PrStr⟩ ← •Import "printer.bqn"

READ ← ReadStr
EVAL ← ⊢
PRINT ← PrStr
Rep ← PRINT∘EVAL∘READ

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
}@

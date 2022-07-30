READ ← ⊢
EVAL ← ⊢
PRINT ← ⊢
Rep ← PRINT∘EVAL∘READ

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
} @

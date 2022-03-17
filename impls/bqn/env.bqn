Env ⇐ {𝕊 outer:
  d ⇐ ↕0‿2
  Set ⇐ {𝕊 k‿v:
    d ↩ k‿v∾((<k)≢˘⊏˘)⊸/d
  }
  Get ⇐ {𝕊 k: {
    i ← ⊑(⊏˘d)⊐<k
    i<⊑≢d ? i‿1⊑d ;
    outer≠@ ? outer.Get k ;
    ¯1∾<"'"∾k∾"' not found"
  }}
}

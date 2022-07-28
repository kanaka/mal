⟨ReadStr⟩ ← •Import "reader.bqn"
⟨PrStrR⟩ ← •Import "printer.bqn"

replEnv ← >⟨
  "+"‿1‿(+´)
  "-"‿1‿(-´)
  "*"‿1‿(×´)
  "/"‿1‿(÷´)
⟩
Undef ← {@‿¯1‿(<"undefined: "∾𝕩)}

EvalAst ← {env 𝕊 t‿v:
  {0: idx ← ⊑((⊏˘env)⊐(<v))
      {idx<≠env ? idx⊏env ; Undef v};
   3: env⊸EVAL¨v ;
   t‿v
  }t
}

READ ← ReadStr
PRINT ← PrStrR
EVAL ← {
  env 𝕊 3‿⟨⟩: 3‿⟨⟩ ;
  env 𝕊 3‿xs: {
    ys ← env EvalAst 3‿xs
    n‿t‿f ← ⊑ys
    zs ← 1↓ys
    t∾F 1⊸↓¨zs
  } ;
  env 𝕊 4‿xs: 4∾<env⊸EVAL¨xs ;
  env 𝕊 5‿xs: 5∾<env⊸EVAL¨xs ;
  env 𝕊 x: env EvalAst x
}
Rep ← PRINT∘{replEnv EVAL 𝕩}∘READ

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
}@

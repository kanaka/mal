⟨nil⟩ ← •Import "globals.bqn"
⟨Env⟩ ← •Import "env.bqn"
⟨ReadStr⟩ ← •Import "reader.bqn"
⟨PrStrR⟩ ← •Import "printer.bqn"
⟨core⟩ ← •Import "core.bqn"

EvalAst ← {env 𝕊 t‿v:
  {0: env.Get v ;
   3: env⊸EVAL¨v ;
   t‿v
  }t
}

READ ← ReadStr
PRINT ← PrStrR
EVAL ← {
  # builtin special forms
  env 𝕊 3‿⟨0‿"def!",0‿k,x⟩:
    v ← env EVAL x
    {¯1:@;env.Set (<k)∾<v}⊑v
    v ;
  outer 𝕊 3‿⟨0‿"let*",t‿binds,exp⟩:
    inner ← Env outer
    {𝕊
      {⟨0‿kw,x⟩: inner.Set (<kw)∾<inner EVAL x}2↑binds
      binds 2⊸↓↩
    }•_while_{𝕊 ⋄ 0<≠binds}@
    inner EVAL exp ;
  env 𝕊 3‿⟨0‿"if",c,t,f⟩:
    env EVAL {8‿x:f; 7‿0:f; t} env EVAL c ;
  env 𝕊 3‿⟨0‿"if",c,t⟩:
    env EVAL {8‿x:nil; 7‿0:nil; t} env EVAL c ;
  outer 𝕊 3‿⟨0‿"fn*",t‿args,exp⟩:
    9‿{𝕊 xs:
      as ← args
      inner ← Env outer
      {𝕊
        {0‿"&": inner.Set (<1⊑1⊑as)∾<3∾<inner EVAL¨xs ⋄ as↩⟨⟩;
         0‿kw: inner.Set (<kw)∾<inner EVAL ⊑xs}⊑as
        as 1⊸↓↩
        xs 1⊸↓↩
      }•_while_{𝕊 ⋄ 0<≠as}@
      inner EVAL exp} ;

  # variable-length special forms
  env 𝕊 3‿⟨⟩: 3‿⟨⟩ ;
  env 𝕊 3‿xs:{
    0‿"do": ¯1⊑env⊸EVAL¨1↓xs
    ; ys ← env EvalAst 3‿xs          # function application
      f ← ⊑ys
      {9‿f: F 1↓ys ;
       ¯1‿x: f;
       ¯1∾<"not a function: "∾PrStrR f} f
    }⊑xs ;

  # basic forms
  env 𝕊 4‿xs: 4∾<env⊸EVAL¨xs ;
  env 𝕊 5‿xs: 5∾<env⊸EVAL¨xs ;
  env 𝕊 x: env EvalAst x
}
Rep ← PRINT∘{core EVAL 𝕩}∘READ

Rep "(def! not (fn* (a) (if a false true)))"

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
}@

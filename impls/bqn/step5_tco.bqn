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

SetParams ← {env 𝕊 args‿xs:
  {𝕊
    {0‿"&": env.Set (<1⊑1⊑args)∾<3∾<env EVAL¨xs ⋄ args↩⟨⟩;
     0‿kw: env.Set (<kw)∾<env EVAL ⊑xs}⊑args
    args 1⊸↓↩
    xs 1⊸↓↩
  }•_while_{𝕊 ⋄ 0<≠args}@
}

READ ← ReadStr
PRINT ← PrStrR
EVAL ← {e 𝕊 ast:
  # this loop runs as long as @ is returned, ie. typically once
  {𝕊 ⋄ {
    # builtin special forms
    𝕊 3‿⟨0‿"def!",0‿k,x⟩:
      v ← e EVAL x
      {¯1:@;e.Set (<k)∾<v}⊑v
      v ;
    𝕊 3‿⟨0‿"let*",t‿binds,exp⟩:
      inner ← Env e
      {𝕊
        {⟨0‿kw,x⟩: inner.Set (<kw)∾<inner EVAL x}2↑binds
        binds 2⊸↓↩
      }•_while_{𝕊 ⋄ 0<≠binds}@
      e ↩ inner
      ast ↩ exp
      @;
    𝕊 3‿⟨0‿"if",c,t,f⟩:
      ast ↩ {8‿x:f; 7‿0:f; t} e EVAL c
      @;
    𝕊 3‿⟨0‿"if",c,t⟩:
      ast ↩ {8‿x:nil; 7‿0:nil; t} e EVAL c
      @;
    𝕊 3‿⟨0‿"fn*",t‿args,exp⟩:
      10‿exp‿args‿e‿{𝕊 xs:
        inner ← Env e
        inner SetParams args‿xs
        inner EVAL exp} ;

    # variable-length special forms
    𝕊 3‿⟨⟩: 3‿⟨⟩ ;
    𝕊 3‿xs:{
      0‿"do":
        e⊸EVAL¨¯1↓1↓xs
        ast ↩ ¯1⊑xs
        @
      ; ys ← e EvalAst 3‿xs          # function application
        f ← ⊑ys
        {9‿f: F 1↓ys ;
         10‿exp‿args‿e2‿fn:
           ast ↩ exp
           e ↩ Env e2
           e SetParams args‿(1↓ys)
           @;
         ¯1‿x: f;
         ¯1∾<"not a function: "∾PrStrR f} f
      }⊑xs ;

    # basic forms
    𝕊 4‿xs: 4∾<e⊸EVAL¨xs ;
    𝕊 5‿xs: 5∾<e⊸EVAL¨xs ;
    𝕊 x: e EvalAst x
  } ast
}•_while_{𝕩≡@}@}

Rep ← PRINT∘{core EVAL 𝕩}∘READ

Rep "(def! not (fn* (a) (if a false true)))"

{
  •Out "user> "
  line ← •GetLine 0
  •Out Rep line
  𝕊𝕩
}@

⟨Atom,nil,Err,_chk,_chks⟩ ← •Import "globals.bqn"
⟨Env⟩ ← •Import "env.bqn"
⟨PrStr,PrStrR⟩ ← •Import "printer.bqn"
⟨ReadStr⟩ ← •Import "reader.bqn"

Q ← {∾´'"'∾𝕩∾'"'}
J ← {0=≠𝕩 ? "" ;
     1=≠𝕩 ? ⊑𝕩 ;
     ∾´(⊑𝕩)∾𝕨∾𝕨J 1↓𝕩}

ns ← ⟨
  "+"‿{1∾+´1⊏˘>𝕩}
  "-"‿{1∾-´1⊏˘>𝕩}
  "*"‿{1∾×´1⊏˘>𝕩}
  "/"‿{1∾⌊∘÷´1⊏˘>𝕩}

  "="‿{7∾∧´≡´˘2↕𝕩}
  "<"‿{7∾∧´<´˘2↕1⊸⊑¨𝕩}
  "<="‿{7∾∧´≤´˘2↕1⊸⊑¨𝕩}
  ">"‿{7∾∧´>´˘2↕1⊸⊑¨𝕩}
  ">="‿{7∾∧´≥´˘2↕1⊸⊑¨𝕩}

  "empty?"‿{7∾0=≠1⊑⊑𝕩}
  "count"‿{1∾≠1⊑⊑𝕩}

  "bqn-show"‿{•Show ⊑ 𝕩}
  "pr-str"‿{2∾<" "J PrStrR¨𝕩}
  "str"‿{2∾<""J PrStr¨𝕩}
  "prn"‿{•Out " "J PrStrR¨𝕩 ⋄ nil}
  "println"‿{•Out " "J PrStr¨𝕩 ⋄ nil}

  "read-string"‿{ReadStr 1⊑⊑𝕩}
  "slurp"‿{2∾<•FChars 1⊑⊑𝕩}

  "atom"‿{11∾Atom ⊑𝕩}
  "atom?"‿{7∾11=⊑⊑𝕩}
  "deref"‿{𝕊 ⟨11‿a⟩: a.Deref@; Err "deref: not an atom"}
  "reset!"‿{
    𝕊 ⟨11‿a,v⟩: a.Reset v;
    Err "reset!: not an atom"}
  "swap!"‿{
    ⟨t‿a,fn⟩ ← 2↑𝕩
    args ← (<a.Deref@) ∾ 2↓𝕩
    f ← {9‿f: f; 10‿x‿y‿z‿f: f}fn
    a.Reset F args}

  "cons"‿{𝕊 x‿⟨t,xs⟩: 3∾<(<x)∾xs ; Err "cons"}
  "concat"‿{𝕊 ⟨⟩:3‿⟨⟩; 3∾<∾´1⊸⊑¨𝕩}

  "nth"‿{𝕊 ⟨t‿xs,1‿n⟩: {n<≠xs ? n⊑xs ; Err "out of bounds"}}
  "first"‿{S ⟨t‿xs⟩: {0<≠xs ? ⊑xs ; nil}; nil}
  "rest"‿{𝕊 ⟨t‿⟨⟩⟩: 3‿⟨⟩; 𝕊 ⟨t‿xs⟩: 3∾<1↓xs; 3‿⟨⟩}

  "throw"‿{𝕊 ⟨x⟩: ¯1‿x}

  "apply"‿{𝕊
    fn ← {9‿f:f; 10‿b‿a‿e‿f: f}⊑𝕩
    args ← (¯1↓1↓𝕩)∾1⊑¯1⊑𝕩
    Fn _chks args}
  "map"‿{𝕊
    fn ← {9‿f:f; 10‿b‿a‿e‿f: f}⊑𝕩
    {3∾<𝕩}_chks{Fn ⟨𝕩⟩}¨ 1⊑1⊑𝕩}

  "nil?"‿{𝕊 ⟨8‿⟨⟩⟩: 7‿1; 7‿0}
  "true?"‿{𝕊 ⟨7‿1⟩: 7‿1; 7‿0}
  "false?"‿{𝕊 ⟨7‿0⟩: 7‿1; 7‿0}
  "symbol?"‿{𝕊 ⟨0‿x⟩: 7‿1; 7‿0}
  "keyword?"‿{𝕊 ⟨6‿x⟩: 7‿1; 7‿0}
  "vector?"‿{𝕊 ⟨4‿x⟩: 7‿1; 7‿0}
  "list?"‿{𝕊 ⟨3‿x⟩: 7‿1; 7‿0}
  "map?"‿{𝕊 ⟨5‿x⟩: 7‿1; 7‿0}
  "sequential?"‿{𝕊 ⟨3‿x⟩: 7‿1; 𝕊 ⟨4‿x⟩: 7‿1; 7‿0}

  "symbol"‿{0∾<1⊑⊑𝕩}
  "keyword"‿{6∾<1⊑⊑𝕩}
  "list"‿{3‿𝕩}
  "vector"‿{4‿𝕩}
  "hash-map"‿{5‿𝕩}

  "keys"‿{𝕊 ⟨5‿xs⟩: 3∾<⥊⊏˘∘‿2⥊xs}
  "vals"‿{𝕊 ⟨5‿xs⟩: 3∾<⥊1⊏˘∘‿2⥊xs}
  "get"‿{𝕊 ⟨5‿xs,k⟩: 1⊑@‿nil∾˜⥊(k⊸≡˘⊑˘)⊸/∘‿2⥊xs; nil}
  "contains?"‿{𝕊 ⟨5‿xs,k⟩: 7∾×+´k⊸≡˘⊑˘∘‿2⥊xs}
  "dissoc"‿{𝕊 ⟨5‿xs,k⟩: 5∾<⥊(k⊸≢˘⊑˘)⊸/∘‿2⥊xs}
  "assoc"‿{𝕊 ⟨5‿xs,k,v⟩: 5∾<k‿v∾˜⥊(k⊸≢˘⊑˘)⊸/∘‿2⥊xs}
⟩

core ⇐ Env@
{𝕊 kw‿f: core.Set kw‿(9‿f)}¨ns

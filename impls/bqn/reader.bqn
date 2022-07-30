⟨nil⟩ ← •Import "globals.bqn"
# Types:
# ¯1 error
# 0 symbol
# 1 int
# 2 string
# 3 list
# 4 vector
# 5 hashmap
# 6 keyword
# 7 boolean
# 8 nil
# 9 function
# 10 fn*
# 11 atom
# 12 macro

Reader ← {𝕊 tokens:
  r ← {
    ts ← tokens
    Peek ⇐ {𝕊 ⋄ 0=≠ts ? @; ⊑ts}
    Next ⇐ {𝕊
      t ← r.Peek@
      ts ↩ 1↓ts
      t
    }
  }
}

T ← {𝕊 s:
  v←⟨⟩
  {𝕊
    @=⊑s ? •Exit 0 ;
    '@'=⊑s ? v∾↩<"@" ⋄ s↩1↓s ;
    "~@"≡2↑s ? v∾↩<"~@" ⋄ s↩2↓s ;
    ⊑(⊑s)∊" ,"∾@+10 ? s↩1↓s ;
    ⊑(⊑s)∊"[]{}()'`~" ? v∾↩<≍⊑s ⋄ s↩1↓s ;
    ';'=⊑s ?
      o←⟨⟩
      {𝕊
        o ∾↩ ⊑s
        s ↩ 1↓s
        {0=≠s ? 0 ; (⊑s) ≠ @+10}
      } •_while_⊢1
      v∾↩<o ;
    """"≡s ? •Out "unbalanced quotes: """ ⋄ s↩⟨⟩;
    '"'=⊑s ?
      o‿p‿r ← """"‿1‿1
      {𝕊
        s ↩ 1↓s
        c ← ⊑s
        o ∾↩ c
        q ← p∧c='"'
        {p ? p↩c≠'\' ; p↩1}
        {q?@;r ↩ 1<≠s}
        r∧¬q
      } •_while_⊢1
      {r?@;•Out "unbalanced quotes: "∾o}
      v∾↩<o ⋄ s↩1↓s;
    o←⟨⟩
    {𝕊
      o ∾↩ ⊑s
      s ↩ 1↓s
      {0=≠s ? 0 ; ¬⊑(⊑s)∊"[]{}()'""`~,; "∾@+10}
    } •_while_⊢1
    v∾↩<o
  }•_while_{𝕊 ⋄ 0≠≠s}@
  v
}

# Thanks to Marshall for writing this version of readString and
# suhr for suggesting the `out` value of Ctrl-* characters! (My
# version was verbose functional code while this is array style.)
readString ← {
  in  ← "abefnrtv"
  out ← "GH[LJMIK"-64
  diff← (out-in)∾0
  {
    m ← » bs ← <`𝕩='\'
    (¬bs) / (diff⊏˜in⊸⊐)⊸+⌾(m⊸/) 𝕩
  }
}

Int ← {10⊸×⊸+˜´'0'-˜⌽𝕩}
nums ← "0123456789"
Atom ← {
  𝕊 "true": 7‿1 ;
  𝕊 "false": 7‿0 ;
  𝕊 "nil": nil ;
  𝕊 a: {'"': 2∾<ReadString 1↓¯1↓a ;
   ':': 6‿a ;
   ';': @ ;
   ⊑𝕩∊nums ? 1∾Int a ;
   (∧´(1↓a)∊nums)∧(𝕩='-')∧1<≠a ? 1∾-Int 1↓a ;
   0‿a }⊑a
}

ReadList ← {𝕊 r‿t‿e:
  l←⟨⟩
  {𝕊
    {@: t ↩ ¯1 ⋄ l ↩ "unbalanced parens" ⋄ 0;
     𝕩≡e ? r.Next@ ⋄ 0;
     v ← ReadForm r
     {v≡@ ? @; l∾↩<v}
     1
    }r.Peek@
  }•_while_⊢1
  t‿l
}

ReadForm ← {𝕊 r:
  {"(": ReadList r‿3‿")" ;
   "[": ReadList r‿4‿"]" ;
   "{": ReadList r‿5‿"}" ;
   "@": 3‿⟨0‿"deref", ReadForm r⟩;
   "'": 3‿⟨0‿"quote", ReadForm r⟩;
   "`": 3‿⟨0‿"quasiquote", ReadForm r⟩;
   "~": 3‿⟨0‿"unquote", ReadForm r⟩;
   "~@": 3‿⟨0‿"splice-unquote", ReadForm r⟩;
   Atom 𝕩
  }r.Next@
}

{
  Tokenize ⇐ T
  ReadStr ⇐ {
    v ← ReadForm Reader Tokenize 𝕩
    v≢@ ? v ; nil}
}

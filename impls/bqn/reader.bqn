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
  {
    0=≠s ? ⟨⟩ ;
    @=⊑s ? •Exit 0 ;
    "~@"≡2↑s ? (<"~@") ∾ T 2↓s ;
    ⊑(⊑s)∊" ,"∾@+10 ? T 1↓s ;
    ⊑(⊑s)∊"[]{}()'`~" ? (<≍⊑s) ∾ T 1↓s ;
    ';'=⊑s ?
      o←⟨⟩
      {𝕊
        o ∾↩ ⊑s
        s ↩ 1↓s
        {0=≠s ? 0 ; (⊑s) ≠ @+10}
      } •_while_⊢1
      (<o) ∾ T s ;
    """"≡s ? •Out "unbalanced quotes: """ ⋄ @;
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
      (<o) ∾ T 1↓s ;
    o←⟨⟩
    {𝕊
      o ∾↩ ⊑s
      s ↩ 1↓s
      {0=≠s ? 0 ; ¬⊑(⊑s)∊"[]{}()'""`~,; "∾@+10}
    } •_while_⊢1
    (<o) ∾ T s
  }
}

Int ← {10⊸×⊸+˜´'0'-˜⌽𝕩}
nums ← "0123456789"
Atom ← {
  𝕊 "true": 7‿1 ;
  𝕊 "false": 7‿0 ;
  𝕊 "nil": 8‿⟨⟩ ;
  𝕊 a: {'"': 2∾<¯1↓1↓a ;
   ':': 6‿a ;
   ⊑𝕩∊nums ? 1∾Int a ;
   (𝕩='-')∧1<≠a ? 1∾-Int 1↓a ;
   0‿a }⊑a
}

ReadList ← {𝕊 r‿t‿e:
  l←⟨⟩
  {𝕊
    {@: t ↩ ¯1 ⋄ l ↩ "unbalanced parens" ⋄ 0;
     𝕩≡e ? r.Next@ ⋄ 0;
     l ∾↩ <ReadForm r ⋄ 1
    }r.Peek@
  }•_while_⊢1
  t‿l
}

ReadForm ← {𝕊 r:
  {"(": ReadList r‿3‿")" ;
   "[": ReadList r‿4‿"]" ;
   "{": ReadList r‿5‿"}" ;
   Atom 𝕩
  }r.Next@
}

{
  Tokenize ⇐ T
  ReadStr ⇐ ReadForm∘Reader∘Tokenize
}

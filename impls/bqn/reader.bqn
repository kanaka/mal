Reader ← {𝕊 tokens:
  {
    ts ← tokens
    Peek ⇐ {𝕊 ⋄ ⊑ ts}
    Next ⇐ {𝕊
      t ← ⊑ts
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
        (⊑s) ≠ @+10
      } •_while_⊢1
      (<o) ∾ T s ;
    '"'=⊑s ?
      o‿p‿r ← """"‿1‿1
      {𝕊
        s ↩ 1↓s
        c ← ⊑s
        o ∾↩ c
        q ← p∧c='"'
        {p ? p↩c≠'\' ; p↩1}
        r ↩ 1<≠s
        r∧¬q
      } •_while_⊢1
      {r?@;•Out "unterminated string: "∾o}
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

# Types:
# 0 symbol
# 1 int
# 2 string
# 3 list
Atom ← {𝕊 a:
  {'"': 2∾<¯1↓1↓a ;
   ⊑𝕩∊"0123456789" ? 1∾10⊸×⊸+˜´'0'-˜⌽a ;
   0‿a }⊑a
}

ReadList ← {𝕊 r:
  l←⟨⟩
  {𝕊
    l ∾↩ <ReadForm r
    ")"≢r.Peek@
  }•_while_⊢1
  r.Next@
  3‿l
}

ReadForm ← {𝕊 r:
  {"(": ReadList r ;
   Atom 𝕩
  }r.Next@
}

{
  Tokenize ⇐ T
  ReadStr ⇐ ReadForm∘Reader∘Tokenize
}

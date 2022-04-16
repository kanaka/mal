Lst ← {0=≠𝕩 ? "" ; {𝕨∾" "∾𝕩}´(𝕨⊸P¨𝕩)}
P ← {
  r 𝕊 ¯1‿x: "ERROR: "∾x;
  r 𝕊 10‿x‿y‿z‿f: "#function";
  r 𝕊 9‿x: "#function";
  r 𝕊 8‿x: "nil";
  r 𝕊 7‿0: "false";
  r 𝕊 7‿1: "true";
  r 𝕊 6‿kw: kw;
  r 𝕊 5‿l: "{"∾(r Lst l)∾"}";
  r 𝕊 4‿l: "["∾(r Lst l)∾"]";
  r 𝕊 3‿l: "("∾(r Lst l)∾")";
  0 𝕊 2‿s: s;
  1 𝕊 2‿s:
    ∾´{
      𝕊 '"': "\""";
      𝕊 '
': "\n";
      𝕊 '\': "\\";
      𝕊 @: """";
      ""∾𝕩
    }¨(@∾s∾@);
  r 𝕊 1‿n: {n<0?"-";""}∾•Fmt |n;
  r 𝕊 a‿x: x ;
  "ERROR printing: "∾(@+10)∾•Fmt 𝕩
}

{PrStr  ⇐ 0⊸P
 PrStrR ⇐ 1⊸P}

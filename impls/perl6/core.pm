unit module core;
use types;
use printer;
use reader;

sub equal ($a, $b) {
  if $a ~~ MalSequence && $b ~~ MalSequence {
    return $FALSE if $a.elems != $b.elems;
    for |$a Z |$b -> ($a_el, $b_el) {
      return $FALSE if equal($a_el, $b_el) ~~ $FALSE;
    }
    return $TRUE;
  }
  elsif $a ~~ MalHashMap && $b ~~ MalHashMap {
    return $FALSE if $a.elems != $b.elems;
    for $a.pairs {
      return $FALSE if !$b{.key} || equal(.value, $b{.key}) ~~ $FALSE;
    }
    return $TRUE;
  }
  else {
    return $a.^name eq $b.^name && $a.val ~~ $b.val ?? $TRUE !! $FALSE;
  }
}

sub perl6-eval ($code) {
  my &convert = -> $data {
    given $data {
      when Array|List { MalList($_.map({&convert($_)}).Array) }
      when Hash { MalHashMap($_.map({.key => &convert(.value)}).Hash) }
      when Bool { $_ ?? $TRUE !! $FALSE }
      when Int { MalNumber($_) }
      when Nil { $NIL }
      default { $_.^name eq 'Any' ?? $NIL !! MalString($_.gist) }
    }
  };

  use MONKEY-SEE-NO-EVAL;
  return &convert(EVAL($code));
}

our %ns = (
  '+'         => MalCode({ MalNumber($^a.val + $^b.val) }),
  '-'         => MalCode({ MalNumber($^a.val - $^b.val) }),
  '*'         => MalCode({ MalNumber($^a.val * $^b.val) }),
  '/'         => MalCode({ MalNumber(($^a.val / $^b.val).Int) }),
  '<'         => MalCode({ $^a.val < $^b.val ?? $TRUE !! $FALSE }),
  '<='        => MalCode({ $^a.val <= $^b.val ?? $TRUE !! $FALSE }),
  '>'         => MalCode({ $^a.val > $^b.val ?? $TRUE !! $FALSE }),
  '>='        => MalCode({ $^a.val >= $^b.val ?? $TRUE !! $FALSE }),
  '='         => MalCode({ equal($^a, $^b) }),
  prn         => MalCode({ say @_.map({ pr_str($_, True) }).join(' '); $NIL }),
  println     => MalCode({ say @_.map({ pr_str($_) }).join(' '); $NIL }),
  pr-str      => MalCode({ MalString(@_.map({ pr_str($_, True) }).join(' ') ) }),
  str         => MalCode({ MalString(@_.map({ pr_str($_) }).join) }),
  read-string => MalCode({ read_str($^a.val) }),
  slurp       => MalCode({ MalString($^a.val.IO.slurp) }),
  list        => MalCode({ MalList(@_) }),
  'list?'     => MalCode({ $^a ~~ MalList ?? $TRUE !! $FALSE }),
  'empty?'    => MalCode({ $^a.elems ?? $FALSE !! $TRUE }),
  count       => MalCode({ MalNumber($^a ~~ $NIL ?? 0 !! $^a.elems) }),
  atom        => MalCode({ MalAtom($^a) }),
  'atom?'     => MalCode({ $^a ~~ MalAtom ?? $TRUE !! $FALSE }),
  deref       => MalCode({ $^a.val }),
  'reset!'    => MalCode({ $^a.val = $^b }),
  'swap!'     => MalCode(-> $atom, $func, *@args { $atom.val = $func.apply($atom.val, |@args) }),
  cons        => MalCode({ MalList([$^a, |$^b.val]) }),
  concat      => MalCode({ MalList([@_.map({|$_.val})]) }),
  vec         => MalCode({ MalVector([|$^a.val]) }),
  nth         => MalCode({ $^a[$^b.val] // die X::MalOutOfRange.new }),
  first       => MalCode({ $^a[0] // $NIL }),
  rest        => MalCode({ MalList([$^a[1..*]]) }),
  throw       => MalCode({ die X::MalThrow.new(value => $^a) }),
  apply       => MalCode(-> $func, *@args { $func.apply(|@args[0..*-2], |@args[*-1].val) }),
  map         => MalCode(-> $func, $list { MalList([$list.map({ $func.apply($_) })]) }),
  'nil?'      => MalCode({ $^a ~~ MalNil ?? $TRUE !! $FALSE }),
  'true?'     => MalCode({ $^a ~~ MalTrue ?? $TRUE !! $FALSE }),
  'false?'    => MalCode({ $^a ~~ MalFalse ?? $TRUE !! $FALSE }),
  'symbol?'   => MalCode({ $^a ~~ MalSymbol ?? $TRUE !! $FALSE }),
  symbol      => MalCode({ MalSymbol($^a.val) }),
  keyword     => MalCode({ $^a.val ~~ /^\x29E/ ?? $^a !! MalString("\x29E" ~ $^a.val) }),
  'keyword?'  => MalCode({ $^a.val ~~ /^\x29E/ ?? $TRUE !! $FALSE }),
  'number?'   => MalCode({ $^a ~~ MalNumber ?? $TRUE !! $FALSE }),
  'fn?'       => MalCode({ ($^a ~~ MalCallable && !$^a.?is_macro) ?? $TRUE !! $FALSE }),
  'macro?'    => MalCode({ $^a.?is_macro ?? $TRUE !! $FALSE }),
  vector      => MalCode({ MalVector(@_) }),
  'vector?'   => MalCode({ $^a ~~ MalVector ?? $TRUE !! $FALSE }),
  hash-map    => MalCode({ MalHashMap(@_.map({ $^a.val => $^b }).Hash) }),
  'map?'      => MalCode({ $^a ~~ MalHashMap ?? $TRUE !! $FALSE }),
  assoc       => MalCode(-> $map, *@kv { MalHashMap(Hash.new(|$map.kv, |@kv.map({$^a.val, $^b}))) }),
  dissoc      => MalCode(-> $map, *@keys { my %h = $map.val.clone; %h{@keys.map(*.val)}:delete; MalHashMap(%h) }),
  get         => MalCode({ $^a.val{$^b.val} // $NIL }),
  'contains?' => MalCode({ $^a.val{$^b.val}:exists ?? $TRUE !! $FALSE }),
  keys        => MalCode({ MalList([$^a.keys.map({ MalString($_) })]) }),
  vals        => MalCode({ MalList([$^a.values]) }),
  'sequential?' => MalCode({ $^a ~~ MalList|MalVector ?? $TRUE !! $FALSE }),
  readline    => MalCode({ with prompt($^a.val) { MalString($_) } else { $NIL } }),
  time-ms     => MalCode({ MalNumber((now * 1000).Int) }),
  conj        => MalCode(-> $seq, *@args { $seq.conj(@args) }),
  'string?'   => MalCode({ $^a ~~ MalString && $^a.val !~~ /^\x29E/ ?? $TRUE !! $FALSE }),
  seq         => MalCode({ $^a.seq }),
  with-meta   => MalCode({ return $NIL if !$^a.can('meta'); my $x = $^a.clone; $x.meta = $^b; $x }),
  meta        => MalCode({ $^a.?meta // $NIL }),
  perl6-eval  => MalCode({ perl6-eval($^a.val) }),
);

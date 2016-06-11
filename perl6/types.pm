unit module types;

class X::MalException is Exception is export {}
class X::MalNoTokens is X::MalException is export {
  method message() { "got no tokens" }
}
class X::MalIncomplete is X::MalException is export {
  has $.end;
  method message() { "expected '$.end', got EOF" }
}
class X::MalUnexpected is X::MalException is export {
  has $.token;
  method message() { "unexpected '$.token'" }
}
class X::MalNotFound is X::MalException is export {
  has $.name;
  method message() { "'$.name' not found" }
}
class X::MalOutOfRange is X::MalException is export {
  method message() { "nth: index out of range" }
}
class X::MalThrow is X::MalException is export {
  has $.value;
}

role MalValue is export {
  has $.val is rw;
  method CALL-ME ($val) { self.new(:$val) }
}
role MalSequence is export {
  has $.val handles <cache AT-POS EXISTS-POS elems end iterator>;
  has $.meta is rw;
  method CALL-ME ($val) { self.new(:$val) }
}
role MalCallable is export {
  has &.fn;
  method apply (*@_) { &!fn(|@_) }
}
role MalMeta is export {
  has $.meta is rw;
}

class MalNil does MalValue is export {
  method seq { self }
}
class MalTrue does MalValue is export {}
class MalFalse does MalValue is export {}

our $NIL is export = MalNil('nil');
our $TRUE is export = MalTrue('true');
our $FALSE is export = MalFalse('false');

class MalSymbol does MalValue does MalMeta is export {}

class MalList does MalSequence is export {
  method conj (@args) { return self.new(val => [|@args.reverse, |$.val]) }
  method seq { return self.elems ?? self !! $NIL }
}

class MalVector does MalSequence is export {
  method conj (@args) { return self.new(val => [|$.val, |@args]) }
  method seq { return self.elems ?? MalList(self.val) !! $NIL }
}

class MalHashMap does MalMeta is export {
  has $.val handles <cache AT-KEY EXISTS-KEY elems pairs keys values kv>;
  method CALL-ME ($val) { self.new(:$val) }
}

class MalNumber does MalValue is export {}

class MalString does MalValue is export {
  method seq {
    return self.val.chars
      ?? MalList(self.val.comb.map({MalString($_)}))
      !! $NIL;
  }
}

class MalCode does MalCallable does MalMeta is export {
  method CALL-ME (&fn) { self.new(:&fn) }
}

class MalFunction does MalCallable does MalMeta is export {
  has $.ast;
  has @.params;
  has $.env;
  has $.is_macro is rw = False;
  method CALL-ME ($ast, $env, @params, &fn) {
    self.bless(:$ast, :$env, :@params, :&fn);
  }
}

class MalAtom does MalValue does MalMeta is export {}

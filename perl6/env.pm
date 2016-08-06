unit class MalEnv;
use types;

has $.outer;
has %.data;
has @.binds;
has @.exprs;

method new ($outer?, @binds?, @exprs?) {
  self.bless(:$outer, :@binds, :@exprs);
}

submethod BUILD (:@!binds, :@!exprs, :$!outer, :%!data) {
  for @!binds.kv -> $idx, $key {
    if $key eq '&' {
      my $value = MalList([@!exprs[$idx..*]]);
      self.set(@!binds[$idx+1], $value);
      last;
    }
    my $value = @!exprs[$idx];
    self.set($key, $value);
  }
}

method set ($key, $value) {
  %.data{$key} = $value;
}

method find ($key) {
  return %.data{$key} ?? self !! $.outer && $.outer.find($key);
}

method get ($key) {
  my $env = self.find($key) or die X::MalNotFound.new(name => $key);
  return $env.data{$key};
}

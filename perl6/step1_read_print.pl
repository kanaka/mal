use v6;
use lib IO::Path.new($?FILE).dirname;
use reader;
use printer;
use types;

sub read ($str) {
  return read_str($str);
}

sub eval ($ast) {
  return $ast;
}

sub print ($exp) {
  return pr_str($exp, True);
}

sub rep ($str) {
  return print(eval(read($str)));
}

sub MAIN {
  while (my $line = prompt 'user> ').defined {
    say rep($line);
    CATCH {
      when X::MalException { .Str.say }
    }
  }
}

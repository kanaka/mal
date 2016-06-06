use v6;
#use Linenoise;

sub read ($str) {
  return $str;
}

sub eval ($ast) {
  return $ast;
}

sub print ($exp) {
  return $exp;
}

sub rep ($str) {
  return print(eval(read($str)));
}

sub MAIN {
  #while (my $line = linenoise('user> ')).defined {
  #  say rep($line);
  #}
  while (my $line = prompt 'user> ').defined {
    say rep($line);
  }
}

use v6;
use lib IO::Path.new($?FILE).dirname;
use reader;
use printer;
use types;

sub read ($str) {
  return read_str($str);
}

sub eval_ast ($ast, $env) {
  given $ast {
    when MalSymbol  { $env{$ast.val} || die X::MalNotFound.new(name => $ast.val) }
    when MalList    { MalList([$ast.map({ eval($_, $env) })]) }
    when MalVector  { MalVector([$ast.map({ eval($_, $env) })]) }
    when MalHashMap { MalHashMap($ast.kv.map({ $^a => eval($^b, $env) }).Hash) }
    default         { $ast // $NIL }
  }
}

sub eval ($ast, $env) {
  return eval_ast($ast, $env) if $ast !~~ MalList;
  return $ast if !$ast.elems;

  my ($func, @args) = eval_ast($ast, $env).val;
  my $arglist = MalList(@args);
  return $func.apply($arglist);
}

sub print ($exp) {
  return pr_str($exp, True);
}

my $repl_env;

sub rep ($str) {
  return print(eval(read($str), $repl_env));
}

sub MAIN {
  $repl_env<+> = MalCode({ MalNumber($^a[0].val + $^a[1].val) });
  $repl_env<-> = MalCode({ MalNumber($^a[0].val - $^a[1].val) });
  $repl_env<*> = MalCode({ MalNumber($^a[0].val * $^a[1].val) });
  $repl_env</> = MalCode({ MalNumber(($^a[0].val / $^a[1].val).Int) });

  while (my $line = prompt 'user> ').defined {
    say rep($line);
    CATCH {
      when X::MalException { .Str.say }
    }
  }
}

use v6;
use lib IO::Path.new($?FILE).dirname;
use reader;
use printer;
use types;
use env;

sub read ($str) {
  return read_str($str);
}

sub eval_ast ($ast, $env) {
  given $ast {
    when MalSymbol  { $env.get($ast.val) || die X::MalNotFound.new(name => $ast.val) }
    when MalList    { MalList([$ast.map({ eval($_, $env) })]) }
    when MalVector  { MalVector([$ast.map({ eval($_, $env) })]) }
    when MalHashMap { MalHashMap($ast.kv.map({ $^a => eval($^b, $env) }).Hash) }
    default         { $ast // $NIL }
  }
}

sub eval ($ast, $env) {
  return eval_ast($ast, $env) if $ast !~~ MalList;
  return $ast if !$ast.elems;

  my ($a0, $a1, $a2, $a3) = $ast.val;
  given $a0.val {
    when 'def!' {
      return $env.set($a1.val, eval($a2, $env));
    }
    when 'let*' {
      my $new_env = MalEnv.new($env);
      for |$a1.val -> $key, $value {
        $new_env.set($key.val, eval($value, $new_env));
      }
      return eval($a2, $new_env);
    }
    default {
      my ($func, @args) = eval_ast($ast, $env).val;
      return $func.apply(|@args);
    }
  }
}

sub print ($exp) {
  return pr_str($exp, True);
}

my $repl_env = MalEnv.new;

sub rep ($str) {
  return print(eval(read($str), $repl_env));
}

sub MAIN {
  $repl_env.set('+', MalCode({ MalNumber($^a.val + $^b.val) }));
  $repl_env.set('-', MalCode({ MalNumber($^a.val - $^b.val) }));
  $repl_env.set('*', MalCode({ MalNumber($^a.val * $^b.val) }));
  $repl_env.set('/', MalCode({ MalNumber(($^a.val / $^b.val).Int) }));

  while (my $line = prompt 'user> ').defined {
    say rep($line);
    CATCH {
      when X::MalException { .Str.say }
    }
  }
}

use v6;
use lib IO::Path.new($?FILE).dirname;
use reader;
use printer;
use types;
use env;
use core;

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

sub qqLoop ($ast) {
  my $acc = MalList([]);
  for |$ast.val.reverse -> $elt {
    if $elt ~~ MalList && $elt.elems == 2 && $elt[0] ~~ MalSymbol
      && $elt[0].val eq 'splice-unquote'
    {
      $acc = MalList([MalSymbol('concat'), $elt[1], $acc]);
    }
    else {
      $acc = MalList([MalSymbol('cons'), quasiquote($elt), $acc]);
    }
  }
  return $acc;
}

sub quasiquote ($ast) {
  given $ast {
    when MalList {
      if $ast.elems == 2 && $ast[0] ~~ MalSymbol && $ast[0].val eq 'unquote' {
        $ast[1]
      } else {
        qqLoop($ast);
      }
    }
    when MalVector            { MalList([MalSymbol('vec'), qqLoop($ast)]) }
    when MalSymbol|MalHashMap { MalList([MalSymbol('quote'), $ast]) }
    default                   { $ast }
  }
}

sub eval ($ast is copy, $env is copy) {
  loop {
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
        $env = $new_env;
        $ast = $a2;
      }
      when 'do' {
        eval_ast(MalList([$ast[1..*-2]]), $env);
        $ast = $ast[*-1];
      }
      when 'if' {
        if eval($a1, $env) ~~ MalNil|MalFalse {
          return $NIL if $a3 ~~ $NIL;
          $ast = $a3;
        }
        else {
          $ast = $a2;
        }
      }
      when 'fn*' {
        my @binds = $a1 ?? $a1.map(*.val) !! ();
        my &fn = -> *@args {
          eval($a2, MalEnv.new($env, @binds, @args));
        };
        return MalFunction($a2, $env, @binds, &fn);
      }
      when 'quote' { return $a1 }
      when 'quasiquoteexpand' { return quasiquote($a1) }
      when 'quasiquote' { $ast = quasiquote($a1) }
      default {
        my ($func, @args) = eval_ast($ast, $env).val;
        return $func.apply(|@args) if $func !~~ MalFunction;
        $ast = $func.ast;
        $env = MalEnv.new($func.env, $func.params, @args);
      }
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

sub MAIN ($source_file?, *@args) {
  $repl_env.set(.key, .value) for %core::ns;
  $repl_env.set('eval', MalCode({ eval($^a, $repl_env) }));
  $repl_env.set('*ARGV*', MalList([@args.map({ MalString($_) })]));
  rep(q{(def! not (fn* (a) (if a false true)))});
  rep(q{(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))});

  if ($source_file.defined) {
    rep("(load-file \"$source_file\")");
    exit;
  }

  while (my $line = prompt 'user> ').defined {
    say rep($line);
    CATCH {
      when X::MalException { .Str.say }
    }
  }
}

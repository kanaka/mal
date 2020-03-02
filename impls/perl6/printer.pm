unit module printer;
use types;

sub pr_str ($exp, $print_readably = False) is export {
  given $exp {
    when MalFunction { "#<fn* ({$exp.params}) {pr_str($exp.ast)}>" }
    when MalCode { "#<builtin_fn* {$exp.fn.gist}>" }
    when MalList {
      '(' ~ join(' ', |$exp.map({ pr_str($_, $print_readably) })) ~ ')';
    }
    when MalVector {
      '[' ~ join(' ', |$exp.map({ pr_str($_, $print_readably) })) ~ ']';
    }
    when MalHashMap {
      '{' ~ $exp.kv.flatmap({ MalString($^a), $^b }).map({ pr_str($_, $print_readably) }) ~ '}'
    }
    when MalString {
      my $str = $exp.val;
      if $str ~~ s/^\x29E/:/ || !$print_readably {
        $str;
      }
      else {
        '"' ~ $str.trans(/\\/ => '\\\\', /\"/ => '\\"', /\n/ => '\\n') ~ '"';
      }
    }
    when MalAtom { "(atom {pr_str($exp.val, $print_readably)})" }
    when MalValue { $exp.val }
  }
}

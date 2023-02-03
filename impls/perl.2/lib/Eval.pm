package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $ns) = @_;
    my $type = ref($ast);

    if ($type ne 'list') {
        eval_ast($ast, $ns);
    }
    elsif (@$ast eq 0) {
        $ast;
    }
    else {
        my $new = eval_ast($ast, $ns);
        my ($fn, @args) = @$new;
        $fn->(@args);
    }
}

sub eval_ast {
    my ($ast, $ns) = @_;

    my $type = ref($ast);

    if ($type eq 'list') {
        return bless [ map Eval::eval($_, $ns), @$ast ], $type;
    }
    elsif ($type eq 'symbol') {
        my $sym = $$ast;
        my $val = $ns->{$sym};
        defined $val or die;
        return $val;
    }
    elsif ($type eq 'number') {
        return $ast
    }
    else {
        $ast;
    }
}

1;

package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $env) = @_;
    my $type = ref($ast);

    if ($type eq 'list') {
        if (@$ast eq 0) {
            $ast;
        }
        else {
            my $sym = $ast->[0];
            if (ref($sym) eq 'symbol' and $$sym eq 'def!') {
                def($ast, $env);
            }
            elsif (ref($sym) eq 'symbol' and $$sym eq 'let*') {
                let($ast, $env);
            }
            else {
                my ($fn, @args) = @{eval_ast($ast, $env)};
                $fn->(@args);
            }
        }
    }
    else {
        eval_ast($ast, $env);
    }
}

sub eval_ast {
    my ($ast, $env) = @_;

    my $type = ref($ast);

    if ($type eq 'list' or $type eq 'vector') {
        return $type->new([ map Eval::eval($_, $env), @$ast ]);
    }
    if ($type eq 'hash_map') {
        return $type->new(map Eval::eval($_, $env), %$ast);
    }
    elsif ($type eq 'symbol') {
        my $sym = $$ast;
        my $val = $env->get($sym);
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

sub def {
    my ($ast, $env) = @_;
    my ($def, $sym, $val) = @$ast;
    $env->set($$sym, Eval::eval($val, $env));
}

sub let {
    my ($ast, $env) = @_;
    $env = Env->new(outer => $env);
    my ($let, $def, $eval) = @$ast;
    for (my $i = 0; $i < @$def; $i += 2) {
        my ($key, $val) = ($def->[$i], $def->[$i+1]);
        $env->set($$key, Eval::eval($val, $env));
    }
    Eval::eval($eval, $env);
}

1;

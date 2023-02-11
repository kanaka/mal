package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $env) = @_;

    while (1) {
        return Eval::eval_ast($ast, $env) unless $ast->isa('list');
        return $ast if not @$ast;
        my ($a0, $a1, $a2, $a3) = @$ast;
        my $sym = (ref($a0) eq 'symbol') ? $$a0 : '';
        if ('def!' eq $sym) {
            return $env->set($$a1, Eval::eval($a2, $env));
        } elsif ('do' eq $sym) {
            my (undef, @do) = @$ast;
            $ast = pop @do;
            eval_ast(list(\@do), $env);
        } elsif ('if' eq $sym) {
            $ast = ${boolean(Eval::eval($a1, $env))} ? $a2 :
                defined $a3 ? $a3 : nil;
        } elsif ('let*' eq $sym) {
            $env = Env->new(outer => $env);
            for (my $i = 0; $i < @$a1; $i += 2) {
                $env->set(${$a1->[$i]}, Eval::eval($a1->[$i+1], $env));
            }
            $ast = $a2;
        } elsif ('fn*' eq $sym) {
            return function($a1, $a2, $env);
        } else {
            my ($f, @args) = @{eval_ast($ast, $env)};
            return $f->(@args) if ref($f) eq 'CODE';
            ($ast, $env) = $f->(@args);
        }
    }
}

sub eval_ast {
    my ($ast, $env) = @_;

    if ($ast->isa('List')) {
        return ref($ast)->new([ map Eval::eval($_, $env), @$ast ]);
    }
    if ($ast->isa('Map')) {
        return ref($ast)->new([map Eval::eval($_, $env), %$ast]);
    }
    elsif ($ast->isa('symbol')) {
        $env->get($$ast);
    }
    else {
        return $ast;
    }
}

1;

package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $env) = @_;

    if (not $ast->isa('list')) {
        return eval_ast($ast, $env);
    }
    return $ast if not @$ast;
    my $sym = $ast->[0];
    my $is_sym = ref($sym) eq 'symbol';
    ($is_sym and $$sym eq 'def!') ?
        Eval::def($ast, $env) :
    ($is_sym and $$sym eq 'do') ?
        Eval::do($ast, $env) :
    ($is_sym and $$sym eq 'fn*') ?
        Eval::fn($ast, $env) :
    ($is_sym and $$sym eq 'if') ?
        Eval::if($ast, $env) :
    ($is_sym and $$sym eq 'let*') ?
        Eval::let($ast, $env) :
    do {
        my ($fn, @args) = @{eval_ast($ast, $env)};
        $fn->(@args);
    };
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

sub def {
    my ($ast, $env) = @_;
    my (undef, $sym, $val) = @$ast;
    $env->set($$sym, Eval::eval($val, $env));
}

sub do {
    my ($ast, $env) = @_;
    my (undef, @do) = @$ast;
    my $list = eval_ast(list(\@do), $env);
    pop @$list;
}

sub fn {
    my ($ast, $env) = @_;
    my (undef, $bind, $form) = @$ast;
    function(
        sub {
            $env = Env->new(
                outer => $env,
                binds => [@$bind],
                exprs => [@_],
            );
            Eval::eval($form, $env);
        }
    );
}

sub if {
    my ($ast, $env) = @_;
    my (undef, $cond, $then, $else) = @$ast;
    if (${boolean(Eval::eval($cond, $env))}) {
        Eval::eval($then, $env);
    }
    else {
        return nil unless defined $else;
        Eval::eval($else, $env);
    }
}

sub let {
    my ($ast, $env) = @_;
    $env = Env->new(outer => $env);
    my (undef, $def, $eval) = @$ast;
    for (my $i = 0; $i < @$def; $i += 2) {
        my ($key, $val) = ($def->[$i], $def->[$i+1]);
        $env->set($$key, Eval::eval($val, $env));
    }
    Eval::eval($eval, $env);
}

1;

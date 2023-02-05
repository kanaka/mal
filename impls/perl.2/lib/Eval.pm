package Eval;

use Mo;

use Types;

sub eval {
    my ($ast, $env) = @_;
    my $type = ref($ast);

    if ($type eq 'list') {
        return $ast if @$ast eq 0;
        my $sym = $ast->[0];
        (ref($sym) eq 'symbol' and $$sym eq 'def!') ?
            def($ast, $env) :
        (ref($sym) eq 'symbol' and $$sym eq 'do') ?
            Eval::do($ast, $env) :
        (ref($sym) eq 'symbol' and $$sym eq 'fn*') ?
            fn($ast, $env) :
        (ref($sym) eq 'symbol' and $$sym eq 'if') ?
            Eval::if($ast, $env) :
        (ref($sym) eq 'symbol' and $$sym eq 'let*') ?
            let($ast, $env) :
        do {
            my ($fn, @args) = @{eval_ast($ast, $env)};
            $fn->(@args);
        };
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
    else {
        return $ast;
    }
}

sub def {
    my ($ast, $env) = @_;
    my ($n, $sym, $val) = @$ast;
    $env->set($$sym, Eval::eval($val, $env));
}

sub do {
    my ($ast, $env) = @_;
    my $ret;
    for my $form (@{$ast}[1..(@$ast-1)]) {
        $ret = Eval::eval($form, $env);
    }
    return $ret;
}

sub fn {
    my ($ast, $env) = @_;
    my ($n, $bind, $form) = @$ast;
    sub {
        $env = Env->new(
            outer => $env,
            binds => [@$bind],
            exprs => [@_],
        );
        Eval::eval($form, $env);
    };
}

sub if {
    my ($ast, $env) = @_;
    my ($n, $cond, $then, $else) = @$ast;

    $cond = boolean->new(Eval::eval($cond, $env));

    if ("$cond" eq 'true') {
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
    my ($n, $def, $eval) = @$ast;
    for (my $i = 0; $i < @$def; $i += 2) {
        my ($key, $val) = ($def->[$i], $def->[$i+1]);
        $env->set($$key, Eval::eval($val, $env));
    }
    Eval::eval($eval, $env);
}

1;

use strict; use warnings;
package Eval;

use Types;

sub eval {
    my ($ast, $env) = @_;

    while (1) {
        return eval_ast($ast, $env) unless $ast->isa('list');
#         unless ($ast->isa('list')) {
#             $ast = eval_ast($ast, $env);
#             return $ast;
#         }

        $ast = macroexpand($ast, $env);

        return eval_ast($ast, $env) unless $ast->isa('list');
#         unless ($ast->isa('list')) {
#             $ast = eval_ast($ast, $env);
#             return $ast;
#         }

        return $ast unless @$ast;
#         unless (@$ast) {
#             return $ast;
#         }

        my ($a0, $a1, $a2, $a3) = @$ast;
        my $sym = (ref($a0) eq 'symbol') ? $$a0 : '';
        if ('def!' eq $sym) {
            return $env->set($$a1, Eval::eval($a2, $env));
#             $ast = Eval::eval($a2, $env);
#             $ast = $env->set($$a1, $ast);
#             return $ast;
        } elsif ('defmacro!' eq $sym) {
            return $env->set($$a1, macro(Eval::eval($a2, $env)));
#             $ast = Eval::eval($a2, $env);
#             $ast = macro($ast);
#             $ast = $env->set($$a1, $ast);
#             return $ast;
        } elsif ('do' eq $sym) {
            my (undef, @do) = @$ast;
            $ast = pop @do;
            eval_ast(list(\@do), $env);
#             $ast = $ast;
        } elsif ('fn*' eq $sym) {
            return function($a1, $a2, $env);
#             $ast = function($a1, $a2, $env);
#             return $ast;
        } elsif ('if' eq $sym) {
            $ast = ${boolean(Eval::eval($a1, $env))} ? $a2 :
                defined $a3 ? $a3 : nil;
#             $ast = Eval::eval($a1, $env);
#             $ast = boolean($ast);
#             $ast = $$ast;
#             if ($ast) {
#                 $ast = $a2;
#             } else {
#                 if (defined $a3) {
#                     $ast = $a3;
#                 } else {
#                     $ast = nil;
#                 }
#             }
#             $ast = $ast;
        } elsif ('let*' eq $sym) {
            $env = Env->new(outer => $env);
            for (my $i = 0; $i < @$a1; $i += 2) {
                $env->set(${$a1->[$i]}, Eval::eval($a1->[$i+1], $env));
            }
            $ast = $a2;
#             $ast = $ast;
        } elsif ('macroexpand' eq $sym) {
            return macroexpand($a1, $env);
#             $ast = macroexpand($a1, $env);
#             return $ast;
        } elsif ('quote' eq $sym) {
            return $a1;
#             $ast = $a1;
#             return $ast;
        } elsif ('quasiquote' eq $sym) {
            $ast = quasiquote($a1);
#             $ast = $ast
        } elsif ('quasiquoteexpand' eq $sym) {
            return quasiquote($a1);
#             $ast = quasiquote($a1);
#             return $ast;
        } elsif ('try*' eq $sym) {
            local $@;
            my $val = eval { Eval::eval($a1, $env) };
            return $val unless $@;
            my $err = $@;
            die ref($err) ? Printer::pr_str($err) : $err
                unless defined $a2;
            die "Invalid 'catch*' clause" unless
                $a2 and $a2->isa('List') and
                @$a2 and $a2->[0]->isa('symbol') and
                ${$a2->[0]} eq 'catch*';
            my $e;
            (undef, $e, $ast) = @$a2;
            if (not ref($err)) {
                chomp $err;
                $err = string($err);
            }
            $env = Env->new(
                outer => $env,
                binds => [$e],
                exprs => [$err],
            );
        } else {
            my ($f, @args) = @{eval_ast($ast, $env)};
            return $f->(@args) if ref($f) eq 'CODE';
#             if (ref($f) eq 'CODE') {
#                 $ast = $f->(@args);
#                 return $ast;
#             }
#             XXX [$f, @args] unless ref($f) eq 'function';
            ($ast, $env) = $f->(@args);
#             $ast = $ast;
        }
    }
}

sub eval_ast {
    my ($ast, $env) = @_;
#     $ast =
    $ast->isa('List') ? ref($ast)->new([ map Eval::eval($_, $env), @$ast ]) :
    $ast->isa('Map') ? ref($ast)->new([map Eval::eval($_, $env), %$ast]) :
    $ast->isa('symbol') ? $env->get($$ast) :
    $ast;
#     $ast;
}

sub macroexpand {
    my ($ast, $env) = @_;
    while (is_macro_call($ast, $env)) {
        my ($name, @args) = @$ast;
        $ast = Eval::eval($env->get($name)->(@args));
    }
    return $ast;
}

sub is_macro_call {
    my ($ast, $env) = @_;
    my $a0;
    (ref($ast) eq 'list' and
        ($a0) = @$ast and
        ref($a0) eq "symbol" and
        $env->find("$a0")
    ) ? ref($env->get("$a0")) eq 'macro' : 0;
}

sub quasiquote {
    my ($ast) = @_;
    return list([symbol('vec'), quasiquote_loop($ast)])
        if $ast->isa('vector');
    return list([symbol('quote'), $ast])
        if $ast->isa('Map') or $ast->isa('symbol');
    return $ast unless $ast->isa('list');
    my ($a0, $a1) = @$ast;
    return $a1 if $a0 and $a0->isa('symbol') and "$a0" eq 'unquote';
    return quasiquote_loop($ast);
}

sub quasiquote_loop {
    my ($ast) = @_;
    my $list = list([]);
    for my $elt (reverse @$ast) {
        if ($elt->isa('List') and
            $elt->[0] and
            $elt->[0]->isa('symbol') and
            "$elt->[0]" eq 'splice-unquote'
        ) {
            $list = list([symbol('concat'), $elt->[1], $list]);
        } else {
            $list = list([symbol('cons'), quasiquote($elt), $list]);
        }
    }
    return $list;
}

1;

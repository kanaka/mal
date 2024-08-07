<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';
require_once 'printer.php';
require_once 'env.php';
require_once 'core.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
function qq_loop($elt, $acc) {
    if (_list_Q($elt)
        and count($elt) == 2
        and _symbol_Q($elt[0])
        and $elt[0]->value === 'splice-unquote') {
        return _list(_symbol("concat"), $elt[1], $acc);
    } else {
        return _list(_symbol("cons"), quasiquote($elt), $acc);
    }
}

function qq_foldr($xs) {
    $acc = _list();
    for ($i=count($xs)-1; 0<=$i; $i-=1) {
        $acc = qq_loop($xs[$i], $acc);
    }
    return $acc;
}

function quasiquote($ast) {
    if (_vector_Q($ast)) {
        return _list(_symbol("vec"), qq_foldr($ast));
    } elseif (_symbol_Q($ast) or _hash_map_Q($ast)) {
        return _list(_symbol("quote"), $ast);
    } elseif (!_list_Q($ast)) {
        return $ast;
    } elseif (count($ast) == 2 and _symbol_Q($ast[0]) and $ast[0]->value === 'unquote') {
        return $ast[1];
    } else {
        return qq_foldr($ast);
    }
}

function MAL_EVAL($ast, $env) {
    while (true) {

    $dbgenv = $env->find("DEBUG-EVAL");
    if ($dbgenv) {
        $dbgeval = $env->get("DEBUG-EVAL");
        if ($dbgeval !== NULL && $dbgeval !== false) {
            echo "EVAL: " . _pr_str($ast) . "\n";
        }
    }

    if (_symbol_Q($ast)) {
        return $env->get($ast->value);
    } elseif (_vector_Q($ast)) {
            $el = _vector();
        foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
        return $el;
    } elseif (_hash_map_Q($ast)) {
        $new_hm = _hash_map();
        foreach (array_keys($ast->getArrayCopy()) as $key) {
            $new_hm[$key] = MAL_EVAL($ast[$key], $env);
        }
        return $new_hm;
    } elseif (!_list_Q($ast)) {
        return $ast;
    }

    // apply list
    if ($ast->count() === 0) {
        return $ast;
    }

    $a0 = $ast[0];
    $a0v = (_symbol_Q($a0) ? $a0->value : $a0);
    switch ($a0v) {
    case "def!":
        $res = MAL_EVAL($ast[2], $env);
        return $env->set($ast[1], $res);
    case "let*":
        $a1 = $ast[1];
        $let_env = new Env($env);
        for ($i=0; $i < count($a1); $i+=2) {
            $let_env->set($a1[$i], MAL_EVAL($a1[$i+1], $let_env));
        }
        $ast = $ast[2];
        $env = $let_env;
        break; // Continue loop (TCO)
    case "quote":
        return $ast[1];
    case "quasiquote":
        $ast = quasiquote($ast[1]);
        break; // Continue loop (TCO)
    case "defmacro!":
        $func = MAL_EVAL($ast[2], $env);
        $func = _function('MAL_EVAL', 'native', $func->ast, $func->env, $func->params);
        $func->ismacro = true;
        return $env->set($ast[1], $func);
    case "try*":
        $a1 = $ast[1];
        $a2 = $ast[2];
        if ($a2[0]->value === "catch*") {
            try {
                return MAL_EVAL($a1, $env);
            } catch (_Error $e) {
                $catch_env = new Env($env, array($a2[1]),
                                            array($e->obj));
                return MAL_EVAL($a2[2], $catch_env);
            } catch (Exception $e) {
                $catch_env = new Env($env, array($a2[1]),
                                            array($e->getMessage()));
                return MAL_EVAL($a2[2], $catch_env);
            }
        } else {
            return MAL_EVAL($a1, $env);
        }
    case "do":
        foreach ($ast->slice(1, -1) as $a) { MAL_EVAL($a, $env); }
        $ast = $ast[count($ast)-1];
        break; // Continue loop (TCO)
    case "if":
        $cond = MAL_EVAL($ast[1], $env);
        if ($cond === NULL || $cond === false) {
            if (count($ast) === 4) { $ast = $ast[3]; }
            else                   { $ast = NULL; }
        } else {
            $ast = $ast[2];
        }
        break; // Continue loop (TCO)
    case "fn*":
        return _function('MAL_EVAL', 'native',
                         $ast[2], $env, $ast[1]);
    default:
        $f = MAL_EVAL($a0, $env);
        $unevaluated_args = array_slice($ast->getArrayCopy(), 1);
        if ($f->ismacro) {
            $ast = $f->apply($unevaluated_args);
            break; // Continue loop (TCO)
        }
        $args = [];
        foreach ($unevaluated_args as $a) { $args[] = MAL_EVAL($a, $env); }
        if ($f->type === 'native') {
            $ast = $f->ast;
            $env = $f->gen_env($args);
            // Continue loop (TCO)
        } else {
            return $f->apply($args);
        }
    }

    }
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True);
}

// repl
$repl_env = new Env(NULL);
function rep($str) {
    global $repl_env;
    return MAL_PRINT(MAL_EVAL(READ($str), $repl_env));
}

// core.php: defined using PHP
foreach ($core_ns as $k=>$v) {
    $repl_env->set(_symbol($k), _function($v));
}
$repl_env->set(_symbol('eval'), _function(function($ast) {
    global $repl_env; return MAL_EVAL($ast, $repl_env);
}));
$_argv = _list();
for ($i=2; $i < count($argv); $i++) {
    $_argv->append($argv[$i]);
}
$repl_env->set(_symbol('*ARGV*'), $_argv);

// core.mal: defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");

if (count($argv) > 1) {
    rep('(load-file "' . $argv[1] . '")');
    exit(0);
}

// repl loop
do {
    try {
        $line = mal_readline("user> ");
        if ($line === NULL) { break; }
        if ($line !== "") {
            print(rep($line) . "\n");
        }
    } catch (BlankException $e) {
        continue;
    } catch (_Error $e) {
        echo "Error: " . _pr_str($e->obj, True) . "\n";
    } catch (Exception $e) {
        echo "Error: " . $e->getMessage() . "\n";
        echo $e->getTraceAsString() . "\n";
    }
} while (true);

?>

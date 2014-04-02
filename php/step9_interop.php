<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
function is_pair($x) {
    return sequential_Q($x) and count($x) > 0;
}

function quasiquote($ast) {
    if (!is_pair($ast)) {
        return new_list(new_symbol("quote"), $ast);
    } elseif (symbol_Q($ast[0]) && $ast[0]->value === 'unquote') {
        return $ast[1];
    } elseif (is_pair($ast[0]) && symbol_Q($ast[0][0]) &&
              $ast[0][0]->value === 'splice-unquote') {
        return new_list(new_symbol("concat"), $ast[0][1],
                        quasiquote($ast->slice(1)));
    } else {
        return new_list(new_symbol("cons"), quasiquote($ast[0]),
                        quasiquote($ast->slice(1)));
    }
}

function is_macro_call($ast, $env) {
    return is_pair($ast) &&
           symbol_Q($ast[0]) &&
           $env->find($ast[0]->value) &&
           $env->get($ast[0]->value)->ismacro;
}

function macroexpand($ast, $env) {
    while (is_macro_call($ast, $env)) {
        $mac = $env->get($ast[0]->value);
        $args = array_slice($ast->getArrayCopy(),1);
        $ast = $mac->apply($args);
    }
    return $ast;
}

function eval_ast($ast, $env) {
    if (symbol_Q($ast)) {
        return $env->get($ast->value);
    } elseif (list_Q($ast) || vector_Q($ast)) {
        if (list_Q($ast)) {
            $el = new_list();
        } else {
            $el = new_vector();
        }
        foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
        return $el;
    } elseif (hash_map_Q($ast)) {
        $new_hm = new_hash_map();
        foreach (array_keys($ast->getArrayCopy()) as $key) {
            $new_hm[$key] = MAL_EVAL($ast[$key], $env);
        }
        return $new_hm;
    } else {
        return $ast;
    }
}

function MAL_EVAL($ast, $env) {
    while (true) {
        #echo "MAL_EVAL: " . _pr_str($ast) . "\n";
        if (!list_Q($ast)) {
            return eval_ast($ast, $env);
        }

        // apply list
        $ast = macroexpand($ast, $env);
        if (!list_Q($ast)) { return $ast; }

        $a0 = $ast[0];
        $a0v = (symbol_Q($a0) ? $a0->value : $a0);
        switch ($a0v) {
        case "def!":
            $res = MAL_EVAL($ast[2], $env);
            return $env->set($ast[1]->value, $res);
        case "let*":
            $a1 = $ast[1];
            $let_env = new Env($env);
            for ($i=0; $i < count($a1); $i+=2) {
                $let_env->set($a1[$i]->value, MAL_EVAL($a1[$i+1], $let_env));
            }
            return MAL_EVAL($ast[2], $let_env);
        case "quote":
            return $ast[1];
        case "quasiquote":
            return MAL_EVAL(quasiquote($ast[1]), $env);
        case "defmacro!":
            $func = MAL_EVAL($ast[2], $env);
            $func->ismacro = true;
            return $env->set($ast[1]->value, $func);
        case "macroexpand":
            return macroexpand($ast[1], $env);
        case "php*":
            return eval($ast[1]);
        case "do":
            eval_ast($ast->slice(1, -1), $env);
            $ast = $ast[count($ast)-1];
            break;
        case "if":
            $cond = MAL_EVAL($ast[1], $env);
            if ($cond === NULL || $cond === false) {
                if (count($ast) === 4) { $ast = $ast[3]; }
                else                   { $ast = NULL; }
            } else {
                $ast = $ast[2];
            }
            break;
        case "fn*":
            return new_function('MAL_EVAL', 'native',
                                new_hash_map('exp', $ast[2],
                                             'env', $env,
                                             'params', $ast[1]));
        default:
            $el = eval_ast($ast, $env);
            $f = $el[0];
            $args = array_slice($el->getArrayCopy(), 1);
            if ($f->type === 'native') {
                $ast = $f->meta['exp'];
                $env = new Env($f->meta['env'], $f->meta['params'], $args);
            } else {
                return $f->apply($args);
            }
        }
    }
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True) . "\n";
}

// repl
$repl_env = new Env(NULL);
function rep($str) {
    global $repl_env;
    return MAL_PRINT(MAL_EVAL(READ($str), $repl_env));
}
function _ref($k, $v) {
    global $repl_env;
    $repl_env->set($k, new_function($v));
}
// Import types functions
foreach ($types_ns as $k=>$v) { _ref($k, $v); }

_ref('read-string', 'read_str');
_ref('eval', function($ast) {
    global $repl_env; return MAL_EVAL($ast, $repl_env);
});
_ref('slurp', function($f) {
    return file_get_contents($f);
});

// Defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");

if (count($argv) > 1) {
    for ($i=1; $i < count($argv); $i++) {
        rep('(load-file "' . $argv[$i] . '")');
    }
} else {
    do {
        try {
            $line = mal_readline("user> ");
            if ($line === NULL) { break; }
            if ($line !== "") {
                print(rep($line));
            }
        } catch (BlankException $e) {
            continue;
        } catch (Exception $e) {
            echo "Error: " . $e->getMessage() . "\n";
            echo $e->getTraceAsString() . "\n";
        }
    } while (true);
}

?> 

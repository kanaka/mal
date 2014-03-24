<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
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
    #echo "MAL_EVAL: " . _pr_str($ast) . "\n";
    if (!list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    // apply list
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
    case "do":
        #$el = eval_ast(array_slice($ast->getArrayCopy(), 1), $env);
        $el = eval_ast($ast->slice(1), $env);
        return $el[count($el)-1];
    case "if":
        $cond = MAL_EVAL($ast[1], $env);
        if ($cond === NULL || $cond === false) {
            if (count($ast) === 4) { return MAL_EVAL($ast[3], $env); }
            else                   { return NULL; }
        } else {
            return MAL_EVAL($ast[2], $env);
        }
    case "fn*":
        return function() use ($env, $ast) {
            $fn_env = new Env($env, $ast[1], func_get_args());
            return MAL_EVAL($ast[2], $fn_env);
        };
    default:
        $el = eval_ast($ast, $env);
        $f = $el[0];
        return call_user_func_array($f, array_slice($el->getArrayCopy(), 1));
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
function _ref($k, $v) { global $repl_env; $repl_env->set($k, $v); }
// Import types functions
foreach ($types_ns as $k=>$v) { _ref($k, $v); }

// Defined using the language itself
rep("(def! not (fn* (a) (if a false true)))");

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

?> 

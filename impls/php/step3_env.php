<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';
require_once 'printer.php';
require_once 'env.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
function MAL_EVAL($ast, $env) {
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

    if ($ast->count() === 0) {
        return $ast;
    }

    // apply list
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
        return MAL_EVAL($ast[2], $let_env);
    default:
        $el = [];
        foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
        $f = $el[0];
        $args = array_slice($el, 1);
        return call_user_func_array($f, $args);
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

$repl_env->set(_symbol('+'), function ($a, $b) { return intval($a + $b,10); });
$repl_env->set(_symbol('-'), function ($a, $b) { return intval($a - $b,10); });
$repl_env->set(_symbol('*'), function ($a, $b) { return intval($a * $b,10); });
$repl_env->set(_symbol('/'), function ($a, $b) { return intval($a / $b,10); });

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
    } catch (Exception $e) {
        echo "Error: " . $e->getMessage() . "\n";
        echo $e->getTraceAsString() . "\n";
    }
} while (true);

?>

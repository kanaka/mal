<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';
require_once 'printer.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
function MAL_EVAL($ast, $env) {
    // echo "EVAL: " . _pr_str($ast) . "\n";

    if (_symbol_Q($ast)) {
        return $env[$ast->value];
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
    $el = [];
    foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
    $f = $el[0];
    $args = array_slice($el, 1);
    return call_user_func_array($f, $args);
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True);
}

// repl
$repl_env = array();
function rep($str) {
    global $repl_env;
    return MAL_PRINT(MAL_EVAL(READ($str), $repl_env));
}

$repl_env['+'] = function ($a, $b) { return intval($a + $b,10); };
$repl_env['-'] = function ($a, $b) { return intval($a - $b,10); };
$repl_env['*'] = function ($a, $b) { return intval($a * $b,10); };
$repl_env['/'] = function ($a, $b) { return intval($a / $b,10); };

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

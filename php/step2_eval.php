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
        return $env[$ast->value];
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
    if (!list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    // apply list
    $el = eval_ast($ast, $env);
    $f = $el[0];
    return call_user_func_array($f, array_slice($el->getArrayCopy(), 1));
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True) . "\n";
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

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
function eval_ast($ast, $env) {
    if (_symbol_Q($ast)) {
        return $env[$ast->value];
    } elseif (_sequential_Q($ast)) {
        if (_list_Q($ast)) {
            $el = _list();
        } else {
            $el = _vector();
        }
        foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
        return $el;
    } elseif (_hash_map_Q($ast)) {
        $new_hm = _hash_map();
        foreach (array_keys($ast->getArrayCopy()) as $key) {
            $new_hm[$key] = MAL_EVAL($ast[$key], $env);
        }
        return $new_hm;
    } else {
        return $ast;
    }
}

function MAL_EVAL($ast, $env) {
    if (!_list_Q($ast)) {
        return eval_ast($ast, $env);
    }
    if ($ast->count() === 0) {
        return $ast;
    }

    // apply list
    $el = eval_ast($ast, $env);
    $f = $el[0];
    return call_user_func_array($f, array_slice($el->getArrayCopy(), 1));
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

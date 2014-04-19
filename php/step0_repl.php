<?php

require_once 'readline.php';

// read
function READ($str) {
    return $str;
}

// eval
function MAL_EVAL($ast, $env) {
    return eval($ast);
}

// print
function MAL_PRINT($exp) {
    return var_export($exp, true) . "\n";
}

// repl
function rep($str) {
    return MAL_PRINT(MAL_EVAL(READ($str), array()));
}

// repl loop
do {
    $line = mal_readline("user> ");
    if ($line === NULL) { break; }
    if (!empty($line)) {
        print(rep($line));
    }
} while (true);

?>

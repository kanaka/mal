<?php

require_once 'readline.php';

// read
function READ($str) {
    return $str;
}

// eval
function MAL_EVAL($ast, $env) {
    return $ast;
}

// print
function MAL_PRINT($exp) {
    return $exp;
}

// repl
function rep($str) {
    return MAL_PRINT(MAL_EVAL(READ($str), array()));
}

// repl loop
do {
    $line = mal_readline("user> ");
    if ($line === NULL) { break; }
    if ($line !== "") {
        print(rep($line) . "\n");
    }
} while (true);

?>

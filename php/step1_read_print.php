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
    return $ast;
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True) . "\n";
}

// repl
function rep($str) {
    return MAL_PRINT(MAL_EVAL(READ($str), array()));
}

// repl loop
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

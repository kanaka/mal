<?php

$HISTORY_FILE = $_SERVER['HOME'] . "/.mal-history";

function mal_readline($prompt) {
    global $HISTORY_FILE;
    static $history_loaded = false;

    // Load the history file
    if (! $history_loaded) {
        $history_loaded = true;
        if (is_readable($HISTORY_FILE)) {
            if ($file = fopen($HISTORY_FILE, "r")) {
                while (!feof($file)) {
                    $line = fgets($file);
                    if ($line) { readline_add_history($line); }
                }
                fclose($file);
            }
        }
    }

    $line = readline($prompt);
    if ($line === false) { return NULL; }
    readline_add_history($line);

    // Append to the history file
    if (is_writable($HISTORY_FILE)) {
        if ($file = fopen($HISTORY_FILE, "a")) {
            fputs($file, $line . "\n");
            fclose($file);
        }
    }

    return $line;
}

?>

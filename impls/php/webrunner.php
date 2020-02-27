<?php
// if we're called in a webserver context, auto-resolve to mal file
if (php_sapi_name() != "cli") {
    $malfile = str_replace(".php", ".mal", $_SERVER['SCRIPT_FILENAME']);
    rep('(load-file "' . $malfile . '")');
    exit(0);
}
?>

#!/usr/bin/env hhvm -c .hhvmconfig.hdf

namespace Mal\Step8;

use namespace Mal;

require_once(__DIR__.'/../vendor/autoload.hack');

<<__EntryPoint>>
async function main_async(): Awaitable<void> {
  AutoloadMap\initialize();

  $environment = Mal\repl_environment();

  $prelude = '(do'.
    '  (def! not (fn* (a) (if a false true)))'.
    '  (def! load-file (fn* (f) '.
    '    (eval (read-string (str "(do " (slurp f) "\nnil)")))))'.
    '  (def! swap! (fn* (a f & rest) (reset! a (apply f (deref a) rest))))'.
    '  (def! first (fn* (l) (if (sequential? l) (if (empty? l) nil (nth l 0)) nil)))'.
    '  (defmacro! cond (fn* (& xs)'.
    '    (if (> (count xs) 0)'.
    "      (list 'if (first xs)".
    "        (if (> (count xs) 1)".
    "          (nth xs 1)".
    "          (throw \"odd number of forms to cond\"))".
    "        (cons 'cond (rest (rest xs)))))))".
    ')';
  rep($prelude, $environment);

  /* HH_IGNORE_ERROR[2050] */
  $argv = $GLOBALS['argv'];
  $argv_list = Vec\drop($argv, 2)
    |> Vec\map($$, $arg ==> '"'.$arg.'"')
    |> Str\join($$, ' ');
  rep('(def! *ARGV* (list '.$argv_list.'))', $environment);
  if (C\count($argv) > 1) {
    $file_name = $argv[1];
    rep('(load-file "'.$file_name.'")', $environment);
    return;
  }

  $cli_input = IO\request_input();
  while (true) {
    // Handles CTRL+D
    if ($cli_input->isEndOfFile()) {
      return;
    }
    echo "user> ";
    try {
      // HHAST_IGNORE_ERROR[DontAwaitInALoop]
      echo rep(await $cli_input->readLineAsync(), $environment)."\n";
    } catch (\Error $e) {
      echo $e->getMessage()."\n";
    }
  }
}

function rep(string $input, Mal\Environment $environment): string {
  return print_out(evaluate(read($input), $environment));
}

// READ
function read(string $input): Mal\Form {
  return Mal\read_str($input);
}

// EVAL
function evaluate(Mal\Form $ast, Mal\Environment $environment): Mal\Form {
  return Mal\evaluate($ast, $environment);
}

// PRINT
function print_out(Mal\Form $ast): string {
  return Mal\pr_str($ast, true);
}

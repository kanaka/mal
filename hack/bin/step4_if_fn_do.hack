#!/usr/bin/env hhvm -c .hhvmconfig.hdf

namespace Mal\Step4;

use namespace Mal;

require_once(__DIR__.'/../vendor/autoload.hack');

<<__EntryPoint>>
async function main_async(): Awaitable<void> {
  AutoloadMap\initialize();

  $environment = Mal\repl_environment();

  $prelude = "(def! not (fn* (a) (if a false true)))";
  rep($prelude, $environment);

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

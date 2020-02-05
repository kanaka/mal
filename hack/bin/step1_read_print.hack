#!/usr/bin/env hhvm -c .hhvmconfig.hdf

namespace Mal\Step1;

use namespace Mal;

require_once(__DIR__.'/../vendor/autoload.hack');

<<__EntryPoint>>
async function main_async(): Awaitable<void> {
  AutoloadMap\initialize();

  $cli_input = IO\request_input();
  while (true) {
    // Handles CTRL+D
    if ($cli_input->isEndOfFile()) {
      return;
    }
    echo "user> ";
    try {
      // HHAST_IGNORE_ERROR[DontAwaitInALoop]
      echo rep(await $cli_input->readLineAsync())."\n";
    } catch (\Error $e) {
      echo $e->getMessage()."\n";
    }
  }
}


function rep(string $input): string {
  return print_out(evaluate(read($input)));
}

// READ
function read(string $input): Mal\Form {
  return Mal\read_str($input);
}

// EVAL
function evaluate(Mal\Form $ast): Mal\Form {
  return $ast;
}

// PRINT
function print_out(Mal\Form $ast): string {
  return Mal\pr_str($ast);
}

#!/usr/bin/env hhvm -c .hhvmconfig.hdf

namespace Mal\Step0;

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
    // HHAST_IGNORE_ERROR[DontAwaitInALoop]
    echo rep(await $cli_input->readLineAsync());
  }
}


function rep(string $input): string {
  return print_out(evaluate(read($input)));
}

// READ
function read(string $input): string {
  return $input;
}

// EVAL
function evaluate(string $input): string {
  return $input;
}

// PRINT
function print_out(string $input): string {
  return $input;
}

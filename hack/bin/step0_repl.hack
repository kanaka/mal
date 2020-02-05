#!/usr/bin/env hhvm -c .hhvmconfig.hdf

namespace Mal\Step0;

require_once(__DIR__.'/../vendor/autoload.hack');

<<__EntryPoint>>
async function main_async(): Awaitable<void> {
  AutoloadMap\initialize();
}

#!/usr/bin/env node
const [bin, ...args] = process.argv.slice(2);
const { spawn } = require('child_process');
const options = { stdio: [process.stdin, 'pipe', 'pipe'] };
const sclang = spawn(bin, args, options);

let seenBanner = false;
const canary = 'REAL OUTPUT HERE\n';
const trailingGarbage = 'cleaning up OSC';

sclang.stdout.on('data', (data) => {
  const chunk = data.toString();
  if (seenBanner && chunk.indexOf(trailingGarbage) < 0) {
    process.stdout.write(chunk);
  } else {
    if (chunk.indexOf(canary) > -1) {
      seenBanner = true;
      process.stdout.write(chunk.replace(canary, ''));
    }
  }
});

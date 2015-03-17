# IMPORTANT: choose one
RL_LIB = "libreadline"  # NOTE: libreadline is GPL
#RL_LIB = "libedit"

HISTORY_FILE = require('path').join(process.env.HOME, '.mal-history')

rlwrap = {} # namespace for this module in web context

ffi = require('ffi')
fs = require('fs')

rllib = ffi.Library(RL_LIB, {
    'readline': ['string', ['string']],
    'add_history': ['int', ['string']]})

rl_history_loaded = false

exports.readline = rlwrap.readline = (prompt = 'user> ') ->
  if !rl_history_loaded
    rl_history_loaded = true
    lines = []
    if fs.existsSync(HISTORY_FILE)
        lines = fs.readFileSync(HISTORY_FILE).toString().split("\n");

    # Max of 2000 lines
    lines = lines[Math.max(lines.length - 2000, 0)..]
    rllib.add_history(line) for line in lines when line != ""

  line = rllib.readline prompt
  if line
    rllib.add_history line
    try
      fs.appendFileSync HISTORY_FILE, line + "\n"
    catch exc
      true

  line

# vim: ts=2:sw=2

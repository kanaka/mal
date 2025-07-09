# IMPORTANT: choose one
RL_LIB = "libreadline.so.8"  # NOTE: libreadline is GPL
#RL_LIB = "libedit.so.2"

HISTORY_FILE = require('path').join(process.env.HOME, '.mal-history')

rlwrap = {} # namespace for this module in web context

koffi = require('koffi')
fs = require('fs')

rllib = null
try
  rllib = koffi.load(RL_LIB)
catch e
  console.error 'ERROR loading RL_LIB:', RL_LIB, e
  throw e

readlineFunc = rllib.func('char *readline(char *)')
addHistoryFunc = rllib.func('int add_history(char *)')

rl_history_loaded = false

exports.readline = rlwrap.readline = (prompt = 'user> ') ->
  if !rl_history_loaded
    rl_history_loaded = true
    lines = []
    if fs.existsSync(HISTORY_FILE)
      lines = fs.readFileSync(HISTORY_FILE).toString().split("\n")
    # Max of 2000 lines
    lines = lines[Math.max(lines.length - 2000, 0)..]
    for line in lines when line != ""
      addHistoryFunc line

  line = readlineFunc prompt
  if line
    addHistoryFunc line
    try
      fs.appendFileSync HISTORY_FILE, line + "\n"
    catch exc
      # ignored
      true

  line

# vim: ts=2:sw=2

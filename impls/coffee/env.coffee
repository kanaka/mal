types = require "./types.coffee"

# Env
exports.Env = class Env
  constructor: (@outer=null, @binds=[], @exprs=[]) ->
    @data = {}
    if @binds.length > 0
      for b,i in @binds
        if types._symbol_Q(b) && b.name == "&"
          @data[@binds[i+1].name] = @exprs[i..]
          break
        else
          @data[b.name] = @exprs[i]
  find: (key) ->
    if key of @data then @
    else if @outer then @outer.find(key)
    else null
  set: (key, value) ->
    if not types._symbol_Q(key)
      throw new Error("env.set key must be symbol")
    @data[key.name] = value
  get: (key) ->
    env = @find(key)
    throw new Error("'" + key + "' not found") if !env
    env.data[key]

# vim: ts=2:sw=2

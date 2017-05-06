export class Env
    (outer = null, data = {}) ->
        @outer = outer
        @data = data

    set: (symbol, ast) ->
        @data[symbol] = ast

    find: (symbol) ->
        if symbol of @data then @
        else if @outer? then @outer.find symbol

    get: (symbol) ->
        env = @find symbol
        if env then env.data[symbol]
        else throw new Error "symbol not found: #{symbol}"

export class Env
    (outer = null, data = {}) ->
        @outer = outer
        @data = data

    set: (symbol, ast) ->
        @data[symbol] = ast

    get: (symbol) ->
        if symbol of @data then @data[symbol]
        else if @outer? then @outer.get symbol

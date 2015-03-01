# We are going to use prototypical inheritance to implement environments.
export create-env = (outer-scope = null) ->
    if outer-scope
        Object.create outer-scope
    else
        {}

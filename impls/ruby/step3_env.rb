require_relative "mal_readline"
require_relative "types"
require_relative "reader"
require_relative "printer"
require_relative "env"

# read
def READ(str)
    return read_str(str)
end

# eval
def EVAL(ast, env)
    if env.get_or_nil(:"DEBUG-EVAL")
        puts "EVAL: #{_pr_str(ast, true)}"
    end

    case ast
    in Symbol
            return env.get(ast)
    in Vector
            return Vector.new ast.map{|a| EVAL(a, env)}
    in Hash
            new_hm = {}
            ast.each{|k,v| new_hm[k] = EVAL(v, env)}
            return new_hm

    # apply list

    in :def!, a1, a2
        return env.set(a1, EVAL(a2, env))
    in :"let*", a1, a2
        let_env = Env.new(env)
        a1.each_slice(2) do |a,e|
            let_env.set(a, EVAL(e, let_env))
        end
        return EVAL(a2, let_env)
    in [a0, *]
        f = EVAL(a0, env)
        args = ast.drop(1)
        return f[*args.map{|a| EVAL(a, env)}]

    else                        # Empty list or scalar
      return ast
    end
end

# print
def PRINT(exp)
    return _pr_str(exp, true)
end

# repl
repl_env = Env.new
REP = lambda {|str| PRINT(EVAL(READ(str), repl_env)) }

repl_env.set(:+, lambda {|a,b| a + b})
repl_env.set(:-, lambda {|a,b| a - b})
repl_env.set(:*, lambda {|a,b| a * b})
repl_env.set(:/, lambda {|a,b| a / b})

# repl loop
while line = _readline("user> ")
    begin
        puts REP[line]
    rescue Exception => e
        puts "Error: #{e}" 
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end

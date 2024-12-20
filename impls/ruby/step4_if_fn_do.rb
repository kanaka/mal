require_relative "mal_readline"
require_relative "types"
require_relative "reader"
require_relative "printer"
require_relative "env"
require_relative "core"

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
    in [:do, *]
        ast[1..-2].map{|a| EVAL(a, env)}
        return EVAL(ast.last, env)
    in [:if, a1, a2, *]
        cond = EVAL(a1, env)
        if cond
            return EVAL(a2, env)
        else
            return EVAL(ast[3], env)
        end
    in :"fn*", a1, a2
        return lambda {|*args|
            EVAL(a2, Env.new(env, a1, List.new(args)))
        }
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
RE = lambda {|str| EVAL(READ(str), repl_env) }
REP = lambda {|str| PRINT(EVAL(READ(str), repl_env)) }

# core.rb: defined using ruby
$core_ns.each do |k,v| repl_env.set(k,v) end

# core.mal: defined using the language itself
RE["(def! not (fn* (a) (if a false true)))"]

# repl loop
while line = _readline("user> ")
    begin
        puts REP[line]
    rescue Exception => e
        puts "Error: #{e}" 
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end

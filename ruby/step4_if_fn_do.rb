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
def eval_ast(ast, env)
    return case ast
        when Symbol
            env.get(ast)
        when List   
            List.new ast.map{|a| EVAL(a, env)}
        when Vector
            Vector.new ast.map{|a| EVAL(a, env)}
        when Hash
            new_hm = {}
            ast.each{|k,v| new_hm[EVAL(k,env)] = EVAL(v, env)}
            new_hm
        else 
            ast
    end
end

def EVAL(ast, env)
    #puts "EVAL: #{_pr_str(ast, true)}"

    if not ast.is_a? List
        return eval_ast(ast, env)
    end

    # apply list
    a0,a1,a2,a3 = ast
    case a0
    when :def!
        return env.set(a1, EVAL(a2, env))
    when :"let*"
        let_env = Env.new(env)
        a1.each_slice(2) do |a,e|
            let_env.set(a, EVAL(e, let_env))
        end
        return EVAL(a2, let_env)
    when :do
        el = eval_ast(ast.drop(1), env)
        return el.last
    when :if
        cond = EVAL(a1, env)
        if not cond
            return nil if a3 == nil
            return EVAL(a3, env)
        else
            return EVAL(a2, env)
        end
    when :"fn*"
        return lambda {|*args|
            EVAL(a2, Env.new(env, a1, args))
        }
    else
        el = eval_ast(ast, env)
        f = el[0]
        return f[*el.drop(1)]
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

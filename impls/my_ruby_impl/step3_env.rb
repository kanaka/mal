require_relative 'reader'
require_relative 'printer'
require_relative 'evaluator'
require_relative 'env'

class REPL
  DEF_SYMBOL = 'def!'
  LET_SYMBOL = 'let*'

  def self.reads
    input = $stdin.readline
    Reader.read_str(input)
  end

  def self.evals(ast, env)
    if !ast.is_a?(MalListType)
      Evaluator.eval_ast(ast, env)
    elsif ast.data.empty?
      ast
    else
      case ast.data[0].data
      when DEF_SYMBOL
        env.set(ast.data[1], evals(ast.data[2], env))
      when LET_SYMBOL
        inner_env = Env.new(env)
        ast.data[1].data.each_slice(2) { |key, value| inner_env.set(key, evals(value, inner_env)) }
        evals(ast.data[2], inner_env)
      else
        evaluated_list = Evaluator.eval_ast(ast, env)
        MalIntegerType.new(evaluated_list.data[0].call(*evaluated_list.data[1..].map(&:data)))
      end
    end
  end

  def self.prints(evaluated_input)
    Printer.pr_str(evaluated_input)
  end

  def self.rep
    env = Env.new(nil)
    env.set(MalSymbolType.new('*'), Proc.new{ |a, b| a * b})
    env.set(MalSymbolType.new('/'), Proc.new{ |a, b| a / b})
    env.set(MalSymbolType.new('+'), Proc.new{ |a, b| a + b})
    env.set(MalSymbolType.new('-'), Proc.new{ |a, b| a - b})

    while true
      begin
        puts "user> "
        puts prints(evals(reads, env))
      rescue Reader::EOFError
        puts "ERROR: EOF"
      rescue Reader::InvalidTokenError
        puts "ERROR: EOF - invalid token"
      rescue Evaluator::SymbolNotFound => e
        puts e
      end
    end
  end
end

REPL.rep

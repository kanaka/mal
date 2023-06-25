require_relative 'reader'
require_relative 'printer'
require_relative 'evaluator'

class REPL
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
      evaluated_list = Evaluator.eval_ast(ast, env)
      MalIntegerType.new(evaluated_list.data[0].call(*evaluated_list.data[1..].map(&:data)))
    end
  end

  def self.prints(evaluated_input)
    Printer.pr_str(evaluated_input)
  end

  def self.rep
    env = {
      '+' => Proc.new{ |a, b| a + b},
      '-' => Proc.new{ |a, b| a - b},
      '*' => Proc.new{ |a, b| a * b},
      '/' => Proc.new{ |a, b| a / b},
    }
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

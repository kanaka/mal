require_relative 'types'

class Evaluator
  class SymbolNotFound < StandardError; end

  def self.eval_ast(ast, env)
    case ast
    when MalSymbolType
      env.fetch(ast.data) { raise SymbolNotFound.new("#{ast.data} symbol not found") }
    when MalListType
      MalListType.new(ast.data.map{ |item| REPL.evals(item, env) })
    when MalVectorType
      MalVectorType.new(ast.data.map{ |item| REPL.evals(item, env) })
    when MalHashMapType
      MalHashMapType.new(ast.data.map{ |item| REPL.evals(item, env) })
    else
      ast
    end
  end
end

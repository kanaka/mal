require_relative 'reader'
require_relative 'printer'

class REPL
  def reads
    input = $stdin.readline
    Reader.read_str(input)
  end

  def evals(raw_input)
    raw_input
  end

  def prints(evaluated_input)
    Printer.pr_str(evaluated_input)
  end

  def rep
    while true
      begin
        puts "user> "
        puts prints(evals(reads))
      rescue Reader::EOFError
        puts "ERROR: EOF"
      rescue Reader::InvalidTokenError
        puts "ERROR: EOF - invalid token"
      end
    end
  end
end

REPL.new.rep

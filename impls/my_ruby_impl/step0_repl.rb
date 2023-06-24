class REPL
  def reads
    input = $stdin.readline
  end

  def evals(raw_input)
    raw_input
  end

  def prints(evaluated_input)
    evaluated_input
  end

  def rep
    while true
      puts "user> "
      puts prints(evals(reads))
    end
  end
end

REPL.new.rep

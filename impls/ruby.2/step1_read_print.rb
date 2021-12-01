require "readline"

require_relative "errors"
require_relative "printer"
require_relative "reader"

module Mal
  extend self

  def READ(input)
    read_str(input)
  end

  def EVAL(input)
    input
  end

  def PRINT(input)
    pr_str(input, true)
  end

  def rep(input)
    PRINT(EVAL(READ(input)))
  rescue InvalidHashmapKeyError => e
    "Error! Hashmap keys can only be strings or keywords."
  rescue UnbalancedEscapingError => e
    "Error! Detected unbalanced escaping. Check for matching '\\'."
  rescue UnbalancedHashmapError => e
    "Error! Detected unbalanced list. Check for matching '}'."
  rescue UnbalancedListError => e
    "Error! Detected unbalanced list. Check for matching ')'."
  rescue UnbalancedStringError => e
    "Error! Detected unbalanced string. Check for matching '\"'."
  rescue UnbalancedVectorError => e
    "Error! Detected unbalanced list. Check for matching ']'."
  rescue SkipCommentError
    nil
  end
end

while input = Readline.readline("user> ")
  puts Mal.rep(input)
end

puts

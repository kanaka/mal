require "./types"

module Mal
  class ParseException < Exception
  end

  class EvalException < Exception
  end

  class RuntimeException < Exception
    getter :thrown

    def initialize(@thrown : Type)
      super()
    end
  end
end

def eval_error(msg)
  raise Mal::EvalException.new msg
end

def parse_error(msg)
  raise Mal::ParseException.new msg
end

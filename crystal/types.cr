module Mal
  class Symbol
    property :val
    def initialize(@val)
    end
  end

  class List < Array(Type)
  end

  class Vector < Array(Type)
  end

  class HashMap < Hash(String, Type)
  end

  alias Type = Nil | Bool | Int32 | String | Symbol | List | Vector | HashMap | Proc(Array(Type), Type)

  alias Func = Proc(Array(Type), Type)

  class ParseException < Exception
  end

  class EvalException < Exception
  end
end

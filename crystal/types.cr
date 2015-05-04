module Mal
  class Symbol
    property :val
    def initialize(@val)
    end
  end

  alias Type = Nil | Bool | Int32 | String | Array(Type) | Symbol
end

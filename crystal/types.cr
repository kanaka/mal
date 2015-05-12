module Mal
  class Symbol
    property :str
    def initialize(@str)
    end
  end

  class List < Array(Type)
  end

  class Vector < Array(Type)
  end

  class HashMap < Hash(String, Type)
  end

  class Type
    alias ValueType = Nil | Bool | Int32 | String | Symbol | List | Vector | HashMap | ((Array(Type) -> Type))

    def initialize(@val : ValueType)
    end

    def initialize(other : Type)
      @val = other.val
    end

    def unwrap
      @val
    end
  end

  alias Func = Array(Type) -> Type
end

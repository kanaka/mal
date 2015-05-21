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

  class Closure
    property :ast, :params, :env, :fn
    def initialize(@ast, @params, @env, @fn)
    end
  end

  class Type
    alias Func = (Array(Type) -> Type)
    alias ValueType = Nil | Bool | Int32 | String | Symbol | List | Vector | HashMap | Func | Closure

    def initialize(@val : ValueType)
    end

    def initialize(other : Type)
      @val = other.unwrap
    end

    def unwrap
      @val
    end

    def ==(other : Mal::Type)
      @val == other.unwrap
    end

    macro rel_op(*ops)
      {% for op in ops %}
        def {{op.id}}(other : Mal::Type)
          l, r = @val, other.unwrap
            {% for t in [Int32, String] %}
              if l.is_a?({{t}}) && r.is_a?({{t}})
                return (l) {{op.id}} (r)
              end
            {% end %}
            if l.is_a?(Symbol) && r.is_a?(Symbol)
              return l.str {{op.id}} r.str
            end
          false
        end
      {% end %}
    end

    rel_op :<, :>, :<=, :>=
  end

  alias Func = Type::Func
end

macro gen_type(t)
  Mal::Type.new {{t.id}}.new
end

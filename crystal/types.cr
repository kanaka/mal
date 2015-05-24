require "./printer"

module Mal
  class Symbol
    property :str
    def initialize(@str)
    end

    def ==(other : Symbol)
      @str == other.str
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

    property :is_macro

    def initialize(@val : ValueType)
      @is_macro = false
    end

    def initialize(other : Type)
      @val = other.unwrap
      @is_macro = false
    end

    def unwrap
      @val
    end

    def macro?
      @is_macro
    end

    def to_s
      pr_str(self)
    end

    def ==(other : Type)
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

macro gen_type(t, *args)
  Mal::Type.new {{t.id}}.new({{*args}})
end

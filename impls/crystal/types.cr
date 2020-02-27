require "./printer"

module Mal
  class Type
    alias Func = (Array(Type) -> Type)

    property :is_macro, :meta

    def initialize(@val : ValueType)
      @is_macro = false
      @meta = nil.as(Type | Nil)
    end

    def initialize(other : Type)
      @val = other.unwrap
      @is_macro = other.is_macro
      @meta = other.meta
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

    def dup
      Type.new(@val).tap do |t|
        t.is_macro = @is_macro
        t.meta = @meta
      end
    end

    def ==(other : Type)
      @val == other.unwrap
    end

    macro rel_op(*ops)
      {% for op in ops %}
        def {{op.id}}(other : Mal::Type)
          l, r = @val, other.unwrap
            {% for t in [Int64, String] %}
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

  class Symbol
    property :str

    def initialize(@str : String)
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

  class Atom
    property :val

    def initialize(@val : Type)
    end

    def ==(rhs : Atom)
      @val == rhs.val
    end
  end

  class Closure
    property :ast, :params, :env, :fn

    def initialize(@ast : Type, @params : Array(Mal::Type) | List | Vector, @env : Env, @fn : Func)
    end
  end

  alias Type::ValueType = Nil | Bool | Int64 | String | Symbol | List | Vector | HashMap | Func | Closure | Atom
  alias Func = Type::Func
end

macro gen_type(t, *args)
  Mal::Type.new {{t.id}}.new({{*args}})
end

class Array
  def to_mal(t = Mal::List)
    each_with_object(t.new) { |e, l| l << e }
  end
end

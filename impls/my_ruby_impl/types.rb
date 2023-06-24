require 'forwardable'

class MalType
  attr_accessor :data

  def initialize(data)
    @data = data
  end

  def data_str
    data
  end
end

class MalScalarType < MalType
end

class MalListType < MalType
  extend Forwardable

  def initialize
    @data = []
  end
  def_delegator :@data, :append, :<<

  def begin_char
    '('
  end

  def end_char
    ')'
  end
end

class MalVectorType < MalListType
  def begin_char
    '['
  end

  def end_char
    ']'
  end
end

class MalHashMapType < MalListType
  def begin_char
    '{'
  end

  def end_char
    '}'
  end
end


class MalIntegerType < MalScalarType
end

class MalSymbolType < MalScalarType
end

class MalKeywordType < MalScalarType
end

class MalOperatorType < MalType
end

class MalModifierType < MalType
end

class MalDerefType < MalModifierType
  def identifier
    'deref '
  end
end

class MalUnquoteType < MalModifierType
  def identifier
    'unquote '
  end
end

class MalSpliceUnquoteType < MalModifierType
  def identifier
    'splice-unquote '
  end
end

class MalQuoteType < MalModifierType
  def identifier
    'quote '
  end
end

class MalQuasiQuoteType < MalModifierType
  def identifier
    'quasiquote '
  end
end

class MalWithMetaType < MalModifierType
  def identifier
    'with-meta '
  end
end

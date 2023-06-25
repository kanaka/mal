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

class MalCollectionType < MalType
  extend Forwardable

  def initialize(data = nil)
    @data = data || []
  end
  def_delegator :@data, :append, :<<
end

class MalListType < MalCollectionType
  def begin_char
    '('
  end

  def end_char
    ')'
  end
end

class MalVectorType < MalCollectionType
  def begin_char
    '['
  end

  def end_char
    ']'
  end
end

class MalHashMapType < MalCollectionType
  def begin_char
    '{'
  end

  def end_char
    '}'
  end
end


class MalIntegerType < MalScalarType
  def initialize(data)
    @data = data.to_i
  end
end

class MalSymbolType < MalScalarType
end

class MalKeywordType < MalScalarType
end

class MalModifierType < MalType
end

class MalSpecialFormType < MalType
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

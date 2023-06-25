require_relative 'types'

class Reader
  attr_reader :tokens
  attr_accessor :current_position

  class InvalidTokenError < StandardError; end
  class EOFError < StandardError; end

  LINE_REGEX = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/
  INTEGER_REGEX = /[0-9]+/
  SYMBOL_REGEX = /[0-9a-zA-Z\/\+\-\*]+/
  SPECIAL_CHARS = ['~', '`', "'", '@', '~@', '^']
  SPECIAL_FORMS = ['let*', 'def!']
  KEYWORD_PREFIX = ':'

  def initialize(tokens)
    @tokens = tokens
    @current_position = 0
  end

  def _next
    tmp = peek
    @current_position += 1
    tmp
  end

  def peek
    tokens[current_position]
  end

  def self.read_str(raw)
    new(tokenize(raw)).read_form
  end

  def self.tokenize(raw)
    raw.scan(LINE_REGEX).flatten.reject(&:empty?)
  end

  def read_form
    case peek
    when '('
      read_list
    when '['
      read_list(vector: true)
    when '{'
      read_list(hash_map: true)
    when ')', ']', '}'
      _next
      return
    when *SPECIAL_CHARS
      read_modifier
    else
      read_atom
    end
  end

  def read_atom
    current = _next

    if current.nil?
      raise EOFError.new
    elsif current && current[0] == KEYWORD_PREFIX
      MalKeywordType.new(current)
    elsif INTEGER_REGEX.match? current
      MalIntegerType.new(current)
    elsif SPECIAL_FORMS.include? current
      MalSpecialFormType.new(current)
    elsif SYMBOL_REGEX.match? current
      MalSymbolType.new(current)
    else
      raise InvalidTokenError.new("#{current} is not a valid token")
    end
  end

  def read_list(vector: false, hash_map: false)
    list = if vector
      MalVectorType.new
    elsif hash_map
      MalHashMapType.new
    else
      MalListType.new
    end
    _next

    while true
      next_token = read_form
      break unless next_token

      list << next_token
    end

    list
  end

  def read_modifier
    current = _next
    case current
    when "@"
      MalDerefType.new read_form
    when "~"
      MalUnquoteType.new read_form
    when "~@"
      MalSpliceUnquoteType.new read_form
    when "'"
      MalQuoteType.new read_form
    when "`"
      MalQuasiQuoteType.new read_form
    when "^"
      MalWithMetaType.new read_form
    end
  end
end

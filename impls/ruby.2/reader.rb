require_relative "errors"
require_relative "types"

module Mal
  extend self

  TOKEN_REGEX = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)/

  def read_atom(reader)
    case reader.peek
    when /\A"(?:\\.|[^\\"])*"\z/
      read_string(reader)
    when /\A"/
      raise UnbalancedStringError, "unbalanced string << #{reader.peek.inspect} >>"
    when /\A:/
      read_keyword(reader)
    when "nil"
      read_nil(reader)
    when "true"
      read_true(reader)
    when "false"
      read_false(reader)
    when /\A-?\d+(\.\d+)?/
      read_number(reader)
    when /\A;/
      raise SkipCommentError
    else
      read_symbol(reader)
    end
  end

  def read_deref(reader)
    list = Types::List.new
    list << Types::Symbol.for("deref")
    list << read_form(reader)
    list
  end

  def read_false(reader)
    reader.advance!
    Types::False.instance
  end

  def read_form(reader)
    case reader.peek
    when "'"
      read_quote(reader.advance!)
    when "`"
      read_quasiquote(reader.advance!)
    when "~"
      read_unquote(reader.advance!)
    when "~@"
      read_splice_unquote(reader.advance!)
    when "@"
      read_deref(reader.advance!)
    when "^"
      read_with_metadata(reader.advance!)
    when "("
      read_list(reader.advance!)
    when "["
      read_vector(reader.advance!)
    when "{"
      read_hashmap(reader.advance!)
    else
      read_atom(reader)
    end
  end

  def read_hashmap(reader)
    hashmap = Types::Hashmap.new

    until reader.peek == "}"
      key = read_form(reader)

      unless Types::String === key || Types::Keyword === key
        raise InvalidHashmapKeyError, "invalid hashmap key, must be string or keyword"
      end

      if reader.peek != "}"
        value = read_form(reader)
      else 
        raise UnbalancedHashmapError, "unbalanced hashmap error, missing closing '}'"
      end

      hashmap[key] = value
    end

    reader.advance!
    hashmap
  rescue Error => e
    case e
    when InvalidReaderPositionError
      raise UnbalancedHashmapError, "unbalanced hashmap error, missing closing '}'"
    else
      raise e
    end
  end

  def read_keyword(reader)
    value = reader.next.dup[1...]
    substitute_escaped_chars!(value)

    Types::Keyword.for(value)
  end

  def read_list(reader)
    list = Types::List.new

    until reader.peek == ")"
      list << read_form(reader)
    end

    reader.advance!
    list
  rescue Error => e
    case e
    when InvalidReaderPositionError
      raise UnbalancedListError, "unbalanced list error, missing closing ')'"
    else
      raise e
    end
  end

  def read_nil(reader)
    reader.advance!
    Types::Nil.instance
  end

  def read_number(reader)
    case reader.peek
    when /\d+\.\d+/
      Types::Number.new(reader.next.to_f)
    when /\d+/
      Types::Number.new(reader.next.to_i)
    else
      raise InvalidTypeError, "invalid number syntax, only supports integers/floats"
    end
  end

  def read_quasiquote(reader)
    list = Types::List.new
    list << Types::Symbol.for("quasiquote")
    list << read_form(reader)
    list
  end

  def read_quote(reader)
    list = Types::List.new
    list << Types::Symbol.for("quote")
    list << read_form(reader)
    list
  end

  def read_splice_unquote(reader)
    list = Types::List.new
    list << Types::Symbol.for("splice-unquote")
    list << read_form(reader)
    list
  end

  def read_str(input)
    tokenized = tokenize(input)
    raise SkipCommentError if tokenized.empty?
    read_form(Reader.new(tokenized))
  end

  def read_string(reader)
    raw_value = reader.next.dup

    value = raw_value[1...-1]
    substitute_escaped_chars!(value)

    if raw_value.length <= 1 || raw_value[-1] != '"'
      raise UnbalancedStringError, "unbalanced string error, missing closing '\"'"
    end

    Types::String.new(value)
  end

  def read_symbol(reader)
    Types::Symbol.for(reader.next)
  end

  def read_true(reader)
    reader.advance!
    Types::True.instance
  end

  def read_unquote(reader)
    list = Types::List.new
    list << Types::Symbol.for("unquote")
    list << read_form(reader)
    list
  end

  def read_vector(reader)
    vector = Types::Vector.new

    until reader.peek == "]"
      vector << read_form(reader)
    end

    reader.advance!
    vector
  rescue Error => e
    case e
    when InvalidReaderPositionError
      raise UnbalancedVectorError, "unbalanced vector error, missing closing ']'"
    else
      raise e
    end
  end

  def read_with_metadata(reader)
    list = Types::List.new
    list << Types::Symbol.for("with-meta")

    first = read_form(reader)
    second = read_form(reader)

    list << second
    list << first

    list
  end

  def tokenize(input)
    input.scan(TOKEN_REGEX).flatten.each_with_object([]) do |token, tokens|
      if token != "" && !token.start_with?(";")
        tokens << token
      end
    end
  end

  class Reader
    attr_reader :tokens

    def initialize(tokens)
      @position = 0
      @tokens = tokens
    end

    def advance!
      @position += 1
      self
    end

    def next
      value = peek
      @position += 1
      value
    end

    def peek
      if @position > @tokens.size - 1
        raise InvalidReaderPositionError, "invalid reader position error, unable to parse mal expression"
      end

      @tokens[@position]
    end
  end

  private

  def substitute_escaped_chars!(string_or_keyword)
    string_or_keyword.gsub!(/\\./, {"\\\\" => "\\", "\\n" => "\n", "\\\"" => '"'})
  end
end

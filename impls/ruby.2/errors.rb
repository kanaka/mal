module Mal
  class Error < ::StandardError; end
  class TypeError < ::TypeError; end

  class MalError < Error
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def message
      value.inspect
    end
  end

  class FileNotFoundError < Error; end
  class IndexError < TypeError; end
  class SkipCommentError < Error; end

  class InvalidHashmapKeyError < TypeError; end
  class InvalidIfExpressionError < TypeError; end
  class InvalidLetBindingsError < TypeError; end
  class InvalidReaderPositionError < Error; end
  class InvalidTypeError < TypeError; end

  class NotCallableError < Error; end

  class SymbolNotFoundError < Error; end
  class SyntaxError < TypeError; end

  class UnbalancedEscapingError < Error; end
  class UnbalancedHashmapError < Error; end
  class UnbalancedListError < Error; end
  class UnbalancedStringError < Error; end
  class UnbalancedVectorError < Error; end

  class UnknownError < Error
    attr_reader :original_error

    def initialize(original_error)
      @original_error = original_error
    end

    def inspect
      "UnknownError :: #{original_error.inspect}"
    end

    def message
      "UnknownError<#{original_error.class}> :: #{original_error.message}"
    end
  end
end

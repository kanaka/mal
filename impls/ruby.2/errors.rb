module Mal
  class Error < ::StandardError; end
  class TypeError < ::TypeError; end

  class SkipCommentError < Error; end
  class FileNotFoundError < Error; end

  class InvalidHashmapKeyError < TypeError; end
  class InvalidIfExpressionError < TypeError; end
  class InvalidLetBindingsError < TypeError; end
  class InvalidReaderPositionError < Error; end
  class InvalidTypeError < TypeError; end

  class NotCallableError < Error; end

  class SymbolNotFoundError < Error; end

  class UnbalancedEscapingError < Error; end
  class UnbalancedHashmapError < Error; end
  class UnbalancedListError < Error; end
  class UnbalancedStringError < Error; end
  class UnbalancedVectorError < Error; end

  class UnknownError < Error; end
end

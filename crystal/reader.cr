require "./types"
require "./error"

class Reader
  def initialize(@tokens)
    @pos = 0
  end

  def current_token
    @tokens[@pos] rescue nil
  end

  def peek
    t = current_token

    if t && t[0] == ';'
      @pos += 1
      peek
    else
      t
    end
  end

  def next
    peek
  ensure
    @pos += 1
  end

  def read_sequence(init, open, close)
    token = self.next
    parse_error "expected '#{open}', got EOF" unless token
    parse_error "expected '#{open}', got #{token}" unless  token[0] == open

    loop do
      token = peek
      parse_error "expected '#{close}', got EOF" unless token
      break if token[0] == close

      init << read_form
      peek
    end

    self.next
    init
  end

  def read_list
    Mal::Type.new read_sequence(Mal::List.new, '(', ')')
  end

  def read_vector
    Mal::Type.new read_sequence(Mal::Vector.new, '[', ']')
  end

  def read_hashmap
    types = read_sequence([] of Mal::Type, '{', '}')

    parse_error "odd number of elements for hash-map: #{types.size}" if types.size.odd?
    map = Mal::HashMap.new

    types.each_slice(2) do |kv|
      k, v = kv[0].unwrap, kv[1]
      case k
      when String
        map[k] = v
      else
        parse_error("key of hash-map must be string or keyword")
      end
    end

    Mal::Type.new map
  end

  def read_atom
    token = self.next
    parse_error "expected Atom but got EOF" unless token

    Mal::Type.new case
    when token =~ /^-?\d+$/ then token.to_i
    when token == "true"    then true
    when token == "false"   then false
    when token == "nil"     then nil
    when token[0] == '"'    then token[1..-2].gsub(/\\"/, "\"")
    when token[0] == ':'    then "\u029e#{token[1..-1]}"
    else                         Mal::Symbol.new token
    end
  end

  def list_of(symname)
    Mal::List.new << gen_type(Mal::Symbol, symname) << read_form
  end

  def read_form
    token = peek

    parse_error "unexpected EOF" unless token
    parse_error "unexpected comment" if token[0] == ';'

    Mal::Type.new case token
    when "("  then read_list
    when ")"  then parse_error "unexpected ')'"
    when "["  then read_vector
    when "]"  then parse_error "unexpected ']'"
    when "{"  then read_hashmap
    when "}"  then parse_error "unexpected '}'"
    when "'"  then self.next; list_of("quote")
    when "`"  then self.next; list_of("quasiquote")
    when "~"  then self.next; list_of("unquote")
    when "~@" then self.next; list_of("splice-unquote")
    when "@"  then self.next; list_of("deref")
    when "^"
      self.next
      meta = read_form
      list_of("with-meta") << meta
    else read_atom
    end
  end

end

def tokenize(str)
  regex = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
  str.scan(regex).map{|m| m[1]}.reject(&.empty?)
end

def read_str(str)
  r = Reader.new(tokenize(str))
  begin
    r.read_form
  ensure
    unless r.peek.nil?
      raise Mal::ParseException.new "expected EOF, got #{r.peek.to_s}"
    end
  end
end


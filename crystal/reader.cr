require "./types"

class Reader
  def initialize(@tokens)
    @pos = 0
  end

  def peek
    @tokens[@pos] rescue nil
  end

  def next
    @tokens[@pos] rescue nil
  ensure
    @pos += 1
  end
end

def tokenize(str)
  regex = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
  str.scan(regex).map{|m| m[1]}.reject(&.empty?)
end

def read_str(str)
  r = Reader.new(tokenize(str))
  begin
    read_form r
  ensure
    unless r.peek.nil?
      raise "expected EOF but got #{r.peek.to_s}"
    end
  end
end

def read_list(reader)
  token = reader.next
  raise "expected '('" unless token && token[0] == '('

  forms = [] of Mal::Type

  loop do
    token = reader.peek
    raise "expected ')' but got EOF" unless token
    break if token[0] == ')'

    forms << read_form reader
    reader.peek
  end

  reader.next
  forms
end

def read_atom(reader)
  token = reader.next
  raise "expected Atom but got EOF" unless token

  case
  when token =~ /^-?\d+$/ then token.to_i
  when token[0] == '"'    then token[1..-2].gsub(/\\"/, "\"")
  when token == "true"    then true
  when token == "false"   then false
  when token == "nil"     then nil
  else                         Mal::Symbol.new token
  end
end

def read_form(reader)
  token = reader.peek
  raise "unexpected EOF" unless token

  case token[0]
  when ';' then nil
  when '(' then read_list reader
  when ')' then raise "unexpected ')'"
  else read_atom reader
  end
end


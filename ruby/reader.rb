require "types"

class Reader
    def initialize(tokens)
        @position = 0
        @tokens = tokens
    end
    def peek
        return @tokens[@position]
    end
    def next
        @position += 1
        return @tokens[@position-1]
    end
end


def tokenize(str)
    re = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
    return str.scan(re).map{|m| m[0]}.select{ |t|
        t != "" && t[0..0] != ";"
    }
end

def parse_str(t)
    return t[1..-2].gsub(/\\"/, '"').gsub(/\\n/, "\n") # unescape
end

def read_atom(rdr)
    token = rdr.next
    return case token
        when /^-?[0-9]+$/ then       token.to_i # integer
        when /^-?[0-9][0-9.]*$/ then token.to_f # float
        when /^"/ then               parse_str(token) # string
        when "nil" then              nil
        when "true" then             true
        when "false" then            false
        else                         token.to_sym # symbol
    end
end

def read_list(rdr, klass, start="(", last =")")
    ast = klass.new
    token = rdr.next()
    if token != start
        raise "expected '" + start + "'"
    end
    while (token = rdr.peek) != last
        if not token
            raise "expected '" + last + "', got EOF"
        end
        ast.push(read_form(rdr))
    end
    rdr.next
    return ast
end

def read_form(rdr)
    token = rdr.peek
    return case rdr.peek
        when ";" then  nil
        when "'" then  rdr.next; List.new [:quote, read_form(rdr)]
        when "`" then  rdr.next; List.new [:quasiquote, read_form(rdr)]
        when "~" then  rdr.next; List.new [:unquote, read_form(rdr)]
        when "~@" then rdr.next; List.new [:"splice-unquote", read_form(rdr)]
        when "^" then  rdr.next; meta = read_form(rdr);
                       List.new [:"with-meta", read_form(rdr), meta]
        when "@" then  rdr.next; List.new [:deref, read_form(rdr)]

        when "(" then  read_list(rdr, List, "(", ")")
        when ")" then  raise "unexpected ')'"
        when "[" then  read_list(rdr, Vector, "[", "]")
        when "]" then  raise "unexpected ']'" 
        when "{" then  Hash[read_list(rdr, List, "{", "}").each_slice(2).to_a]
        when "}" then  raise "unexpected '}'" 
        else           read_atom(rdr)
    end
end

def read_str(str)
    tokens = tokenize(str)
    return nil if tokens.size == 0
    return read_form(Reader.new(tokens))
end


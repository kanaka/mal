import groovy.json.StringEscapeUtils
import types
import types.MalException
import types.MalSymbol

class reader {
    static class Reader {
        def tokens
        def position
        Reader(def toks) {
            tokens = toks
            position = 0
        }

        def peek() {
            if (position >= tokens.size) {
                null
            } else {
                tokens[position]
            }
        }
        def next() {
            if (position >= tokens.size) {
                null
            } else {
                tokens[position++]
            }
        }
    }

    def static tokenizer(String str) {
        def m = str =~ /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/
        def tokens = []
        while (m.find()) {
            String token = m.group(1)
            if (token != null &&
                !(token == "") &&
                !(token[0] == ';')) {
                tokens.add(token)
            }
        }
        return tokens
    }

    def static read_atom(Reader rdr) {
        def token = rdr.next()
        def m = token =~ /(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^"(.*)"$|:(.*)|(^[^"]*$)/
        if (!m.find()) {
            throw new MalException("unrecognized token '$token'")
        }
        if (m.group(1) != null) {
            Integer.parseInt(m.group(1))
        } else if (m.group(3) != null) {
            null
        } else if (m.group(4) != null) {
            true
        } else if (m.group(5) != null) {
            false
        } else if (m.group(6) != null) {
            StringEscapeUtils.unescapeJava(m.group(6))
        } else if (m.group(7) != null) {
            "\u029e" + m.group(7)
        } else if (m.group(8) != null) {
            new MalSymbol(m.group(8))
        } else {
            throw new MalException("unrecognized '${m.group(0)}'")
        }
    }

    def static read_list(Reader rdr, char start, char end) {
        def token = rdr.next()
        def lst = []
        if (token.charAt(0) != start) {
            throw new MalException("expected '${start}'")
        }

        while ((token = rdr.peek()) != null && token.charAt(0) != end) {
            lst.add(read_form(rdr))
        }

        if (token == null) {
            throw new MalException("expected '${end}', got EOF")
        }
        rdr.next()

        return lst
    }

    def static read_vector(Reader rdr) {
        def lst = read_list(rdr, '[' as char, ']' as char)
        return types.vector(lst)
    }

    def static read_hash_map(Reader rdr) {
        def lst = read_list(rdr, '{' as char, '}' as char)
        return types.hash_map(lst)
    }

    def static read_form(Reader rdr) {
        def token = rdr.peek()
        switch (token) {
            // reader macros/transforms
            case "'":
                rdr.next()
                return [new MalSymbol("quote"), read_form(rdr)]
            case '`':
                rdr.next()
                return [new MalSymbol("quasiquote"), read_form(rdr)]
            case '~':
                rdr.next()
                return [new MalSymbol("unquote"), read_form(rdr)]
            case '~@':
                rdr.next()
                return [new MalSymbol("splice-unquote"), read_form(rdr)]
            case '^':
                rdr.next()
                def meta = read_form(rdr);
                return [new MalSymbol("with-meta"), read_form(rdr), meta]
            case '@':
                rdr.next()
                return [new MalSymbol("deref"), read_form(rdr)]

            // list
            case ')': throw new MalException("unexpected ')'")
            case '(': return read_list(rdr, '(' as char, ')' as char)

            // vector
            case ']': throw new MalException("unexpected ']'")
            case '[': return read_vector(rdr)

            // hash-map
            case '}': throw new MalException("unexpected '}'")
            case '{': return read_hash_map(rdr)

            // atom
            default: return read_atom(rdr)
        }
    }
    
    def static read_str(String str) {
        def tokens = tokenizer(str)
        if (tokens.size() == 0) {
            return null;
        }
        //println "tokens ${tokens}"
        def rdr = new Reader(tokens)
        read_form(rdr)
    }
}


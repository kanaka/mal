public class Reader
{
    0 => int position;
    string tokens[];

    fun string peek()
    {
        return tokens[position];
    }

    fun string next()
    {
        return tokens[position++];
    }

    fun static string[] tokenizer(string input)
    {
        "^[ \n,]*(~@|[][{}()'`~^@]|\"(\\\\.|[^\\\"])*\"|;[^\n]*|[^][ \n{}()'`~@,;\"]*)" => string tokenRe;
        "^([ \n,]*|;[^\n]*)$" => string blankRe;

        string tokens[0];

        while( true )
        {
            string matches[1];
            RegEx.match(tokenRe, input, matches);
            matches[1] => string token;

            if( token.length() == 0 && !RegEx.match(blankRe, input) )
            {
                tokens << input;
                break;
            }

            if( !RegEx.match(blankRe, token) )
            {
                tokens << token;
            }

            matches[0].length() => int tokenStart;
            String.slice(input, tokenStart) => input;

            if( input.length() == 0 )
            {
                break;
            }
        }

        return tokens;
    }

    fun static MalObject read_str(string input)
    {
        Reader reader;
        tokenizer(input) @=> reader.tokens;

        if( reader.tokens.size() == 0 )
        {
            return MalError.create(MalString.create("empty input"));
        }
        else
        {
            return read_form(reader);
        }
    }

    fun static MalObject read_form(Reader reader)
    {
        reader.peek() => string token;
        if( token == "(" )
        {
            return read_list(reader, "(", ")");
        }
        else if( token == "[" )
        {
            return read_list(reader, "[", "]");
        }
        else if( token == "{" )
        {
            return read_list(reader, "{", "}");
        }
        else if( token == ")" || token == "]" || token == "}" )
        {
            return MalError.create(MalString.create("unexpected '" + token + "'"));
        }
        else if( token == "'" )
        {
            return read_simple_reader_macro(reader, "quote");
        }
        else if( token == "`" )
        {
            return read_simple_reader_macro(reader, "quasiquote");
        }
        else if( token == "~" )
        {
            return read_simple_reader_macro(reader, "unquote");
        }
        else if( token == "~@" )
        {
            return read_simple_reader_macro(reader, "splice-unquote");
        }
        else if( token == "@" )
        {
            return read_simple_reader_macro(reader, "deref");
        }
        else if( token == "^" )
        {
            return read_meta_reader_macro(reader);
        }
        else
        {
            return read_atom(reader);
        }
    }

    fun static MalObject read_list(Reader reader, string start, string end)
    {
        MalObject items[0];

        reader.next(); // discard list start token

        while( true )
        {
            // HACK: avoid checking for reader.peek() returning null
            // (as doing that directly isn't possible and too
            // bothersome to do indirectly)
            if( reader.position == reader.tokens.size() )
            {
                return MalError.create(MalString.create("expected '" + end + "', got EOF"));
            }

            if( reader.peek() == end )
            {
                break;
            }

            read_form(reader) @=> MalObject item;

            if( item.type == "error" )
            {
                return item;
            }
            else
            {
                items << item;
            }
        }

        reader.next(); // discard list end token

        if( start == "(" )
        {
            return MalList.create(items);
        }
        else if( start == "[" )
        {
            return MalVector.create(items);
        }
        else if( start == "{" )
        {
            return MalHashMap.create(items);
        }
    }

    fun static MalObject read_atom(Reader reader)
    {
        "^[+-]?[0-9]+$" => string intRe;
        "^\"(\\\\.|[^\\\"])*\"$" => string stringRe;

        reader.next() => string token;

        if( token == "true" )
        {
            return Constants.TRUE;
        }
        else if( token == "false" )
        {
            return Constants.FALSE;
        }
        else if( token == "nil" )
        {
            return Constants.NIL;
        }
        else if( RegEx.match(intRe, token) )
        {
            return MalInt.create(Std.atoi(token));
        }
        else if( token.substring(0, 1) == "\"" )
        {
            if( RegEx.match(stringRe, token) )
            {
                return MalString.create(String.parse(token));
            }
            else
            {
                return MalError.create(MalString.create("expected '\"', got EOF"));
            }
        }
        else if( token.substring(0, 1) == ":" )
        {
            return MalKeyword.create(String.slice(token, 1));
        }
        else
        {
            return MalSymbol.create(token);
        }
    }

    fun static MalObject read_simple_reader_macro(Reader reader, string symbol)
    {
        reader.next(); // discard reader macro token

        read_form(reader) @=> MalObject form;
        if( form.type == "error" )
        {
            return form;
        }

        return MalList.create([MalSymbol.create(symbol), form]);
    }

    fun static MalObject read_meta_reader_macro(Reader reader)
    {
        reader.next(); // discard reader macro token

        read_form(reader) @=> MalObject meta;
        if( meta.type == "error" )
        {
            return meta;
        }

        read_form(reader) @=> MalObject form;
        if( form.type == "error" )
        {
            return meta;
        }

        return MalList.create([MalSymbol.create("with-meta"), form, meta]);
    }
}

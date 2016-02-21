import std.array;
import std.regex;
import std.stdio;
import types;

MalSymbol sym_quote;
MalSymbol sym_quasiquote;
MalSymbol sym_unquote;
MalSymbol sym_splice_unquote;
MalSymbol sym_deref;
MalSymbol sym_with_meta;

static this()
{
    sym_quote = new MalSymbol("quote");
    sym_quasiquote = new MalSymbol("quasiquote");
    sym_unquote = new MalSymbol("unquote");
    sym_splice_unquote = new MalSymbol("splice-unquote");
    sym_deref = new MalSymbol("deref");
    sym_with_meta = new MalSymbol("with-meta");
}

class Reader
{
    int pos = 0;
    const string[] tokens;

    this(string[] the_tokens)
    {
        tokens = the_tokens.dup;
    }

    string peek()
    {
        if (pos >= tokens.length) return null;
        return tokens[pos];
    }

    string next()
    {
        auto token = peek();
        pos++;
        return token;
    }
}

auto tokenize_ctr = ctRegex!(r"[\s,]*(~@|[\[\]{}()'`~^@]|" `"` `(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"` r"`,;)]*)");

string[] tokenize(string str)
{
    string[] tokens;
    foreach(c; matchAll(str, tokenize_ctr))
    {
        auto token = c[1];
        if (token.length == 0) continue;
        if (token[0] == ';') continue;
        tokens ~= token;
    }
    return tokens;
}

MalString parse_string(string token)
{
    string unescaped = 
        token[1..$-1] // Remove surrounding quotes
        .replace("\\n", "\n")
        .replace("\\\"", "\"")
        .replace("\\\\", "\\");
    return new MalString(unescaped);
}

auto integer_ctr = ctRegex!(r"^-?[0-9]+$");

MalType read_atom(Reader reader)
{
    auto token = reader.next();
    switch (token)
    {
        case "nil": return mal_nil;
        case "false": return mal_false;
        case "true": return mal_true;
        default:
            switch (token[0]) {
                case ':':
                    return new MalString("\u029e" ~ token[1..$]);
                case '"':
                    return parse_string(token);
                default:
                    auto captures = matchFirst(token, integer_ctr);
                    if (!captures.empty())
                    {
                        return new MalInteger(token);
                    }

                    return new MalSymbol(token);
            }
    }
}

MalType[] read_items(Reader reader, string start, string end)
{
    auto open_paren = reader.next();
    if (open_paren != start) throw new Exception("expected '" ~ start ~ "'");

    string token;
    MalType[] res;
    while ((token = reader.peek()) != end)
    {
        if (token is null)
        {
            throw new Exception("expected '" ~ end ~ "'");
        }
        res ~= read_form(reader);
    }
    reader.next(); // consume the ')'
    return res;
}

MalList read_list(Reader reader)
{
    return new MalList(read_items(reader, "(", ")"));
}

MalVector read_vector(Reader reader)
{
    return new MalVector(read_items(reader, "[", "]"));
}

MalHashmap read_hashmap(Reader reader)
{
    return new MalHashmap(read_items(reader, "{", "}"));
}

MalList read_quote_shortcut(Reader reader, MalSymbol sym)
{
    reader.next(); // consume the special quote char
    return new MalList([sym, read_form(reader)]);
}

MalType read_form(Reader reader)
{
    auto token = reader.peek();
    if (token is null) return new MalNil();
    switch(token)
    {
        case "'":
            return read_quote_shortcut(reader, sym_quote);
        case "`":
            return read_quote_shortcut(reader, sym_quasiquote);
        case "~":
            return read_quote_shortcut(reader, sym_unquote);
        case "~@":
            return read_quote_shortcut(reader, sym_splice_unquote);
        case "@":
            return read_quote_shortcut(reader, sym_deref);
        case "^":
            reader.next(); // consume the caret char
            auto meta = read_form(reader);
            return new MalList([sym_with_meta, read_form(reader), meta]);
        case "(":
            return read_list(reader);
        case ")":
            throw new Exception("unexpected ')'");
        case "[":
            return read_vector(reader);
        case "]":
            throw new Exception("unexpected ']'");
        case "{":
            return read_hashmap(reader);
        case "}":
            throw new Exception("unexpected '}'");
        default:
            return read_atom(reader);
    }
}

MalType read_str(string str)
{
    auto tokens = tokenize(str);
    auto reader = new Reader(tokens);
    return read_form(reader);
}

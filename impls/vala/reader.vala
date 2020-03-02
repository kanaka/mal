class Mal.Reader : GLib.Object {
    static Regex tok_re;
    static Regex tok_num;

    int origlen;
    string data;
    int pos;

    string next_token;

    static construct {
        tok_re = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;[^\n]*|[^\s\[\]{}('"`,;)]*)/; // comment to unconfuse emacs vala-mode "]);
        tok_num = /^-?[0-9]/;
    }

    private string poserr(string fmt, ...) {
        return "char %d: %s".printf(origlen - data.length,
                                    fmt.vprintf(va_list()));
    }

    private void advance() throws Error {
        do {
            MatchInfo info;
            if (!tok_re.match(data, 0, out info))
                throw new Error.BAD_TOKEN(poserr("bad token"));

            next_token = info.fetch(1);
            int tokenend;
            info.fetch_pos(1, null, out tokenend);
            data = data[tokenend:data.length];
        } while (next_token.has_prefix(";"));
    }

    public Reader(string str) throws Error {
        data = str;
        origlen = data.length;
        pos = 0;
        advance();
    }

    public string peek() throws Error {
        return next_token;
    }

    public string next() throws Error {
        advance();
        return peek();
    }

    public static Mal.Val? read_str(string str) throws Error {
        var rdr = new Reader(str);
        if (rdr.peek() == "")
            return null;
        var toret = rdr.read_form();
        if (rdr.peek() != "")
            throw new Mal.Error.PARSE_ERROR(
                rdr.poserr("trailing junk after expression"));
        return toret;
    }

    public Mal.Val read_form() throws Error {
        string token = peek();
        if (token == "(") {
            next(); // eat (
            return new Mal.List(read_list(")"));
        } else {
            return read_atom();
        }
    }

    public GLib.List<Mal.Val> read_list(string endtok) throws Error {
        var list = new GLib.List<Mal.Val>();
        string token;
        while (true) {
            token = peek();
            if (token == "")
                throw new Mal.Error.PARSE_ERROR(poserr("unbalanced parens"));
            if (token == endtok) {
                next(); // eat end token
                return list;
            }

            list.append(read_form());
        }
    }

    public Mal.Hashmap read_hashmap() throws Error {
        var map = new Mal.Hashmap();
        string token;
        while (true) {
            Mal.Val vals[2];
            for (int i = 0; i < 2; i++) {
                token = peek();
                if (token == "")
                    throw new Mal.Error.PARSE_ERROR(
                        poserr("unbalanced braces"));
                if (token == "}") {
                    if (i != 0)
                        throw new Mal.Error.PARSE_ERROR(
                            poserr("odd number of elements in hashmap"));

                    next(); // eat end token
                    return map;
                }

                vals[i] = read_form();
            }
            map.insert(vals[0], vals[1]);
        }
    }

    public Mal.Val read_atom() throws Error {
        string token = peek();
        next();
        if (tok_num.match(token))
            return new Mal.Num(int64.parse(token));
        if (token.has_prefix(":"))
            return new Mal.Keyword(token[1:token.length]);
        if (token.has_prefix("\"")) {
            if (token.length < 2 || !token.has_suffix("\""))
                throw new Mal.Error.BAD_TOKEN(
                    poserr("end of input in mid-string"));

            token = token[1:token.length-1];

            int end = 0;
            int pos = 0;
            string strval = "";

            while ((pos = token.index_of ("\\", end)) != -1) {
                strval += token[end:pos];
                if (token.length - pos < 2)
                    throw new Mal.Error.BAD_TOKEN(
                        poserr("end of input in mid-string"));
                switch (token[pos:pos+2]) {
                case "\\\\":
                    strval += "\\"; break;
                case "\\\"":
                    strval += "\""; break;
                case "\\n":
                    strval += "\n"; break;
                }
                end = pos+2;
            }
            strval += token[end:token.length];
            return new Mal.String(strval);
        }
        switch (token) {
        case "nil":
            return new Mal.Nil();
        case "true":
            return new Mal.Bool(true);
        case "false":
            return new Mal.Bool(false);
        case "[":
            return new Mal.Vector.from_list(read_list("]"));
        case "{":
            return read_hashmap();
        case "'":
        case "`":
        case "~":
        case "~@":
        case "@":
            var list = new GLib.List<Mal.Val>();
            list.append(new Mal.Sym(
                            token == "'" ? "quote" :
                            token == "`" ? "quasiquote" :
                            token == "~" ? "unquote" :
                            token == "~@" ? "splice-unquote" : "deref"));
            list.append(read_form());
            return new Mal.List(list);
        case "^":
            var list = new GLib.List<Mal.Val>();
            list.append(new Mal.Sym("with-meta"));
            var metadata = read_form();
            list.append(read_form());
            list.append(metadata);
            return new Mal.List(list);
        default:
            return new Mal.Sym(token);
        }
    }
}

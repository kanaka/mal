package reader;

import types.Types.MalType;
import types.Types.*;

class Reader {
    // Reader class implementation
    var tokens:Array<String>;
    var position:Int = 0;

    public function new(toks:Array<String>) {
        tokens = toks;
    }

    public function next() {
        return tokens[position++];
    }

    public function peek() {
        if (tokens.length > position) {
            return tokens[position];
        } else {
            return null;
        }
    }


    // Static functions grouped with Reader class
    static function tokenize(str:String) {
        var re = ~/[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g;
        var tokens = new Array<String>();
        var pos = 0;
        while (re.matchSub(str, pos)) {
            var t = re.matched(1);
            if (t == "") { break; }
            var pos_len = re.matchedPos();
            pos = pos_len.pos + pos_len.len;
            if (t.charAt(0) == ";") { continue; }
            tokens.push(t);

        }
        return tokens;
    }

    static function read_atom(rdr:Reader) {
        var re_int = ~/^[0-9]*$/;
        var re_str = ~/^".*"$/;
        var token = rdr.next();
        return switch (token) {
            case "nil":
                MalNil;
            case "true":
                MalTrue;
            case "false":
                MalFalse;
            case _ if (token.charAt(0) == ":"):
                MalString("\x7f" + token.substr(1));
            case _ if (re_int.match(token)):
                MalInt(Std.parseInt(token));
            case _ if (re_str.match(token)):
                var re1 = ~/\\"/g,
                    re2 = ~/\\n/g,
                    re3 = ~/\\\\/g,
                    s = token.substr(1, token.length-2);
                MalString(re3.replace(
                           re2.replace(
                             re1.replace(s, "\""),
                             "\n"),
                           "\\"));
            case _:
                MalSymbol(token);
        }
    }

    static function read_seq(rdr:Reader, start, end) {
        var lst = [];
        var token = rdr.next();
        if (token != start) {
            throw 'expected \'${start}\'';
        }
        while ((token = rdr.peek()) != end) {
            if (token == null) {
                throw 'expected \'${end}\', got EOF';
            }
            lst.push(read_form(rdr));
        }
        rdr.next();
        return lst;
    }

    static function read_form(rdr:Reader):MalType {
        var token = rdr.peek();
        return switch (token) {
            // reader macros/transforms
            case "'": rdr.next();
                      MalList([MalSymbol("quote"), read_form(rdr)]);
            case "`": rdr.next();
                      MalList([MalSymbol("quasiquote"), read_form(rdr)]);
            case "~": rdr.next();
                      MalList([MalSymbol("unquote"), read_form(rdr)]);
            case "~@": rdr.next();
                      MalList([MalSymbol("splice-unquote"), read_form(rdr)]);
            case "^": rdr.next();
                      var meta = read_form(rdr);
                      MalList([MalSymbol("with-meta"), read_form(rdr), meta]);
            case "@": rdr.next();
                      MalList([MalSymbol("deref"), read_form(rdr)]);

            // list
            case ")": throw("unexpected ')'");
            case "(": MalList(read_seq(rdr, '(', ')'));

            // vector
            case "]": throw("unexpected ']'");
            case "[": MalVector(read_seq(rdr, '[', ']'));

            // hashmap
            case "}": throw("unexpected '}'");
            case "{": hash_map(read_seq(rdr, '{', '}'));
            case _: read_atom(rdr);
        }
    }

    public static function read_str(str:String):MalType {
        var tokens = tokenize(str);
        if (tokens.length == 0) { throw(new BlankLine()); }
        return read_form(new Reader(tokens));
    }
}

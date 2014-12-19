package mal;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringEscapeUtils;
import mal.types.*;

public class reader {
    public static class ParseError extends MalThrowable {
        public ParseError(String msg) {
            super(msg);
        }
    }

    public static class Reader {
        ArrayList<String> tokens;
        Integer position;
        public Reader(ArrayList<String> t) {
            tokens = t;
            position = 0;
        }

        public String peek() {
            if (position >= tokens.size()) {
                return null;
            } else {
                return tokens.get(position);
            }
        }
        public String next() {
            return tokens.get(position++);
        }
    }

    public static ArrayList<String> tokenize(String str) {
        ArrayList<String> tokens = new ArrayList<String>();
        Pattern pattern = Pattern.compile("[\\s ,]*(~@|[\\[\\]{}()'`~@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\\s \\[\\]{}()'\"`~@,;]*)");
        Matcher matcher = pattern.matcher(str);
        while (matcher.find()) {
            String token = matcher.group(1);
            if (token != null &&
                !token.equals("") &&
                !(token.charAt(0) == ';')) {
                tokens.add(token);
            }
        }
        return tokens;
    }

    public static MalVal read_atom(Reader rdr)
            throws ParseError {
        String token = rdr.next();
        Pattern pattern = Pattern.compile("(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^\"(.*)\"$|:(.*)|(^[^\"]*$)");
        Matcher matcher = pattern.matcher(token);
        if (!matcher.find()) {
            throw new ParseError("unrecognized token '" + token + "'");
        }
        if (matcher.group(1) != null) {
            return new MalInteger(Integer.parseInt(matcher.group(1)));
        } else if (matcher.group(3) != null) {
            return types.Nil;
        } else if (matcher.group(4) != null) {
            return types.True;
        } else if (matcher.group(5) != null) {
            return types.False;
        } else if (matcher.group(6) != null) {
            return new MalString(StringEscapeUtils.unescapeJson(matcher.group(6)));
        } else if (matcher.group(7) != null) {
            return new MalString("\u029e" + matcher.group(7));
        } else if (matcher.group(8) != null) {
            return new MalSymbol(matcher.group(8));
        } else {
            throw new ParseError("unrecognized '" + matcher.group(0) + "'");
        }
    }

    public static MalVal read_list(Reader rdr, MalList lst, char start, char end)
            throws MalContinue, ParseError {
        String token = rdr.next();
        if (token.charAt(0) != start) {
            throw new ParseError("expected '" + start + "'");
        }

        while ((token = rdr.peek()) != null && token.charAt(0) != end) {
            lst.conj_BANG(read_form(rdr));
        }

        if (token == null) {
            throw new ParseError("expected '" + end + "', got EOF");
        }
        rdr.next();

        return lst;
    }

    public static MalVal read_hash_map(Reader rdr)
            throws MalContinue, ParseError {
        MalList lst = (MalList)read_list(rdr, new MalList(), '{', '}');
        return new MalHashMap(lst);
    }

    public static MalVal read_form(Reader rdr)
            throws MalContinue, ParseError {
        String token = rdr.peek();
        if (token == null) { throw new MalContinue(); }
        MalVal form;

        switch (token.charAt(0)) {
            case '\'': rdr.next();
                       return new MalList(new MalSymbol("quote"),
                                          read_form(rdr));
            case '`': rdr.next();
                      return new MalList(new MalSymbol("quasiquote"),
                                         read_form(rdr));
            case '~':
                if (token.equals("~")) {
                    rdr.next();
                    return new MalList(new MalSymbol("unquote"),
                                       read_form(rdr));
                } else {
                    rdr.next();
                    return new MalList(new MalSymbol("splice-unquote"),
                                       read_form(rdr));
                }
            case '^': rdr.next();
                      MalVal meta = read_form(rdr);
                      return new MalList(new MalSymbol("with-meta"),
                                         read_form(rdr),
                                         meta);
            case '@': rdr.next();
                      return new MalList(new MalSymbol("deref"),
                                         read_form(rdr));
            case '(': form = read_list(rdr, new MalList(), '(' , ')'); break;
            case ')': throw new ParseError("unexpected ')'");
            case '[': form = read_list(rdr, new MalVector(), '[' , ']'); break;
            case ']': throw new ParseError("unexpected ']'");
            case '{': form = read_hash_map(rdr); break;
            case '}': throw new ParseError("unexpected '}'");
            default:  form = read_atom(rdr);
        }
        return form;
    }

    public static MalVal read_str(String str)
            throws MalContinue, ParseError {
        return read_form(new Reader(tokenize(str)));
    }
}

package truffle.mal;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class Reader {
    private static final Pattern TOKEN_PATTERN = Pattern.compile("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)");

    public static List<String> tokenize(String s) {
        var m = TOKEN_PATTERN.matcher(s);
        var result = new ArrayList<String>();
        while (m.find()) {
            String t = m.group(1);
            if (!t.isEmpty()) {
                result.add(t);
            }
        }
        return result;
    }

    public static Object readStr(String s) {
        return new Reader(tokenize(s)).readForm();
    }

    private int i = 0;
    private final List<String> tokens;

    private Reader(List<String> tokens) {
        this.tokens = tokens;
    }

    private boolean hasNext() {
        return i < tokens.size();
    }

    private String peek() {
        if (!hasNext()) {
            throw new MalException("EOF");
        }
        return tokens.get(i);
    }

    private String next() {
        if (!hasNext()) {
            throw new MalException("EOF");
        }
        return tokens.get(i++);
    }

    private Object readForm() {
        if (!hasNext()) {
            return MalNil.NIL;
        }
        String t = peek();
        if (t.equals("'")) {
            next();
            return MalList.EMPTY.cons(readForm()).cons(MalSymbol.QUOTE);
        } else if (t.equals("`")) {
            next();
            return MalList.EMPTY.cons(readForm()).cons(MalSymbol.QUASIQUOTE);
        } else if (t.equals("@")) {
            next();
            return MalList.EMPTY.cons(readForm()).cons(MalSymbol.DEREF);
        } else if (t.equals("~")) {
            next();
            return MalList.EMPTY.cons(readForm()).cons(MalSymbol.UNQUOTE);
        } else if (t.equals("~@")) {
            next();
            return MalList.EMPTY.cons(readForm()).cons(MalSymbol.SPLICE_UNQUOTE);
        } else if (t.equals("^")) {
            next();
            var meta = readForm();
            var obj = readForm();
            return MalList.EMPTY.cons(meta).cons(obj).cons(MalSymbol.get("with-meta"));
        } else if (t.equals("(")) {
            return readList();
        } else if (t.equals("[")) {
            return readVector();
        } else if (t.equals("{")) {
            return readMap();
        } else if (t.startsWith(";")) {
            // gobble up consecutive comments without consuming stack space
            while (t.startsWith(";")) {
                next();
                if (!hasNext())
                    break;
                t = peek();
            }
            return readForm();
        } else {
            return readAtom();
        }
    }

    private MalVector readVector() {
        var elements = new ArrayList<Object>();
        next(); // consume '['
        while (!peek().equals("]")) {
            elements.add(readForm());
        }
        next(); // consume ']'
        return MalVector.EMPTY.concat(elements);
    }

    private MalList readList() {
        var elements = new ArrayList<Object>();
        next(); // consume '('
        while (!peek().equals(")")) {
            elements.add(readForm());
        }
        next(); // consume ')'
        MalList result = MalList.EMPTY;
        var iter = elements.listIterator(elements.size());
        while (iter.hasPrevious()) {
            result = result.cons(iter.previous());
        }
        return result;
    }

    private MalMap readMap() {
        MalMap map = MalMap.EMPTY;
        next(); // consume '{'
        while (!peek().equals("}")) {
            map = map.assoc(readForm(), readForm());
        }
        next(); // consume '}'
        return map;
    }

    private Object readAtom() {
        String t = next();
        if (t.charAt(0) == '"') {
            StringBuilder sb = new StringBuilder();
            int i=1;
            for (int j=t.indexOf('\\', i); j != -1; j=t.indexOf('\\', i)) {
                sb.append(t.subSequence(i, j));
                switch (t.charAt(j+1)) {
                case 'n': sb.append('\n'); break;
                case '"': sb.append('"'); break;
                case '\\': sb.append('\\'); break;
                }
                i = j+2;
            }
            if (i > t.length()-1 || t.charAt(t.length()-1) != '"') {
                throw new MalException("EOF");
            }
            sb.append(t.substring(i, t.length()-1));
            return sb.toString();
        } else if (t.charAt(0) == ':') {
            return MalKeyword.get(t.substring(1));
        } else if (t.charAt(0) >= '0' && t.charAt(0) <= '9') {
            return Long.parseLong(t);
        } else if (t.length() > 1 && t.charAt(0) == '-' && t.charAt(1) >= '0' && t.charAt(1) <= '9') {
            return Long.parseLong(t);
        } else if (t.equals("true")) {
            return true;
        } else if (t.equals("false")) {
            return false;
        } else if (t.equals("nil")) {
            return MalNil.NIL;
        } else {
            return MalSymbol.get(t);
        }
    }
}

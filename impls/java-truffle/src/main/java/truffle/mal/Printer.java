package truffle.mal;

public class Printer {

    public static String prStr(Object form, boolean printReadably) {
        var buf = new StringBuilder();
        prStr(buf, form, printReadably);
        return buf.toString();
    }

    public static void prStr(StringBuilder buf, Object form, boolean printReadably) {
        if (form instanceof Boolean) {

            buf.append((boolean)form);

        } else if (form instanceof Long) {

            buf.append((long)form);

        } else if (form instanceof String) {

            var s = (String)form;
            if (printReadably) {
                buf.append('"');
                buf.append(s.replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "\\\""));
                buf.append('"');
            } else {
                buf.append(s);
            }

        } else if (form instanceof MalSymbol) {

            buf.append(((MalSymbol)form).symbol);

        } else if (form instanceof MalKeyword) {

            buf.append(':');
            buf.append(((MalKeyword)form).keyword);

        } else if (form instanceof MalNil) {

            buf.append("nil");

        } else if (form instanceof MalList) {

            var list = (MalList)form;
            buf.append("(");
            MalList l = list;
            while (l != null && l.head != null) {
                prStr(buf, l.head, printReadably);
                l = l.tail;
                if (l.head != null) {
                    buf.append(' ');
                }
            }
            buf.append(")");

        } else if (form instanceof MalVector) {

            var vector = (MalVector)form;
            final int size = vector.size();
            buf.append('[');
            for (int i=0; i < size; ++i) {
                prStr(buf, vector.get(i), printReadably);
                if (i < size-1) {
                    buf.append(' ');
                }
            }
            buf.append(']');

        } else if (form instanceof MalMap) {

            var map = (MalMap)form;
            int i = 0;
            buf.append('{');
            for (var entry : map.map) {
                prStr(buf, entry.getKey(), printReadably);
                buf.append(' ');
                prStr(buf, entry.getValue(), printReadably);
                if (++i < map.map.size()) {
                    buf.append(' ');
                }
            }
            buf.append('}');

        } else if (form instanceof MalFunction) {

            buf.append("#<function>");

        } else if (form instanceof MalAtom) {

            buf.append("(atom ");
            prStr(buf, ((MalAtom)form).deref(), printReadably);
            buf.append(")");

        } else {
            throw new RuntimeException("Not a MAL type: "+form.getClass().getCanonicalName());
        }
    }
}

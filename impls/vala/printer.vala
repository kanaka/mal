namespace Mal {
    string pr_str(Mal.Val val, bool print_readably = true) {
        if (val is Mal.Nil)
            return "nil";
        if (val is Mal.Bool)
            return (val as Mal.Bool).v ? "true" : "false";
        if (val is Mal.Sym)
            return (val as Mal.Sym).v;
        if (val is Mal.Keyword)
            return ":" + (val as Mal.Keyword).v;
        if (val is Mal.Num)
            return ("%"+int64.FORMAT_MODIFIER+"d")
                .printf((val as Mal.Num).v);
        if (val is Mal.String) {
            string s = (val as Mal.String).v;
            if (print_readably)
                s = "\"%s\"".printf(s.replace("\\", "\\\\")
                                    .replace("\n", "\\n").
                                    replace("\"", "\\\""));
            return s;
        }
        if (val is Mal.Listlike) {
            bool vec = val is Mal.Vector;
            string toret = vec ? "[" : "(";
            string sep = "";
            for (var iter = (val as Mal.Listlike).iter();
                 iter.nonempty(); iter.step()) {
                toret += sep + pr_str(iter.deref(), print_readably);
                sep = " ";
            }
            toret += vec ? "]" : ")";
            return toret;
        }
        if (val is Mal.Hashmap) {
            string toret = "{";
            string sep = "";
            var map = (val as Mal.Hashmap).vs;
            foreach (var key in map.get_keys()) {
                toret += (sep + pr_str(key, print_readably) + " " +
                          pr_str(map[key], print_readably));
                sep = " ";
            }
            toret += "}";
            return toret;
        }
        if (val is Mal.BuiltinFunction) {
            return "#<builtin:%s>".printf((val as Mal.BuiltinFunction).name());
        }
        if (val is Mal.Function) {
            return "#<function>";
        }
        if (val is Mal.Atom) {
            return "(atom %s)".printf(
                pr_str((val as Mal.Atom).v, print_readably));
        }
        return "??";
    }
}

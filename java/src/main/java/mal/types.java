package mal;

import java.util.List;
import java.util.ArrayList;
import com.google.common.base.Joiner;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringEscapeUtils;

public class types {
    //
    // Exceptions/Errors
    //
    public static class MalThrowable extends Exception {
        public MalThrowable() { }
        public MalThrowable(String msg) { super(msg); }
    }
    public static class MalError extends MalThrowable {
        public MalError(String msg) { super(msg); }
    }
    public static class MalContinue extends MalThrowable { }

    // Thrown by throw function
    public static class MalException extends MalThrowable {
        MalVal value;
        public MalException(MalVal value) {
            this.value = value;
        }
        public MalException(String value) {
            this.value = new MalString(value);
        }
        public MalVal getValue() { return value; }
    }


    //
    // Mal boxed types
    //
    abstract public static class MalVal {
        MalVal meta = Nil;
        abstract public MalVal copy() throws MalThrowable;

        // Default is just to call regular toString()
        public String toString(Boolean print_readably) {
            return this.toString();
        }
        public MalVal getMeta() { return meta; }
        public void setMeta(MalVal m) { meta = m; }
    }
    public static class MalConstant extends MalVal {
        String value;
        public MalConstant(String name) { value = name; }
        public MalConstant copy() throws MalThrowable { return this; }

        public String toString() { return value; }
    }
    static MalConstant Nil = new MalConstant("nil");
    static MalConstant True = new MalConstant("true");
    static MalConstant False = new MalConstant("false");

    public static class MalInteger extends MalVal {
        Integer value;
        public MalInteger(Integer v) { value = v; }
        public MalInteger copy() throws MalThrowable { return this; }

        public Integer getValue() { return value; }
        @Override public String toString() {
            return value.toString();
        }
        public MalInteger add(MalInteger other) {
            return new MalInteger(value + other.getValue());
        }
        public MalInteger subtract(MalInteger other) {
            return new MalInteger(value - other.getValue());
        }
        public MalInteger multiply(MalInteger other) {
            return new MalInteger(value * other.getValue());
        }
        public MalInteger divide(MalInteger other) {
            return new MalInteger(value / other.getValue());
        }
        public MalConstant lt(MalInteger other) {
            return (value < other.getValue()) ? True : False;
        }
        public MalConstant lte(MalInteger other) {
            return (value <= other.getValue()) ? True : False;
        }
        public MalConstant gt(MalInteger other) {
            return (value > other.getValue()) ? True : False;
        }
        public MalConstant gte(MalInteger other) {
            return (value >= other.getValue()) ? True : False;
        }
    }

    public static class MalSymbol extends MalVal {
        String value;
        public MalSymbol(String v) { value = v; }
        public MalSymbol copy() throws MalThrowable { return this; }

        public String getName() { return value; }
        @Override public String toString() {
            return value;
        }
    }

    public static class MalString extends MalVal {
        String value;
        public MalString(String v) { value = v; }
        public MalString copy() throws MalThrowable { return this; }

        public String getValue() { return value; }
        @Override public String toString() {
            return "\"" + value + "\"";
        }
        public String toString(Boolean print_readably) {
            if (print_readably) {
                return "\"" + StringEscapeUtils.escapeJson(value) + "\"";
            } else {
                return value;
            }
        }
    }

    public static class MalList extends MalVal {
        String start = "(", end = ")";
        List value;
        public MalList(List val) {
            value = val;
        }
        public MalList(MalVal... mvs) {
            value = new ArrayList<MalVal>();
            conj_BANG(mvs);
        }
        public MalList copy() throws MalThrowable {
            MalList new_ml = new MalList();
            new_ml.value.addAll(value);
            new_ml.meta = meta;
            return new_ml;
        }

        String _join(String delim, Boolean print_readably) {
            ArrayList<String> strs = new ArrayList<String>();
            for (MalVal mv : (List<MalVal>)value) {
                strs.add(mv.toString(print_readably));
            }
            return Joiner.on(delim).join(strs);
        }
        @Override public String toString() {
            return start + _join(" ", true) + end;
        }
        public String toString(Boolean print_readably) {
            return start + _join(" ", print_readably) + end;
        }
        
        public MalList conj_BANG(MalVal... mvs) {
            for (MalVal mv : mvs) {
                value.add(mv);
            }
            return this;
        }

        public Integer size() {
            return value.size();
        }

        public MalVal nth(Integer idx) {
            return (MalVal)value.get(idx);
        }

        public MalList slice(Integer start, Integer end) {
            return new MalList(value.subList(start, end));
        }
        public MalList slice(Integer start) {
            return slice(start, value.size());
        }
    }

    public static class MalVector extends MalList {
        // Same implementation except for instantiation methods
        public MalVector(List val) {
            value = val;
            start = "[";
            end = "]";
        }
        public MalVector(MalVal... mvs) {
            super(mvs);
            start = "[";
            end = "]";
        }
        public MalVector copy() throws MalThrowable {
            MalVector new_mv = new MalVector();
            new_mv.value.addAll(value);
            new_mv.meta = meta;
            return new_mv;
        }

        public MalVector slice(Integer start, Integer end) {
            return new MalVector(value.subList(start, end));
        }
    }

    public static class MalHashMap extends MalVal {
        Map value;
        public MalHashMap(Map val) {
            value = val;
        }
        public MalHashMap(MalList lst) {
            value = new HashMap<String, MalVal>();
            assoc_BANG(lst);
        }
        public MalHashMap(MalVal... mvs) {
            value = new HashMap<String, MalVal>();
            assoc_BANG(mvs);
        }
        public MalHashMap copy() throws MalThrowable {
            Map<String,MalVal> shallowCopy = new HashMap<String,MalVal>();
            shallowCopy.putAll(value);
            MalHashMap new_hm = new MalHashMap(shallowCopy);
            new_hm.meta = meta;
            return new_hm;
        }

        String _join(Boolean print_readably) {
            ArrayList<String> strs = new ArrayList<String>();
            for (Map.Entry<String, MalVal> entry :
                    ((Map<String,MalVal>)value).entrySet()) {
                if (print_readably) {
                    strs.add("\"" + entry.getKey().toString() + "\"");
                } else {
                    strs.add(entry.getKey().toString());
                }
                strs.add(entry.getValue().toString(print_readably));
            }
            return Joiner.on(" ").join(strs);
        }
        @Override public String toString() {
            return "{" + _join(true) + "}";
        }
        public String toString(Boolean print_readably) {
            return "{" + _join(print_readably) + "}";
        }

        public Set _entries() {
            return value.entrySet();
        }
        
        public MalHashMap assoc_BANG(MalVal... mvs) {
            for (Integer i=0; i<mvs.length; i+=2) {
                value.put(((MalSymbol)mvs[i]).getName(),
                          mvs[i+1]);
            }
            return this;
        }

        public MalHashMap assoc_BANG(MalList lst) {
            for (Integer i=0; i<lst.value.size(); i+=2) {
                value.put(((MalString)lst.nth(i)).getValue(),
                          lst.nth(i+1));
            }
            return this;
        }

        public MalHashMap dissoc_BANG(MalList lst) {
            for (Integer i=0; i<lst.value.size(); i++) {
                value.remove(((MalString)lst.nth(i)).getValue());
            }
            return this;
        }

        public Integer size() {
            return value.size();
        }
    }

    public static class MalAtom extends MalVal {
        MalVal value;
        public MalAtom(MalVal value) { this.value = value; }
        public MalAtom copy() throws MalThrowable { return new MalAtom(value); }
        @Override public String toString() {
            return "(atom " + _pr_str(value, true) + ")";
        }
        public String toString(Boolean print_readably) {
            return "(atom " + _pr_str(value, print_readably) + ")";
        }
    }

    public static interface ILambda {
        public MalVal apply(MalList args) throws MalThrowable;
    }

    public static abstract class MalFunction extends MalVal
            implements ILambda, java.lang.Cloneable {
        public MalVal ast = null;
        public Env env = null;
        public MalList params = null;
        public Boolean macro = false;
        public MalFunction() { }
        public MalFunction(MalVal ast, Env env, MalList params) {
            this.ast = ast;
            this.env = env;
            this.params = params;
        }
        public MalFunction copy() throws MalThrowable {
            try {
                // WARNING: clone() is broken:
                //   http://www.artima.com/intv/bloch13.html
                // However, this doesn't work:
                //   MalFunction new_mf = this.getClass().newInstance();
                // So for now it's clone.
                MalFunction new_mf = (MalFunction) this.clone();
                new_mf.ast = ast;
                new_mf.env = env;
                new_mf.params = params;
                new_mf.macro = macro;
                return new_mf;
            } catch (Throwable t) {
                // not much we can do
                t.printStackTrace();
                throw new MalError("Could not copy MalFunction: " + this);
            }
        }

        public MalVal getAst() { return ast; }
        public Env getEnv() { return env; }
        public MalList getParams() { return params; }
        public Boolean isMacro() { return macro; }
        public void setMacro() { macro = true; }
    }


    //
    // General functions
    //
    public static String _pr_str(MalVal mv, Boolean print_readably) {
        return mv.toString(print_readably);
    }

    public static String _pr_str_args(MalList args, String sep, Boolean print_readably) {
        return args._join(sep, print_readably);
    }

    static MalFunction pr_str = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalString(_pr_str_args(args, " ", true));
        }
    };

    static MalFunction str = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalString(_pr_str_args(args, "", false));
        }
    };

    static MalFunction prn = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            System.out.println(_pr_str_args(args, " ", true));
            return Nil;
        }
    };

    static MalFunction println = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            System.out.println(_pr_str_args(args, " ", false));
            return Nil;
        }
    };


    static MalFunction meta = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0).getMeta();
        }
    };

    static MalFunction with_meta = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            MalVal new_mv = ((MalVal)args.nth(0)).copy();
            new_mv.setMeta(args.nth(1));
            return new_mv;
        }
    };


    public static Boolean _equal_Q(MalVal a, MalVal b) {
        Class ota = a.getClass(), otb = b.getClass();
        if (!((ota == otb) ||
              (a instanceof MalList && b instanceof MalList))) {
            return false;
        } else {
            if (a instanceof MalInteger) {
                return ((MalInteger)a).getValue() ==
                       ((MalInteger)b).getValue();
            } else if (a instanceof MalSymbol) {
                return ((MalSymbol)a).getName().equals(
                       ((MalSymbol)b).getName());
            } else if (a instanceof MalString) {
                return ((MalString)a).getValue().equals(
                       ((MalString)b).getValue());
            } else if (a instanceof MalList) {
                if (((MalList)a).size() != ((MalList)b).size()) {
                    return false;
                }
                for (Integer i=0; i<((MalList)a).size(); i++) {
                    if (! _equal_Q(((MalList)a).nth(i),
                                   ((MalList)b).nth(i))) {
                        return false;
                    }
                }
                return true;
            } else {
                return a == b;
            }
        }
    }

    static MalFunction equal_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return _equal_Q(args.nth(0), args.nth(1)) ? True : False;
        }
    };


    //
    // Constants operations
    //
    static MalFunction symbol_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0) instanceof MalSymbol ? True : False;
        }
    };

    static MalFunction nil_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0) == Nil ? True : False;
        }
    };

    static MalFunction true_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0) == True ? True : False;
        }
    };

    static MalFunction false_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0) == False ? True : False;
        }
    };


    //
    // Number operations
    //
    static MalFunction add = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).add((MalInteger)a.nth(1));
        }
    };
    static MalFunction subtract = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).subtract((MalInteger)a.nth(1));
        }
    };
    static MalFunction multiply = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).multiply((MalInteger)a.nth(1));
        }
    };
    static MalFunction divide = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).divide((MalInteger)a.nth(1));
        }
    };

    static MalFunction lt = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).lt((MalInteger)a.nth(1));
        }
    };
    static MalFunction lte = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).lte((MalInteger)a.nth(1));
        }
    };
    static MalFunction gt = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).gt((MalInteger)a.nth(1));
        }
    };
    static MalFunction gte = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalInteger)a.nth(0)).gte((MalInteger)a.nth(1));
        }
    };

    //
    // Errors/Exceptions
    //
    static MalFunction mal_throw = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            throw new MalException(a.nth(0));
        }
    };

    //
    // List operations
    //
    static MalFunction new_list = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalList(a.value);
        }
    };

    static public Boolean _list_Q(MalVal mv) {
        return mv.getClass().equals(MalList.class);
    }
    static MalFunction list_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return _list_Q(a.nth(0)) ? True : False;
        }
    };

    //
    // Vector operations
    //
    static MalFunction new_vector = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalVector(a.value);
        }
    };

    static public Boolean _vector_Q(MalVal mv) {
        return mv.getClass().equals(MalVector.class);
    }
    static MalFunction vector_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return _vector_Q(a.nth(0)) ? True : False;
        }
    };

    //
    // Hash map operations
    //
    static MalFunction new_hash_map = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalHashMap(a);
        }
    };

    static MalFunction hash_map_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return a.nth(0) instanceof MalHashMap ? True : False;
        }
    };

    static MalFunction contains_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            String key = ((MalString)a.nth(1)).getValue();
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            return hm.containsKey(key) ? True : False;
        }
    };

    static MalFunction assoc = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            MalHashMap new_mhm = new MalHashMap((Map)hm.clone());
            new_mhm.assoc_BANG((MalList)a.slice(1));
            return new_mhm;
        }
    };

    static MalFunction dissoc = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            MalHashMap new_mhm = new MalHashMap((Map)hm.clone());
            new_mhm.dissoc_BANG((MalList)a.slice(1));
            return new_mhm;
        }
    };

    static MalFunction get = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            String key = ((MalString)a.nth(1)).getValue();
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            if (hm.containsKey(key)) {
                return hm.get(key);
            } else {
                return Nil;
            }
        }
    };

    static MalFunction keys = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            MalList key_lst = new MalList();
            for (String key : hm.keySet()) {
                key_lst.conj_BANG(new MalString(key));
            }
            return key_lst;
        }
    };

    static MalFunction vals = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalHashMap mhm = (MalHashMap)a.nth(0);
            HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
            //return new ArrayList<MalVal>(((HashMap<String,MalVal>)hm).values());
            MalList val_lst = new MalList();
            for (MalVal val : hm.values()) {
                val_lst.conj_BANG(val);
            }
            return val_lst;
        }
    };


    //
    // Atoms
    //
    static MalFunction new_atom = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalAtom(a.nth(0));
        }
    };

    static MalFunction atom_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return a.nth(0) instanceof MalAtom ? True : False;
        }
    };

    static MalFunction deref = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalAtom)a.nth(0)).value;
        }
    };

    static MalFunction reset_BANG = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return ((MalAtom)a.nth(0)).value = a.nth(1);
        }
    };

    static MalFunction swap_BANG = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalAtom atm = (MalAtom)a.nth(0);
            MalFunction f = (MalFunction)a.nth(1);
            MalList new_args = new MalList();
            new_args.value.addAll(((MalList)a.slice(2)).value);
            new_args.value.add(0, atm.value);
            atm.value = f.apply(new_args);
            return atm.value;
        }
    };

    


    //
    // Sequence operations
    //
    static MalFunction sequential_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return a.nth(0) instanceof MalList ? True : False;
        }
    };

    static MalFunction count = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalInteger(((MalList)a.nth(0)).size());
        }
    };

    static MalFunction empty_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalVal exp = a.nth(0);
            if (exp == Nil || (exp instanceof MalList &&
                               ((MalList)exp).size() == 0)) {
                return True;
            } else {
                return False;
            }
        }
    };

    static MalFunction cons = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList lst = new MalList();
            lst.value.addAll(((MalList)a.nth(1)).value);
            lst.value.add(0, a.nth(0));
            return (MalVal) lst;
        }
    };

    static MalFunction concat = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            if (a.size() == 0) { return new MalList(); }
            MalList lst = new MalList();
            lst.value.addAll(((MalList)a.nth(0)).value);
            for(Integer i=1; i<a.size(); i++) {
                lst.value.addAll(((MalList)a.nth(i)).value);
            }
            return (MalVal) lst;
        }
    };

    static MalFunction conj = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList src_seq = (MalList)a.nth(0), new_seq;
            if (a.nth(0) instanceof MalVector) {
                new_seq = new MalVector();
                new_seq.value.addAll(src_seq.value);
                for(Integer i=1; i<a.size(); i++) {
                    new_seq.value.add(a.nth(i));
                }
            } else {
                new_seq = new MalList();
                new_seq.value.addAll(src_seq.value);
                for(Integer i=1; i<a.size(); i++) {
                    new_seq.value.add(0, a.nth(i));
                }
            }
            return (MalVal) new_seq;
        }
    };

    static MalFunction first = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList ml = ((MalList)a.nth(0));
            return ml.size() > 0 ? ml.nth(0) : Nil;
        }
    };

    static MalList _rest (MalList ml) {
        if (ml.size() > 0) {
            return new MalList(ml.value.subList(1, ml.value.size()));
        } else {
            return new MalList();
        }
    }

    static MalFunction rest = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList ml = ((MalList)a.nth(0));
            return _rest(ml);
        }
    };

    static MalFunction nth = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            Integer idx = ((MalInteger)a.nth(1)).getValue();
            return ((MalList)a.nth(0)).nth(idx);
        }
    };

    // General list related functions
    static MalFunction apply = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalFunction f = (MalFunction)a.nth(0);
            MalList args = a.slice(1,a.size()-1);
            args.value.addAll( ((MalList)a.nth(a.size()-1)).value);
            return f.apply(args);
        }
    };

    static MalFunction map = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalFunction f = (MalFunction) a.nth(0);
            MalList src_lst = (MalList) a.nth(1);
            MalList new_lst = new MalList();
            for(Integer i=0; i<src_lst.size(); i++) {
                new_lst.value.add(
                        f.apply(new MalList(src_lst.nth(i))));
            }
            return new_lst;
        }
    };



    //
    // Env implementation
    //
    public static class Env {
        Env outer = null;
        HashMap<String,MalVal> data = new HashMap<String,MalVal>();

        public Env(Env outer) {
            this.outer = outer;
        }
        public Env(Env outer, MalList binds, MalList exprs) {
            this.outer = outer;
            for (Integer i=0; i<binds.size(); i++) {
                String sym = ((MalSymbol)binds.nth(i)).getName();
                if (sym.equals("&")) {
                    data.put(((MalSymbol)binds.nth(i+1)).getName(),
                            exprs.slice(i));
                    break;
                } else {
                    data.put(sym, exprs.nth(i));
                }
            }
        }
        
        public Env find(String key) {
            if (data.containsKey(key)) {
                return this;
            } else if (outer != null) {
                return outer.find(key);
            } else {
                return null;
            }
        }

        public MalVal get(String key) throws MalThrowable {
            Env e = find(key);
            if (e == null) {
                throw new MalException("'" + key + "' not found");
            } else {
                return e.data.get(key);
            }
        }

        public Env set(String key, MalVal value) {
            data.put(key, value);
            return this;
        }
    }

    // types_ns is namespace of type functions
    static Map<String, MalVal> types_ns = ImmutableMap.<String, MalVal>builder()
        .put("pr-str",    pr_str)
        .put("str",       str)
        .put("prn",       prn)
        .put("println",   println)
        .put("meta",      meta)
        .put("with-meta", with_meta)
        .put("=",         equal_Q)
        .put("symbol?",   symbol_Q)
        .put("nil?",      nil_Q)
        .put("true?",     true_Q)
        .put("false?",    false_Q)
        .put("<",         lt)
        .put("<=",        lte)
        .put(">",         gt)
        .put(">=",        gte)
        .put("+",         add)
        .put("-",         subtract)
        .put("*",         multiply)
        .put("/",         divide)
        .put("throw",     mal_throw)
        .put("list",      new_list)
        .put("list?",     list_Q)
        .put("vector",    new_vector)
        .put("vector?",   vector_Q)
        .put("hash-map",  new_hash_map)
        .put("map?",      hash_map_Q)
        .put("assoc",     assoc)
        .put("dissoc",    dissoc)
        .put("contains?", contains_Q)
        .put("get",       get)
        .put("keys",      keys)
        .put("vals",      vals)
        .put("atom",      new_atom)
        .put("atom?",     atom_Q)
        .put("deref",     deref)
        .put("reset!",    reset_BANG)
        .put("swap!",     swap_BANG)
        .put("sequential?", sequential_Q)
        .put("cons",      cons)
        .put("concat",    concat)
        .put("conj",      conj)
        .put("first",     first)
        .put("rest",      rest)
        .put("nth",       nth)
        .put("count",     count)
        .put("empty?",    empty_Q)
        .put("apply",     apply)
        .put("map",       map)
        .build();
}

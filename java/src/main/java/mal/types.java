package mal;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;

import mal.printer;
import mal.env.Env;

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
    // General functions
    //

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
            } else if (a instanceof MalHashMap) {
                if (((MalHashMap)a).value.size() != ((MalHashMap)b).value.size()) {
                    return false;
                }
                //HashMap<String,MalVal> hm = (HashMap<String,MalVal>)a.value;
                MalHashMap mhm = ((MalHashMap)a);
                HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
                for (String k : hm.keySet()) {
                    if (! _equal_Q(((MalVal)((MalHashMap)a).value.get(k)),
                                   ((MalVal)((MalHashMap)b).value.get(k)))) {
                        return false;
                    }
                }
                return true;
            } else {
                return a == b;
            }
        }
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
        public Boolean list_Q() { return false; }
    }
    public static class MalConstant extends MalVal {
        String value;
        public MalConstant(String name) { value = name; }
        public MalConstant copy() throws MalThrowable { return this; }

        public String toString() { return value; }
    }
    public static MalConstant Nil = new MalConstant("nil");
    public static MalConstant True = new MalConstant("true");
    public static MalConstant False = new MalConstant("false");

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
        public MalSymbol(MalString v) { value = v.getValue(); }
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
            if (value.length() > 0 && value.charAt(0) == '\u029e') {
                return ":" + value.substring(1);
            } else if (print_readably) {
                return "\"" + printer.escapeString(value) + "\"";
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

        @Override public String toString() {
            return start + printer.join(value, " ", true) + end;
        }
        public String toString(Boolean print_readably) {
            return start + printer.join(value, " ", print_readably) + end;
        }

        public List getList() { return value; }
        public Boolean list_Q() { return true; }
        
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
        public MalList rest () {
            if (size() > 0) {
                return new MalList(value.subList(1, value.size()));
            } else {
                return new MalList();
            }
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

        public Boolean list_Q() { return false; }

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

        @Override public String toString() {
            return "{" + printer.join(value, " ", true) + "}";
        }
        public String toString(Boolean print_readably) {
            return "{" + printer.join(value, " ", print_readably) + "}";
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
            return "(atom " + printer._pr_str(value, true) + ")";
        }
        public String toString(Boolean print_readably) {
            return "(atom " + printer._pr_str(value, print_readably) + ")";
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
        public Env genEnv(MalList args) {
            return new Env(env, params, args);
        }
        public Boolean isMacro() { return macro; }
        public void setMacro() { macro = true; }
    }
}

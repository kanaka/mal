package mal;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import com.google.common.collect.ImmutableMap;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.io.File;

import mal.types.*;
import mal.printer;
import mal.readline;

public class core {
    // Local references for convenience
    static MalConstant Nil = mal.types.Nil;
    static MalConstant True = mal.types.True;
    static MalConstant False = mal.types.False;


    // Errors/Exceptions
    static MalFunction mal_throw = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            throw new MalException(a.nth(0));
        }
    };


    // Scalar functions
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
    static MalFunction symbol = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalSymbol((MalString)args.nth(0));
        }
    };
    static MalFunction symbol_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return args.nth(0) instanceof MalSymbol ? True : False;
        }
    };
    static MalFunction keyword = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalString(
                    "\u029e" + ((MalString)args.nth(0)).getValue());
        }
    };
    static MalFunction keyword_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            if (args.nth(0) instanceof MalString &&
                (((MalString)args.nth(0)).getValue().charAt(0) == '\u029e')) {
                return True;
            } else {
                return False;
            }
        }
    };


    // String functions
    static MalFunction pr_str = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalString(printer._pr_str_args(args, " ", true));
        }
    };

    static MalFunction str = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return new MalString(printer._pr_str_args(args, "", false));
        }
    };

    static MalFunction prn = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            System.out.println(printer._pr_str_args(args, " ", true));
            return Nil;
        }
    };

    static MalFunction println = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            System.out.println(printer._pr_str_args(args, " ", false));
            return Nil;
        }
    };


    static MalFunction equal_Q = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            return types._equal_Q(args.nth(0), args.nth(1)) ? True : False;
        }
    };

    static MalFunction mal_readline = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            String prompt = ((MalString)args.nth(0)).getValue();
            try {
                return new MalString(readline.readline(prompt));
            } catch (IOException e) {
                throw new MalException(new MalString(e.getMessage()));
            } catch (readline.EOFException e) {
                return Nil;
            }
        }
    };

    static MalFunction read_string = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            try {
                return reader.read_str(((MalString)args.nth(0)).getValue());
            } catch (MalContinue c) {
                return types.Nil;
            }
        }
    };

    static MalFunction slurp = new MalFunction() {
        public MalVal apply(MalList args) throws MalThrowable {
            String fname = ((MalString)args.nth(0)).getValue();
            try {
                // Scanner drops final newline, so add it back
                return new MalString(
                    new Scanner(new File(fname)).useDelimiter("\\Z").next()
                                + "\n");
            } catch (FileNotFoundException e) {
                throw new MalError(e.getMessage());
            }
        }
    };


    // Number functions
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

    static MalFunction time_ms = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return new MalInteger((int)System.currentTimeMillis());
        }
    };


    // List functions
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


    // Vector functions
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

    // HashMap functions
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
            if (a.nth(0) == Nil) {
                return Nil;
            } else {
                String key = ((MalString)a.nth(1)).getValue();
                MalHashMap mhm = (MalHashMap)a.nth(0);
                HashMap<String,MalVal> hm = (HashMap<String,MalVal>)mhm.value;
                if (hm.containsKey(key)) {
                    return hm.get(key);
                } else {
                    return Nil;
                }
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


    // Sequence functions
    static MalFunction sequential_Q = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            return a.nth(0) instanceof MalList ? True : False;
        }
    };

    static MalFunction count = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            if (a.nth(0) == Nil) {
                return new MalInteger(0);
            } else {
                return new MalInteger(((MalList)a.nth(0)).size());
            }
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
            List<MalVal> lst = new ArrayList<MalVal>();
            lst.add(a.nth(0));
            lst.addAll(((MalList)a.nth(1)).getList());
            return (MalVal)new MalList(lst);
        }
    };

    static MalFunction concat = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            if (a.size() == 0) { return new MalList(); }
            List<MalVal> lst = new ArrayList<MalVal>();
            lst.addAll(((MalList)a.nth(0)).value);
            for(Integer i=1; i<a.size(); i++) {
                lst.addAll(((MalList)a.nth(i)).value);
            }
            return (MalVal)new MalList(lst);
        }
    };

    static MalFunction first = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList ml = ((MalList)a.nth(0));
            return ml.size() > 0 ? ml.nth(0) : Nil;
        }
    };

    static MalFunction rest = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            MalList ml = ((MalList)a.nth(0));
            return ml.rest();
        }
    };

    static MalFunction nth = new MalFunction() {
        public MalVal apply(MalList a) throws MalThrowable {
            Integer idx = ((MalInteger)a.nth(1)).getValue();
            if (idx < ((MalList)a.nth(0)).size()) {
                return ((MalList)a.nth(0)).nth(idx);
            } else {
                throw new MalError("nth: index out of range");
            }
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


    // Metadata functions

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


    // Atom functions
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

    



    // types_ns is namespace of type functions
    static Map<String, MalVal> ns = ImmutableMap.<String, MalVal>builder()
        .put("=",         equal_Q)
        .put("throw",     mal_throw)
        .put("nil?",      nil_Q)
        .put("true?",     true_Q)
        .put("false?",    false_Q)
        .put("symbol",    symbol)
        .put("symbol?",   symbol_Q)
        .put("keyword",   keyword)
        .put("keyword?",  keyword_Q)

        .put("pr-str",    pr_str)
        .put("str",       str)
        .put("prn",       prn)
        .put("println",   println)
        .put("readline",  mal_readline)
        .put("read-string", read_string)
        .put("slurp",     slurp)
        .put("<",         lt)
        .put("<=",        lte)
        .put(">",         gt)
        .put(">=",        gte)
        .put("+",         add)
        .put("-",         subtract)
        .put("*",         multiply)
        .put("/",         divide)
        .put("time-ms",   time_ms)

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

        .put("sequential?", sequential_Q)
        .put("cons",      cons)
        .put("concat",    concat)
        .put("nth",       nth)
        .put("first",     first)
        .put("rest",      rest)
        .put("empty?",    empty_Q)
        .put("count",     count)
        .put("conj",      conj)
        .put("apply",     apply)
        .put("map",       map)

        .put("with-meta", with_meta)
        .put("meta",      meta)
        .put("atom",      new_atom)
        .put("atom?",     atom_Q)
        .put("deref",     deref)
        .put("reset!",    reset_BANG)
        .put("swap!",     swap_BANG)
        .build();
}

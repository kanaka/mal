using System;
using System.IO;
using System.Collections.Generic;
using MalVal = Mal.types.MalVal;
using MalConstant = Mal.types.MalConstant;
using MalInt = Mal.types.MalInt;
using MalSymbol = Mal.types.MalSymbol;
using MalString = Mal.types.MalString;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalAtom = Mal.types.MalAtom;
using MalFunc = Mal.types.MalFunc;

namespace Mal {
    public class core {
        static MalConstant Nil = Mal.types.Nil;
        static MalConstant True = Mal.types.True;
        static MalConstant False = Mal.types.False;

        // Errors/Exceptions
        static public MalFunc mal_throw = new MalFunc(
            a => { throw new Mal.types.MalException(a[0]); });

        // Scalar functions
        static MalFunc nil_Q = new MalFunc(
            a => a[0] == Nil ? True : False);

        static MalFunc true_Q = new MalFunc(
            a => a[0] == True ? True : False);

        static MalFunc false_Q = new MalFunc(
            a => a[0] == False ? True : False);

        static MalFunc symbol_Q = new MalFunc(
            a => a[0] is MalSymbol ? True : False);

        static MalFunc keyword = new MalFunc(
            a => {
                if (a[0] is MalString &&
                    ((MalString)a[0]).getValue()[0] == '\u029e') {
                    return a[0];
                } else {
                    return new MalString("\u029e" + ((MalString)a[0]).getValue());
                }
            } );

        static MalFunc keyword_Q = new MalFunc(
            a => {
                if (a[0] is MalString &&
                    ((MalString)a[0]).getValue()[0] == '\u029e') {
                    return True;
                } else {
                    return False;
                }
            } );


        // Number functions
        static MalFunc time_ms = new MalFunc(
            a => new MalInt(DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond));

        // String functions
        static public MalFunc pr_str = new MalFunc(
            a => new MalString(printer._pr_str_args(a, " ", true)) );

        static public MalFunc str = new MalFunc(
            a => new MalString(printer._pr_str_args(a, "", false)) );

        static public MalFunc prn = new MalFunc(
            a => { 
                Console.WriteLine(printer._pr_str_args(a, " ", true));
                return Nil;
            } );

        static public MalFunc println = new MalFunc(
            a => {
                Console.WriteLine(printer._pr_str_args(a, " ", false));
                return Nil;
            } );

        static public MalFunc mal_readline = new MalFunc(
            a => {
                var line = readline.Readline(((MalString)a[0]).getValue());
                if (line == null) { return types.Nil; }
                else {              return new MalString(line); }
            } );

        static public MalFunc read_string = new MalFunc(
            a => reader.read_str(((MalString)a[0]).getValue()));

        static public MalFunc slurp = new MalFunc(
            a => new MalString(File.ReadAllText(
                        ((MalString)a[0]).getValue())));


        // List/Vector functions
        static public MalFunc list_Q = new MalFunc(
            a => a[0].GetType() == typeof(MalList) ? True : False);

        static public MalFunc vector_Q = new MalFunc(
            a => a[0].GetType() == typeof(MalVector) ? True : False);

        // HashMap functions
        static public MalFunc hash_map_Q = new MalFunc(
            a => a[0].GetType() == typeof(MalHashMap) ? True : False);

        static MalFunc contains_Q = new MalFunc(
            a => {
                string key = ((MalString)a[1]).getValue();
                var dict = ((MalHashMap)a[0]).getValue();
                return dict.ContainsKey(key) ? True : False;
            });

        static MalFunc assoc = new MalFunc(
            a => {
                var new_hm = ((MalHashMap)a[0]).copy();
                return new_hm.assoc_BANG((MalList)a.slice(1));
            });

        static MalFunc dissoc = new MalFunc(
            a => {
                var new_hm = ((MalHashMap)a[0]).copy();
                return new_hm.dissoc_BANG((MalList)a.slice(1));
            });

        static MalFunc get = new MalFunc(
            a => {
                string key = ((MalString)a[1]).getValue();
                if (a[0] == Nil) {
                    return Nil;
                } else {
                    var dict = ((MalHashMap)a[0]).getValue();
                    return dict.ContainsKey(key) ? dict[key] : Nil;
                }
            });

        static MalFunc keys = new MalFunc(
            a => {
                var dict = ((MalHashMap)a[0]).getValue();
                MalList key_lst = new MalList();
                foreach (var key in dict.Keys) {
                    key_lst.conj_BANG(new MalString(key));
                }
                return key_lst;
            });

        static MalFunc vals = new MalFunc(
            a => {
                var dict = ((MalHashMap)a[0]).getValue();
                MalList val_lst = new MalList();
                foreach (var val in dict.Values) {
                    val_lst.conj_BANG(val);
                }
                return val_lst;
            });

        // Sequence functions
        static public MalFunc sequential_Q = new MalFunc(
            a => a[0] is MalList ? True : False);

        static MalFunc cons = new MalFunc(
            a => {
                var lst = new List<MalVal>();
                lst.Add(a[0]);
                lst.AddRange(((MalList)a[1]).getValue());
                return (MalVal)new MalList(lst);
            });

        static MalFunc concat = new MalFunc(
            a => {
                if (a.size() == 0) { return new MalList(); }
                var lst = new List<MalVal>();
                lst.AddRange(((MalList)a[0]).getValue());
                for(int i=1; i<a.size(); i++) {
                    lst.AddRange(((MalList)a[i]).getValue());
                }
                return (MalVal)new MalList(lst);
            });

        static MalFunc nth = new MalFunc(
            a => {
                var idx = (int)((MalInt)a[1]).getValue();
                if (idx < ((MalList)a[0]).size()) {
                    return ((MalList)a[0])[idx];
                } else {
                    throw new Mal.types.MalException(
                        "nth: index out of range");
                }
            });

        static MalFunc first = new MalFunc(
            a => ((MalList)a[0])[0]);

        static MalFunc rest = new MalFunc(
            a => ((MalList)a[0]).rest());

        static MalFunc empty_Q = new MalFunc(
            a => ((MalList)a[0]).size() == 0 ? True : False);

        static MalFunc count = new MalFunc(
            a => {
                return (a[0] == Nil)
                    ? new MalInt(0)
                    :new MalInt(((MalList)a[0]).size());
            });

        static MalFunc conj = new MalFunc(
            a => {
                var src_lst = ((MalList)a[0]).getValue();
                var new_lst = new List<MalVal>();
                new_lst.AddRange(src_lst);
                if (a[0] is MalVector) {
                    for(int i=1; i<a.size(); i++) {
                        new_lst.Add(a[i]);
                    }
                    return new MalVector(new_lst);
                } else {
                    for(int i=1; i<a.size(); i++) {
                        new_lst.Insert(0, a[i]);
                    }
                    return new MalList(new_lst);
                }
            });


        // General list related functions
        static MalFunc apply = new MalFunc(
            a => {
                var f = (MalFunc)a[0];
                var lst = new List<MalVal>();
                lst.AddRange(a.slice(1,a.size()-1).getValue());
                lst.AddRange(((MalList)a[a.size()-1]).getValue());
                return f.apply(new MalList(lst));
            });

        static MalFunc map = new MalFunc(
            a => {
                MalFunc f = (MalFunc) a[0];
                var src_lst = ((MalList)a[1]).getValue();
                var new_lst = new List<MalVal>();
                for(int i=0; i<src_lst.Count; i++) {
                    new_lst.Add(f.apply(new MalList(src_lst[i])));
                }
                return new MalList(new_lst);
            });


        // Metadata functions
        static MalFunc meta = new MalFunc(
            a => a[0].getMeta());

        static MalFunc with_meta = new MalFunc(
            a => ((MalVal)a[0]).copy().setMeta(a[1]));


        // Atom functions
        static MalFunc atom_Q = new MalFunc(
            a => a[0] is MalAtom ? True : False);

        static MalFunc deref = new MalFunc(
            a => ((MalAtom)a[0]).getValue());

        static MalFunc reset_BANG = new MalFunc(
            a => ((MalAtom)a[0]).setValue(a[1]));

        static MalFunc swap_BANG = new MalFunc(
            a => {
                MalAtom atm = (MalAtom)a[0];
                MalFunc f = (MalFunc)a[1];
                var new_lst = new List<MalVal>();
                new_lst.Add(atm.getValue());
                new_lst.AddRange(((MalList)a.slice(2)).getValue());
                return atm.setValue(f.apply(new MalList(new_lst)));
            });



        static public Dictionary<string, MalVal> ns =
                  new Dictionary<string, MalVal> {
            {"=",  new MalFunc(
                a => Mal.types._equal_Q(a[0], a[1]) ? True : False)},
            {"throw", mal_throw},
            {"nil?", nil_Q},
            {"true?", true_Q},
            {"false?", false_Q},
            {"symbol", new MalFunc(a => new MalSymbol((MalString)a[0]))},
            {"symbol?", symbol_Q},
            {"keyword", keyword},
            {"keyword?", keyword_Q},

            {"pr-str", pr_str},
            {"str", str},
            {"prn", prn},
            {"println", println},
            {"readline", mal_readline},
            {"read-string", read_string},
            {"slurp", slurp},
            {"<",  new MalFunc(a => (MalInt)a[0] <  (MalInt)a[1])},
            {"<=", new MalFunc(a => (MalInt)a[0] <= (MalInt)a[1])},
            {">",  new MalFunc(a => (MalInt)a[0] >  (MalInt)a[1])},
            {">=", new MalFunc(a => (MalInt)a[0] >= (MalInt)a[1])},
            {"+",  new MalFunc(a => (MalInt)a[0] +  (MalInt)a[1])},
            {"-",  new MalFunc(a => (MalInt)a[0] -  (MalInt)a[1])},
            {"*",  new MalFunc(a => (MalInt)a[0] *  (MalInt)a[1])},
            {"/",  new MalFunc(a => (MalInt)a[0] /  (MalInt)a[1])},
            {"time-ms", time_ms},

            {"list",  new MalFunc(a => new MalList(a.getValue()))},
            {"list?", list_Q},
            {"vector",  new MalFunc(a => new MalVector(a.getValue()))},
            {"vector?", vector_Q},
            {"hash-map",  new MalFunc(a => new MalHashMap(a))},
            {"map?", hash_map_Q},
            {"contains?", contains_Q},
            {"assoc", assoc},
            {"dissoc", dissoc},
            {"get", get},
            {"keys", keys},
            {"vals", vals},

            {"sequential?", sequential_Q},
            {"cons", cons},
            {"concat", concat},
            {"nth", nth},
            {"first", first},
            {"rest",  rest},
            {"empty?", empty_Q},
            {"count", count},
            {"conj", conj},
            {"apply", apply},
            {"map", map},

            {"with-meta", with_meta},
            {"meta", meta},
            {"atom", new MalFunc(a => new MalAtom(a[0]))},
            {"atom?", atom_Q},
            {"deref", deref},
            {"reset!", reset_BANG},
            {"swap!", swap_BANG},
        };
    }
}

using System;
using System.Collections.Generic;
using MalVal = Mal.types.MalVal;
using MalConstant = Mal.types.MalConstant;
using MalInteger = Mal.types.MalInteger;
using MalSymbol = Mal.types.MalSymbol;
using MalString = Mal.types.MalString;
using MalList = Mal.types.MalList;
using MalVector = Mal.types.MalVector;
using MalHashMap = Mal.types.MalHashMap;
using MalFunction = Mal.types.MalFunction;

namespace Mal {
    public class core {
        static MalConstant Nil = Mal.types.Nil;
        static MalConstant True = Mal.types.True;
        static MalConstant False = Mal.types.False;

        // Errors/Exceptions
        static public MalFunction mal_throw = new MalFunction(
            a => { throw new Mal.types.MalException(a[0]); });

        // Scalar functions
        static MalFunction nil_Q = new MalFunction(
            a => a[0] == Nil ? True : False);

        static MalFunction true_Q = new MalFunction(
            a => a[0] == True ? True : False);

        static MalFunction false_Q = new MalFunction(
            a => a[0] == False ? True : False);

        static MalFunction symbol_Q = new MalFunction(
            a => a[0] is MalSymbol ? True : False);


        // String functions
        static public MalFunction pr_str = new MalFunction(
            a => new MalString(printer._pr_str_args(a, " ", true)) );

        static public MalFunction str = new MalFunction(
            a => new MalString(printer._pr_str_args(a, "", false)) );

        static public MalFunction prn = new MalFunction(
            a => { 
                Console.WriteLine(printer._pr_str_args(a, " ", true));
                return Nil;
            } );

        static public MalFunction println = new MalFunction(
            a => {
                Console.WriteLine(printer._pr_str_args(a, " ", false));
                return Nil;
            } );

        // List/Vector functions
        static public MalFunction list_Q = new MalFunction(
            a => a[0].GetType() == typeof(MalList) ? True : False);

        static public MalFunction vector_Q = new MalFunction(
            a => a[0].GetType() == typeof(MalVector) ? True : False);

        // HashMap functions
        static public MalFunction hash_map_Q = new MalFunction(
            a => a[0].GetType() == typeof(MalHashMap) ? True : False);

        static MalFunction contains_Q = new MalFunction(
            a => {
                string key = ((MalString)a[1]).getValue();
                var dict = ((MalHashMap)a[0]).getValue();
                return dict.ContainsKey(key) ? True : False;
            });

        static MalFunction assoc = new MalFunction(
            a => {
                var new_hm = ((MalHashMap)a[0]).copy();
                return new_hm.assoc_BANG((MalList)a.slice(1));
            });

        static MalFunction dissoc = new MalFunction(
            a => {
                var new_hm = ((MalHashMap)a[0]).copy();
                return new_hm.dissoc_BANG((MalList)a.slice(1));
            });

        static MalFunction get = new MalFunction(
            a => {
                string key = ((MalString)a[1]).getValue();
                var dict = ((MalHashMap)a[0]).getValue();
                return dict.ContainsKey(key) ? dict[key] : Nil;
            });

        static MalFunction keys = new MalFunction(
            a => {
                var dict = ((MalHashMap)a[0]).getValue();
                MalList key_lst = new MalList();
                foreach (var key in dict.Keys) {
                    key_lst.conj_BANG(new MalString(key));
                }
                return key_lst;
            });

        static MalFunction vals = new MalFunction(
            a => {
                var dict = ((MalHashMap)a[0]).getValue();
                MalList val_lst = new MalList();
                foreach (var val in dict.Values) {
                    val_lst.conj_BANG(val);
                }
                return val_lst;
            });

        // Sequence functions
        static public MalFunction sequential_Q = new MalFunction(
            a => a[0] is MalList ? True : False);

        static MalFunction cons = new MalFunction(
            a => {
                var lst = new List<MalVal>();
                lst.Add(a[0]);
                lst.AddRange(((MalList)a[1]).getValue());
                return (MalVal)new MalList(lst);
            });

        static MalFunction concat = new MalFunction(
            a => {
                if (a.size() == 0) { return new MalList(); }
                var lst = new List<MalVal>();
                lst.AddRange(((MalList)a[0]).getValue());
                for(int i=1; i<a.size(); i++) {
                    lst.AddRange(((MalList)a[i]).getValue());
                }
                return (MalVal)new MalList(lst);
            });

        static MalFunction nth = new MalFunction(
            a => ((MalList)a[0])[ ((MalInteger)a[1]).getValue() ]);

        static MalFunction first = new MalFunction(
            a => ((MalList)a[0])[0]);

        static MalFunction rest = new MalFunction(
            a => ((MalList)a[0]).rest());

        static MalFunction empty_Q = new MalFunction(
            a => ((MalList)a[0]).size() == 0 ? True : False);

        static MalFunction count = new MalFunction(
            a => new MalInteger(((MalList)a[0]).size()));

        static MalFunction conj = new MalFunction(
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
        static MalFunction apply = new MalFunction(
            a => {
                var f = (MalFunction)a[0];
                var lst = new List<MalVal>();
                lst.AddRange(a.slice(1,a.size()-1).getValue());
                lst.AddRange(((MalList)a[a.size()-1]).getValue());
                return f.apply(new MalList(lst));
            });

        static MalFunction map = new MalFunction(
            a => {
                MalFunction f = (MalFunction) a[0];
                var src_lst = ((MalList)a[1]).getValue();
                var new_lst = new List<MalVal>();
                for(int i=0; i<src_lst.Count; i++) {
                    new_lst.Add(f.apply(new MalList(src_lst[i])));
                }
                return new MalList(new_lst);
            });




        static public Dictionary<string, MalVal> ns =
                  new Dictionary<string, MalVal> {
            {"=",  new MalFunction(
                a => Mal.types._equal_Q(a[0], a[1]) ? True : False)},
            {"throw", mal_throw},
            {"nil?", nil_Q},
            {"true?", true_Q},
            {"false?", false_Q},
            {"symbol?", symbol_Q},
            {"pr-str", pr_str},
            {"str", str},
            {"prn", prn},
            {"println", println},
            {"<",  new MalFunction(a => (MalInteger)a[0] <  (MalInteger)a[1])},
            {"<=", new MalFunction(a => (MalInteger)a[0] <= (MalInteger)a[1])},
            {">",  new MalFunction(a => (MalInteger)a[0] >  (MalInteger)a[1])},
            {">=", new MalFunction(a => (MalInteger)a[0] >= (MalInteger)a[1])},
            {"+",  new MalFunction(a => (MalInteger)a[0] +  (MalInteger)a[1])},
            {"-",  new MalFunction(a => (MalInteger)a[0] -  (MalInteger)a[1])},
            {"*",  new MalFunction(a => (MalInteger)a[0] *  (MalInteger)a[1])},
            {"/",  new MalFunction(a => (MalInteger)a[0] /  (MalInteger)a[1])},

            {"list",  new MalFunction(a => new MalList(a.getValue()))},
            {"list?", list_Q},
            {"vector",  new MalFunction(a => new MalVector(a.getValue()))},
            {"vector?", vector_Q},
            {"hash-map",  new MalFunction(a => new MalHashMap(a))},
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
        };
    }
}

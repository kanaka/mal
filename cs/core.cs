using System;
using System.Collections.Generic;
using MalVal = Mal.types.MalVal;
using MalConstant = Mal.types.MalConstant;
using MalInteger = Mal.types.MalInteger;
using MalString = Mal.types.MalString;
using MalList = Mal.types.MalList;
using MalFunction = Mal.types.MalFunction;

namespace Mal {
    public class core {
        static MalConstant Nil = Mal.types.Nil;
        static MalConstant True = Mal.types.True;
        static MalConstant False = Mal.types.False;

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

        // Sequence functions
        static public MalFunction list_Q = new MalFunction(
            a => a[0].GetType() == typeof(MalList) ? True : False);



        static public Dictionary<string, MalVal> ns =
                  new Dictionary<string, MalVal> {
            {"=",  new MalFunction(
                a => Mal.types._equal_Q(a[0], a[1]) ? True : False)},
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
            {"first", new MalFunction(a => ((MalList)a[0]).nth(0))},
            {"rest",  new MalFunction(a => ((MalList)a[0]).rest())},
            {"count", new MalFunction(
                a => new MalInteger(((MalList)a[0]).size()))},
            {"empty?", new MalFunction(
                a => ((MalList)a[0]).size() == 0 ? True : False)},
        };
    }
}

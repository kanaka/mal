using System;
using System.Collections.Generic;
using System.Linq;

namespace mal
{
    class core
    {
        public static Dictionary<string, MalFunction> ns = new Dictionary<string, MalFunction>()
        {
            {"list", new MalFunction((IList<MalType> args) => new MalList(args)) },
            {"list?", new MalFunction((IList<MalType> args) =>
                ((args[0] is MalList) && ((MalList)args[0]).isList())? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE)},
            {"empty?", new MalFunction((IList<MalType> args) =>
                ((args[0] is MalList) && ((MalList)args[0]).items.Count == 0)? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE)},
            {"count", new MalFunction((IList<MalType> args) => {
                if (args.Count == 0 || args[0] == MalNil.MAL_NIL) return new MalInteger(0);
                else return new MalInteger( ((MalList)args[0]).items.Count );
            })},
            {"+", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value + ((MalInteger)args[1]).value))},
            {"-", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value - ((MalInteger)args[1]).value))},
            {"*", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value * ((MalInteger)args[1]).value))},
            {"/", new MalFunction((IList<MalType> args) => new MalInteger(((MalInteger)args[0]).value / ((MalInteger)args[1]).value))},

            {"<", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value < ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {"<=", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value <= ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {">", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value > ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},
            {">=", new MalFunction((IList<MalType> args) => (((MalInteger)args[0]).value >= ((MalInteger)args[1]).value) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE )},

            {"=", new MalFunction((IList<MalType> args) => (args[0].Equals(args[1])) ? MalBoolean.MAL_TRUE : MalBoolean.MAL_FALSE)},

            // string functions
            {"pr-str",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, true)).ToList();
                    return new MalString(string.Join(" ", printed));
                })},
            {"str",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, false)).ToList();
                    return new MalString(string.Join("", printed));
                })},
            {"prn",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, true)).ToList();
                    Console.WriteLine(string.Join(" ", printed));
                    return MalNil.MAL_NIL;
                })},
            {"println",
            new MalFunction((IList<MalType> args) => {
                    List<string> printed = args.Select(it => printer.pr_str(it, false)).ToList();
                    Console.WriteLine(string.Join(" ", printed));
                    return MalNil.MAL_NIL;
                })},
        };
    }
}